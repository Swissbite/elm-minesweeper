{-
   This file is part of Elm Minesweeper.

   Elm Minesweeper is free software: you can redistribute it and/or modify it under
   the terms of the GNU Affero General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later version.

   Elm Minesweeper is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
   PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License along with
   Elm Minesweeper. If not, see <https://www.gnu.org/licenses/>.

-}


module Game.Game exposing (decodeStoredFinishedGameHistory, initModel, subscriptions, update, view)

{-| Game module for rendering the complete game, as long as the currentView in the model is set to Game.
Exposes the basic update / view / subscription functions, so that Main.elm can use them.
-}

import Array
import Browser.Events
import Colors
import Element exposing (DeviceClass(..), Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import Game.Internal exposing (..)
import Grid
import Html.Attributes as HA
import Json.Decode as Decode
import List
import Random exposing (Generator)
import Set exposing (Set)
import Styles exposing (..)
import Time
import Types exposing (..)


initModel : GameModel
initModel =
    { gameBoardStatus = NoGame PreSelect
    , gameInteractionMode = Reveal
    , gameRunningTimes = []
    , gamePauseResumeState = Paused
    , lastClockTick = Time.millisToPosix 0
    }



----- Subscription -----


decodeStoredFinishedGameHistory : String -> List FinishedGameHistoryEntry
decodeStoredFinishedGameHistory string =
    Decode.decodeString deocdeFinishedGameHistory string
        |> Result.withDefault []


subscriptions : Model -> Sub GameMsg
subscriptions m =
    case ( m.currentView, m.game.gameBoardStatus ) of
        ( Game, RunningGame _ ) ->
            Sub.batch
                [ Time.every 200 (\posix -> ClockTick posix)
                , Browser.Events.onKeyDown keyPressedDecoder
                ]

        _ ->
            Sub.none


keyPressedDecoder : Decode.Decoder GameMsg
keyPressedDecoder =
    Decode.map toKeyEventMsg (Decode.field "key" Decode.string)


toKeyEventMsg : String -> GameMsg
toKeyEventMsg eventKeyString =
    case eventKeyString of
        string_ ->
            case String.uncons string_ of
                Just ( char, "" ) ->
                    case Char.toLower char of
                        't' ->
                            ToogleGameCellInteractionMode

                        'p' ->
                            ToogleGamePause

                        _ ->
                            NoUpdate

                _ ->
                    NoUpdate



----- UPDATE -----


update : GameMsg -> Model -> ( Model, Cmd GameMsg )
update gameMsg model =
    case gameMsg of
        NoUpdate ->
            ( model, Cmd.none )

        GoToStartPage ->
            let
                gameModel =
                    model.game
            in
            ( { model | game = { gameModel | gameBoardStatus = NoGame PreSelect } }, Cmd.none )

        ClickedOnInitGameCell initGame coords ->
            ( model, generatePlayGameGrid initGame coords |> Random.generate StartGame )

        StartGame playGrid ->
            let
                gameModel =
                    model.game
            in
            ( { model | game = { gameModel | gameBoardStatus = RunningGame playGrid, gameRunningTimes = [], gamePauseResumeState = Resumed 1 } }, Cmd.none )

        CreateNewGame playgroundDefinition ->
            let
                gameModel =
                    model.game
            in
            ( { model | game = { gameModel | gameBoardStatus = WaitOnStart <| createInitGameGrid playgroundDefinition, gameInteractionMode = Reveal } }, Cmd.none )

        ClickOnGameCell coords ->
            updateModelByClickOnGameCell coords model

        ToogleGameCellInteractionMode ->
            let
                gameModel =
                    model.game

                nextMode =
                    case gameModel.gameInteractionMode of
                        Reveal ->
                            Flag

                        Flag ->
                            Reveal
            in
            ( { model | game = { gameModel | gameInteractionMode = nextMode } }, Cmd.none )

        ToogleGamePause ->
            ( togglePause model, Cmd.none )

        ClockTick posix ->
            updateTimePlayGame model posix

        NavigationEvent to ->
            case ( model.currentView, to ) of
                ( Game, Game ) ->
                    ( model, Cmd.none )

                ( Game, _ ) ->
                    ( togglePause model, Cmd.none )

                ( _, Game ) ->
                    ( togglePause model, Cmd.none )

                ( _, _ ) ->
                    ( model, Cmd.none )


togglePause : Model -> Model
togglePause model =
    let
        gameModel =
            model.game
    in
    case ( gameModel.gameBoardStatus, gameModel.gamePauseResumeState ) of
        ( RunningGame _, Paused ) ->
            { model | game = { gameModel | gamePauseResumeState = Resumed (List.length gameModel.gameRunningTimes + 1) } }

        ( RunningGame _, Resumed _ ) ->
            { model | game = { gameModel | gamePauseResumeState = Paused } }

        _ ->
            model


updateTimePlayGame : Model -> Time.Posix -> ( Model, Cmd GameMsg )
updateTimePlayGame model time =
    case ( model.game.gameBoardStatus, model.game.gamePauseResumeState ) of
        ( RunningGame _, Resumed timesResume ) ->
            let
                gameModel =
                    model.game

                shouldReplaceHead =
                    List.length gameModel.gameRunningTimes == timesResume

                newList =
                    if shouldReplaceHead then
                        case gameModel.gameRunningTimes of
                            [] ->
                                [ ( time, time ) ]

                            ( start, _ ) :: xs ->
                                ( start, time ) :: xs

                    else
                        ( time, time ) :: model.game.gameRunningTimes
            in
            ( { model | game = { gameModel | gameRunningTimes = newList, lastClockTick = time } }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateModelByClickOnGameCell : Coordinate -> Model -> ( Model, Cmd GameMsg )
updateModelByClickOnGameCell coords model =
    case ( model.game.gamePauseResumeState, model.game.gameBoardStatus ) of
        ( Resumed _, RunningGame playGrid ) ->
            let
                gameModel =
                    model.game

                updatedPlayGrid : PlayGameGrid
                updatedPlayGrid =
                    case gameModel.gameInteractionMode of
                        Reveal ->
                            openCell coords playGrid

                        Flag ->
                            flagCell coords playGrid

                aMineIsExploded =
                    isAMineExploded updatedPlayGrid

                allFieldsRevealed =
                    areAllNoMineFieldsRevealed updatedPlayGrid

                nextGameBoardStatus =
                    if aMineIsExploded then
                        calculateElapsedTimeMillis gameModel.gameRunningTimes |> FinishedGame updatedPlayGrid Lost

                    else if allFieldsRevealed then
                        calculateElapsedTimeMillis gameModel.gameRunningTimes |> FinishedGame updatedPlayGrid Won

                    else
                        RunningGame updatedPlayGrid

                nextHistoryList =
                    case nextGameBoardStatus of
                        FinishedGame grid result time ->
                            { grid = grid
                            , result = result
                            , duration = time
                            , playFinish = gameModel.lastClockTick
                            }
                                :: model.playedGameHistory

                        _ ->
                            model.playedGameHistory
            in
            ( { model | game = { gameModel | gameBoardStatus = nextGameBoardStatus }, playedGameHistory = nextHistoryList }, saveFinishedGameHistory nextHistoryList )

        _ ->
            ( model, Cmd.none )



----- VIEW for Game -----


view : Model -> Element GameMsg
view model =
    let
        boardConfigFromDefinition : PlayGroundDefinition -> BoardViewConfig
        boardConfigFromDefinition definition =
            boardViewConfig model definition.cols definition.rows

        boardConfigFromInitGrid : InitGameData -> BoardViewConfig
        boardConfigFromInitGrid initGrid =
            boardConfigFromDefinition
                { cols = Grid.width initGrid.grid
                , rows = Grid.height initGrid.grid
                , mines = initGrid.mines
                }

        boardConfigFromPlayGrid : PlayGameGrid -> BoardViewConfig
        boardConfigFromPlayGrid playGrid =
            boardConfigFromDefinition (playGameGridToPlaygroundDefinition playGrid)

        gameGridElement : BoardViewConfig -> PauseResumeState -> PlayGameGrid -> Element GameMsg
        gameGridElement boardConfig pauseResumeState playGrid =
            case pauseResumeState of
                Paused ->
                    Lazy.lazy2 pausedGameView boardConfig playGrid

                Resumed _ ->
                    Lazy.lazy2 runningGameView boardConfig playGrid
    in
    case model.game.gameBoardStatus of
        NoGame _ ->
            gameSelectionView model

        WaitOnStart initGameGrid ->
            let
                boardConfig =
                    boardConfigFromInitGrid initGameGrid
            in
            gameScreenLayout model boardConfig <|
                Lazy.lazy2 initGameGridView boardConfig initGameGrid

        RunningGame playGrid ->
            let
                boardConfig =
                    boardConfigFromPlayGrid playGrid
            in
            gameScreenLayout model boardConfig <|
                gameGridElement boardConfig model.game.gamePauseResumeState playGrid

        FinishedGame playGrid finishedStatus _ ->
            let
                boardConfig =
                    boardConfigFromPlayGrid playGrid
            in
            gameScreenLayout model boardConfig <|
                Lazy.lazy3 finishedGameView boardConfig playGrid finishedStatus


smallPlayground : PlayGroundDefinition
smallPlayground =
    { cols = 8
    , rows = 8
    , mines = 10
    }


mediumPlayground : PlayGroundDefinition
mediumPlayground =
    { cols = 16
    , rows = 16
    , mines = 40
    }


advancePlayground : PlayGroundDefinition
advancePlayground =
    { cols = 30
    , rows = 16
    , mines = 99
    }


xxlPlayground : PlayGroundDefinition
xxlPlayground =
    { cols = 30
    , rows = 30
    , mines = 200
    }


type alias BoardViewConfig =
    { cellSize : Int
    , cols : Int
    , rows : Int
    , isMobile : Bool
    }


boardViewConfig : Model -> Int -> Int -> BoardViewConfig
boardViewConfig model cols rows =
    { cellSize = Styles.cellPixelSize model.device { cols = cols, rows = rows }
    , cols = cols
    , rows = rows
    , isMobile = model.device.class == Phone || model.device.class == Tablet
    }


gameSelectionView : Model -> Element GameMsg
gameSelectionView model =
    let
        isPhone =
            model.device.class == Phone

        optionView : ( String, PlayGroundDefinition ) -> Element GameMsg
        optionView ( title, definition ) =
            Styles.styledGameSelectionButton
                { onPress = Just (CreateNewGame definition)
                , title = title
                , subtitle =
                    String.fromInt definition.cols
                        ++ " x "
                        ++ String.fromInt definition.rows
                        ++ " • "
                        ++ String.fromInt definition.mines
                        ++ " mines"
                , isPhone = isPhone
                }

        options =
            [ ( "Small", smallPlayground )
            , ( "Medium", mediumPlayground )
            , ( "Advanced", advancePlayground )
            , ( "XXL", xxlPlayground )
            ]
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.padding 16
        , Element.spacing 16
        ]
        [ Element.column [ Element.width Element.fill, Element.spacing 8 ]
            [ Element.el [ Font.bold, Font.size 28 ] <| Element.text "Choose a board"
            , Element.paragraph [ Font.color Colors.caputMortuum ]
                [ Element.text "Mobile keeps touch-friendly cells and lets larger boards scroll when needed." ]
            ]
        , (if isPhone then
            Element.column [ Element.width Element.fill, Element.spacing 12 ]

           else
            Element.wrappedRow [ Element.width Element.fill, Element.spacing 12 ]
          )
            (List.map optionView options)
        ]


gameScreenLayout : Model -> BoardViewConfig -> Element GameMsg -> Element GameMsg
gameScreenLayout model boardConfig boardElement =
    if boardConfig.isMobile then
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding 12
            , Element.spacing 12
            ]
            [ mobileStatusBarElement model
            , Element.el [ Element.width Element.fill, Element.height Element.fill ] <| boardViewport boardConfig boardElement
            , mobileActionBarElement model
            ]

    else
        Element.row
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding 20
            , Element.spacing 20
            ]
            [ Element.el [ Element.width <| Element.fillPortion 5, Element.height Element.fill ] <| boardViewport boardConfig boardElement
            , sidebarElement model
            ]


boardViewport : BoardViewConfig -> Element GameMsg -> Element GameMsg
boardViewport boardConfig boardElement =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color Colors.white
        , Border.color Colors.cellBorderColor
        , Border.width 1
        , Border.rounded 16
        , Element.padding 8
        , Element.htmlAttribute <| HA.style "overflow" "auto"
        , Element.htmlAttribute <| HA.style "overscroll-behavior" "contain"
        , Element.htmlAttribute <| HA.style "-webkit-overflow-scrolling" "touch"
        , Element.htmlAttribute <| HA.style "touch-action" "pan-x pan-y pinch-zoom"
        ]
    <|
        Element.el
            [ if boardConfig.isMobile then
                Element.alignLeft

              else
                Element.centerX
            , Element.alignTop
            ]
            boardElement


mobilePauseOverlayFontSize : Int
mobilePauseOverlayFontSize =
    48


desktopPauseOverlayFontSize : Int
desktopPauseOverlayFontSize =
    80


mobilePauseButtonFontSize : Int
mobilePauseButtonFontSize =
    28


desktopPauseButtonFontSize : Int
desktopPauseButtonFontSize =
    42


gameInformationElements : Model -> List (Element GameMsg)
gameInformationElements model =
    case getRunningGameStats model.game of
        Nothing ->
            []

        Just data ->
            [ Styles.pillBadge <| String.concat [ Styles.icons.stopWatch, " ", millisToString data.elapsedTime ]
            , Styles.pillBadge <| String.concat [ String.fromChar Styles.icons.untouchedBomb, " ", String.fromInt data.mines ]
            , Styles.pillBadge <| String.concat [ String.fromChar Styles.icons.markerFlag, " ", String.fromInt data.flags ]
            ]


modeSelectorElements : Model -> List (Element GameMsg)
modeSelectorElements model =
    case model.game.gameBoardStatus of
        RunningGame _ ->
            [ Element.row
                [ Element.spacing 8
                , Background.color Colors.openedCellGray
                , Border.rounded Styles.pillBorderRadius
                , Element.paddingXY 10 6
                ]
                [ Element.el [ Font.bold, Element.centerY ] <|
                    Element.text <|
                        case model.game.gameInteractionMode of
                            Reveal ->
                                "Mode: Reveal"

                            Flag ->
                                "Mode: Flag"
                , Lazy.lazy mineToggleElement model.game.gameInteractionMode
                ]
            ]

        _ ->
            []


giveUpElements : Model -> List (Element GameMsg)
giveUpElements model =
    case model.game.gameBoardStatus of
        FinishedGame _ _ _ ->
            []

        RunningGame _ ->
            [ Input.button [ Background.color Colors.black, Border.solid, Element.paddingXY 12 10, Border.rounded 10, Font.color Colors.gold ]
                { onPress = Just GoToStartPage
                , label = Element.text "Give up 💀"
                }
            ]

        WaitOnStart _ ->
            [ Input.button [ Background.color Colors.black, Border.solid, Element.paddingXY 12 10, Border.rounded 10, Font.color Colors.gold ]
                { onPress = Just GoToStartPage
                , label = Element.text "Cancel ❌"
                }
            ]

        _ ->
            []


pauseToggleElements : Model -> List (Element GameMsg)
pauseToggleElements model =
    let
        buttonAttributes =
            [ Border.solid
            , Element.paddingXY 10 6
            , Border.rounded 10
            , Font.size <|
                if model.device.class == Phone then
                    mobilePauseButtonFontSize

                else
                    desktopPauseButtonFontSize
            ]
    in
    case ( model.game.gameBoardStatus, model.game.gamePauseResumeState ) of
        ( RunningGame _, Paused ) ->
            [ Input.button buttonAttributes
                { onPress = Just ToogleGamePause
                , label = Element.text Styles.icons.resume
                }
            ]

        ( RunningGame _, Resumed _ ) ->
            [ Input.button buttonAttributes
                { onPress = Just ToogleGamePause
                , label = Element.text Styles.icons.pause
                }
            ]

        _ ->
            []


gameActionElements : Model -> List (Element GameMsg)
gameActionElements model =
    modeSelectorElements model ++ giveUpElements model ++ pauseToggleElements model


gameFinishedElements : Model -> List (Element GameMsg)
gameFinishedElements model =
    case model.game.gameBoardStatus of
        FinishedGame playGameGrid gameResult _ ->
            [ Styles.pillBadge <|
                case gameResult of
                    Won ->
                        "You won!"

                    Lost ->
                        "You lost!"
            , Input.button [ Background.color Colors.asparagus, Border.solid, Element.paddingXY 12 10, Border.rounded 10 ]
                { onPress = Just (CreateNewGame <| playGameGridToPlaygroundDefinition playGameGrid)
                , label = Element.text "Start new game"
                }
            , Input.button [ Background.color Colors.saffron, Border.solid, Element.paddingXY 12 10, Border.rounded 10 ]
                { onPress = Just GoToStartPage
                , label = Element.text "Back to overview"
                }
            ]

        _ ->
            []


mobileStatusBarElement : Model -> Element GameMsg
mobileStatusBarElement model =
    wrapIfNotEmpty (gameInformationElements model)


mobileActionBarElement : Model -> Element GameMsg
mobileActionBarElement model =
    let
        actionElements =
            gameActionElements model ++ gameFinishedElements model
    in
    wrapIfNotEmpty actionElements


wrapIfNotEmpty : List (Element msg) -> Element msg
wrapIfNotEmpty elements =
    if List.isEmpty elements then
        Element.none

    else
        Element.wrappedRow
            [ Element.width Element.fill
            , Element.spacing 8
            ]
            elements


sidebarElement : Model -> Element GameMsg
sidebarElement model =
    Element.column
        [ Element.width <| Element.fillPortion 2
        , Element.alignTop
        , Element.spacing 12
        ]
        (gameInformationElements model
            ++ gameActionElements model
            ++ gameFinishedElements model
            ++ [ Element.column [ Font.bold ]
                    [ Element.text "Shortcuts:"
                    , Element.text "T: Toggle Selector"
                    , Element.text "P: Pause/Resume"
                    ]
               ]
        )


styledToggleElement : Bool -> Element GameMsg
styledToggleElement =
    Styles.toggleCheckboxWidget
        { offColor = Colors.lightGrey
        , onColor = Colors.green
        , sliderColor = Colors.white
        , toggleWidth = 60
        , toggleHeight = 28
        , onSymbol = Just Styles.icons.untouchedBomb
        , offSymbol = Just Styles.icons.markerFlag
        , tooltip = Just "Shortcut: T"
        }


mineToggleElement : CellClickMode -> Element GameMsg
mineToggleElement gameInteractionMode =
    Element.el [ Element.centerX, Element.centerY, Element.paddingXY 0 10 ] <|
        Input.checkbox [ Element.centerX, Element.centerY ] <|
            { onChange = always ToogleGameCellInteractionMode
            , label = Input.labelHidden "Activate/deactivate mine flag mode"
            , checked =
                case gameInteractionMode of
                    Reveal ->
                        False

                    Flag ->
                        True
            , icon = styledToggleElement
            }


initGameGridView : BoardViewConfig -> InitGameData -> Element GameMsg
initGameGridView boardConfig initGameGrid =
    let
        indexedFn =
            initGameCellToElement boardConfig.cellSize initGameGrid

        gridWithElements =
            Grid.indexedMap indexedFn initGameGrid.grid

        gridAsListOfRows =
            Grid.rows gridWithElements |> Array.map Array.toList |> Array.map (\l -> Element.row [] l) |> Array.toList
    in
    Element.column [ Element.alignTop ] gridAsListOfRows


initGameCellToElement : Int -> InitGameData -> (Int -> Int -> InitGameCell -> Element GameMsg)
initGameCellToElement cellSize initGameGrid =
    \x y _ ->
        let
            coords =
                Coordinate x y
        in
        Element.el (Styles.untouchedCellStyle cellSize ++ [ Events.onClick <| ClickedOnInitGameCell initGameGrid coords ]) <| Element.text ""


gameView : PlayGameGrid -> (Grid.Grid GameCell -> Grid.Grid (Element GameMsg)) -> Element GameMsg
gameView playGameGrid gridGameToGridElementMapper =
    playGameGrid
        |> gridGameToGridElementMapper
        |> Grid.rows
        |> Array.map Array.toList
        |> Array.map (\l -> Element.row [] l)
        |> Array.toList
        |> Element.column [ Element.alignTop ]


runningGameView : BoardViewConfig -> PlayGameGrid -> Element GameMsg
runningGameView boardConfig playGameGrid =
    gameView playGameGrid <| Grid.indexedMap (runningGameCellToElement boardConfig.cellSize)


pausedGameView : BoardViewConfig -> PlayGameGrid -> Element GameMsg
pausedGameView boardConfig playGameGrid =
    Element.el
        [ Element.width Element.fill
        , Element.inFront <|
            Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Background.color <| Element.rgba255 255 0 0 0.5
                ]
            <|
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    , Font.extraBold
                    , Font.size <|
                        if boardConfig.isMobile then
                            mobilePauseOverlayFontSize

                        else
                            desktopPauseOverlayFontSize
                    ]
                <|
                    Element.text "Paused"
        ]
    <|
        gameView playGameGrid <|
            Grid.map (\_ -> Element.el (Styles.openedCellStyle boardConfig.cellSize) Element.none)


runningGameCellToElement : Int -> Int -> Int -> GameCell -> Element GameMsg
runningGameCellToElement cellSize x y cell =
    case cell of
        GameCell _ Flagged ->
            Element.el (Styles.untouchedCellStyle cellSize ++ [ Events.onClick <| ClickOnGameCell { x = x, y = y } ]) <| Element.el [ Element.centerX, Element.centerY ] <| Element.text <| String.fromChar Styles.icons.markerFlag

        GameCell _ Untouched ->
            Element.el (Styles.untouchedCellStyle cellSize ++ [ Events.onClick <| ClickOnGameCell { x = x, y = y } ]) Element.none

        GameCell EmptyCell Opened ->
            Element.el (Styles.openedCellStyle cellSize) Element.none

        GameCell MineCell Opened ->
            Element.el (Styles.openedCellStyle cellSize) <| Element.el [ Element.centerX, Element.centerY ] <| Element.text <| String.fromChar Styles.icons.exploded

        GameCell (MineNeighbourCell neighbours) Opened ->
            Element.el (Styles.openedMineNeighbourCellStyle cellSize neighbours ++ [ Events.onClick <| ClickOnGameCell { x = x, y = y } ]) <| Element.el [ Element.centerX, Element.centerY ] <| Element.text (String.fromInt neighbours)


finishedGameView : BoardViewConfig -> PlayGameGrid -> GameResult -> Element GameMsg
finishedGameView boardConfig playGameGrid _ =
    Element.column [ Element.alignTop ]
        [ finishedGridToView boardConfig playGameGrid
        ]


finishedGridToView : BoardViewConfig -> PlayGameGrid -> Element GameMsg
finishedGridToView boardConfig playGameGrid =
    playGameGrid
        |> Grid.map (finishedGameCellToElement boardConfig.cellSize)
        |> Grid.rows
        |> Array.map Array.toList
        |> Array.map (\l -> Element.row [] l)
        |> Array.toList
        |> Element.column [ Element.alignTop ]


finishedGameCellToElement : Int -> GameCell -> Element GameMsg
finishedGameCellToElement cellSize cell =
    case cell of
        GameCell MineCell Opened ->
            Element.el (Styles.openedCellStyle cellSize) <| Element.el [ Element.centerX, Element.centerY ] <| Element.text <| String.fromChar Styles.icons.exploded

        GameCell MineCell _ ->
            Element.el (Styles.openedCellStyle cellSize) <| Element.el [ Element.centerX, Element.centerY ] <| Element.text <| String.fromChar Styles.icons.untouchedBomb

        GameCell (MineNeighbourCell neighbours) Opened ->
            Element.el (Styles.openedMineNeighbourCellStyle cellSize neighbours) <| Element.el [ Element.centerX, Element.centerY ] <| Element.text (String.fromInt neighbours)

        GameCell EmptyCell Opened ->
            Element.el (Styles.openedCellStyle cellSize) Element.none

        GameCell _ Flagged ->
            Element.el (Styles.untouchedCellStyle cellSize) <| Element.el [ Element.centerX, Element.centerY ] <| Element.text <| String.fromChar Styles.icons.markerFlag

        _ ->
            Element.el (Styles.untouchedCellStyle cellSize) Element.none



--- HELPER ---


createInitGameGrid : PlayGroundDefinition -> InitGameData
createInitGameGrid definition =
    let
        sanitized =
            sanitizePlaygroundDefinition definition
    in
    { grid = Grid.repeat sanitized.cols sanitized.rows InitGameCell
    , mines = sanitized.mines
    }


playGameGridToPlaygroundDefinition : PlayGameGrid -> PlayGroundDefinition
playGameGridToPlaygroundDefinition grid =
    let
        foldLFn : GameCell -> Int -> Int
        foldLFn cell count =
            case cell of
                GameCell MineCell _ ->
                    count + 1

                _ ->
                    count
    in
    { cols = Grid.width grid
    , rows = Grid.height grid
    , mines = Grid.foldl foldLFn 0 grid
    }


sanitizePlaygroundDefinition : PlayGroundDefinition -> PlayGroundDefinition
sanitizePlaygroundDefinition definition =
    let
        cols =
            max 4 definition.cols

        rows =
            max 4 definition.rows

        mines =
            definition.mines
                |> max 1
                |> min (cols * rows - 1)
    in
    { cols = cols
    , rows = rows
    , mines = mines
    }


{-| Takes the initGame and the clicked coordinates and generates a new play game grid.
The clicked cell is opened and the surrounding cells - if the clicked cell is an empty cell - are opened as well.
-}
generatePlayGameGrid : InitGameData -> Coordinate -> Generator PlayGameGrid
generatePlayGameGrid initGameGrid coords =
    let
        initialPossibilities : List Int
        initialPossibilities =
            generateListOfPossibleIndices initGameGrid.grid coords

        gameGridWidth =
            Grid.width initGameGrid.grid

        gameGridHeight =
            Grid.height initGameGrid.grid

        minesIdxGenerator : Generator (Set Int)
        minesIdxGenerator =
            minesIndexGenerator initGameGrid.mines initialPossibilities emptySetGenerator

        minesIdxAsCoordinates : Set Int -> List Coordinate
        minesIdxAsCoordinates indices =
            Set.toList indices
                |> List.map (\i -> { x = modBy gameGridWidth i, y = i // gameGridWidth })

        minesIdxAsCoordinatesGenerator : Generator (Set Int) -> Generator (List Coordinate)
        minesIdxAsCoordinatesGenerator =
            Random.andThen (\set -> minesIdxAsCoordinates set |> Random.constant)
    in
    minesIdxAsCoordinatesGenerator minesIdxGenerator
        |> Random.andThen
            (\coordinates ->
                List.map coordinateToPair coordinates
                    |> createPlayGameGrid gameGridWidth gameGridHeight
                    |> openCell coords
                    |> Random.constant
            )


coordinateToPair : Coordinate -> ( Int, Int )
coordinateToPair coords =
    ( coords.x, coords.y )


createPlayGameGrid : Int -> Int -> List ( Int, Int ) -> PlayGameGrid
createPlayGameGrid width height mineCoordinates =
    let
        grid =
            Grid.repeat width height <| GameCell EmptyCell Untouched

        placeMines : PlayGameGrid -> List ( Int, Int ) -> PlayGameGrid
        placeMines gameGrid coordinates =
            List.foldl
                (\coordinate g ->
                    case Grid.get coordinate g of
                        Nothing ->
                            g

                        Just _ ->
                            Grid.set coordinate (GameCell MineCell Untouched) g
                )
                gameGrid
                coordinates

        countNeighbourMines : ( Int, Int ) -> PlayGameGrid -> Int
        countNeighbourMines ( x, y ) gameGrid =
            [ Grid.get ( x - 1, y - 1 )
            , Grid.get ( x - 1, y )
            , Grid.get ( x - 1, y + 1 )
            , Grid.get ( x, y - 1 )
            , Grid.get ( x, y + 1 )
            , Grid.get ( x + 1, y - 1 )
            , Grid.get ( x + 1, y )
            , Grid.get ( x + 1, y + 1 )
            ]
                |> List.map (\fn -> fn gameGrid)
                |> List.foldl
                    (\cell count ->
                        case cell of
                            Just (GameCell MineCell _) ->
                                count + 1

                            _ ->
                                count
                    )
                    0

        indexedMapFn : PlayGameGrid -> Int -> Int -> GameCell -> GameCell
        indexedMapFn gameGrid x y gameCell =
            case gameCell of
                GameCell MineCell _ ->
                    gameCell

                _ ->
                    case countNeighbourMines ( x, y ) gameGrid of
                        0 ->
                            gameCell

                        other ->
                            GameCell (MineNeighbourCell other) Untouched
    in
    placeMines grid mineCoordinates
        |> (\minedGrid -> Grid.indexedMap (indexedMapFn minedGrid) minedGrid)


flagCell : Coordinate -> PlayGameGrid -> PlayGameGrid
flagCell coords playGrid =
    coordinateToPair coords
        |> (\c ->
                Grid.get c playGrid
                    |> (\cell ->
                            case cell of
                                Just (GameCell gameCell Flagged) ->
                                    Grid.set c (GameCell gameCell Untouched) playGrid

                                Just (GameCell gameCell Untouched) ->
                                    Grid.set c (GameCell gameCell Flagged) playGrid

                                Just (GameCell (MineNeighbourCell neighbours) Opened) ->
                                    if neighbours == calculateFlaggedCellsAroundCoordinate coords playGrid then
                                        openSurroundingCells coords playGrid

                                    else
                                        playGrid

                                _ ->
                                    playGrid
                       )
           )


openCell : Coordinate -> PlayGameGrid -> PlayGameGrid
openCell coords playGrid =
    let
        coordinateAsPair =
            coordinateToPair coords

        cell =
            Grid.get coordinateAsPair playGrid
    in
    case cell of
        Nothing ->
            playGrid

        Just (GameCell (MineNeighbourCell neighbours) Opened) ->
            if neighbours == calculateFlaggedCellsAroundCoordinate coords playGrid then
                openSurroundingCells coords playGrid

            else
                playGrid

        Just (GameCell cellType cellStatus) ->
            case ( cellType, cellStatus ) of
                ( _, Opened ) ->
                    playGrid

                ( _, Flagged ) ->
                    playGrid

                ( MineNeighbourCell neighbours, _ ) ->
                    Grid.set coordinateAsPair (GameCell (MineNeighbourCell neighbours) Opened) playGrid

                ( EmptyCell, _ ) ->
                    Grid.set coordinateAsPair (GameCell EmptyCell Opened) playGrid
                        |> (\nextGrid ->
                                calculateNeighbourCoordinates coords
                                    |> (\surroundingCoordinatesAsPair -> List.foldl (\coordinate grid -> openCell coordinate grid) nextGrid surroundingCoordinatesAsPair)
                           )

                ( MineCell, _ ) ->
                    Grid.set coordinateAsPair (GameCell MineCell Opened) playGrid


openSurroundingCells : Coordinate -> PlayGameGrid -> PlayGameGrid
openSurroundingCells coordinate playGrid =
    let
        mapCoordinateToTupleCoordinateAndMaybeGameCell : Coordinate -> ( Coordinate, Maybe GameCell )
        mapCoordinateToTupleCoordinateAndMaybeGameCell neighbourCoordinate =
            ( neighbourCoordinate, Grid.get (coordinateToPair neighbourCoordinate) playGrid )

        foldGridOpenUntouchedCellsToGrid : ( Coordinate, Maybe GameCell ) -> PlayGameGrid -> PlayGameGrid
        foldGridOpenUntouchedCellsToGrid ( coordinateToCheck, maybeCell ) grid =
            case maybeCell of
                Just (GameCell _ Untouched) ->
                    openCell coordinateToCheck grid

                _ ->
                    grid
    in
    calculateNeighbourCoordinates coordinate
        |> List.map mapCoordinateToTupleCoordinateAndMaybeGameCell
        |> List.foldl foldGridOpenUntouchedCellsToGrid playGrid


calculateFlaggedCellsAroundCoordinate : Coordinate -> PlayGameGrid -> Int
calculateFlaggedCellsAroundCoordinate coords grid =
    calculateNeighbourCoordinates coords
        |> List.map coordinateToPair
        |> List.map (\coordinateAsPair -> Grid.get coordinateAsPair grid)
        |> List.map
            (\maybeCell ->
                case maybeCell of
                    Just (GameCell _ Flagged) ->
                        1

                    _ ->
                        0
            )
        |> List.sum


calculateNeighbourCoordinates : Coordinate -> List Coordinate
calculateNeighbourCoordinates coords =
    [ { x = coords.x - 1, y = coords.y - 1 }
    , { x = coords.x - 1, y = coords.y }
    , { x = coords.x - 1, y = coords.y + 1 }
    , { x = coords.x, y = coords.y - 1 }
    , { x = coords.x, y = coords.y + 1 }
    , { x = coords.x + 1, y = coords.y - 1 }
    , { x = coords.x + 1, y = coords.y }
    , { x = coords.x + 1, y = coords.y + 1 }
    ]


isAMineExploded : PlayGameGrid -> Bool
isAMineExploded =
    let
        isExplodedMine : GameCell -> Bool -> Bool
        isExplodedMine cell exploded =
            case cell of
                GameCell MineCell Opened ->
                    True

                _ ->
                    exploded
    in
    Grid.foldl isExplodedMine False


areAllNoMineFieldsRevealed : PlayGameGrid -> Bool
areAllNoMineFieldsRevealed =
    let
        isMissingFieldOpen : GameCell -> Bool -> Bool
        isMissingFieldOpen cell allRevealed =
            case cell of
                GameCell EmptyCell state ->
                    case state of
                        Opened ->
                            allRevealed

                        _ ->
                            False

                GameCell (MineNeighbourCell _) state ->
                    case state of
                        Opened ->
                            allRevealed

                        _ ->
                            False

                _ ->
                    allRevealed
    in
    Grid.foldl isMissingFieldOpen True


minesIndexGenerator : Int -> List Int -> Generator (Set Int) -> Generator (Set Int)
minesIndexGenerator remainingMines remainingPossibilities alreadyGenerated =
    case remainingMines of
        0 ->
            alreadyGenerated

        _ ->
            case remainingPossibilities of
                [] ->
                    alreadyGenerated

                x :: xs ->
                    singleMineIndexGenerator x xs
                        |> appendGenerator alreadyGenerated
                        |> Random.andThen
                            (\set ->
                                let
                                    newRemainingMines =
                                        remainingMines - 1

                                    newRemainingPossibilities =
                                        List.filter
                                            (\idx ->
                                                Set.member idx set
                                                    |> not
                                            )
                                            remainingPossibilities
                                in
                                minesIndexGenerator newRemainingMines newRemainingPossibilities (Random.constant set)
                            )


calculateElapsedTimeMillis : List ( Time.Posix, Time.Posix ) -> Int
calculateElapsedTimeMillis =
    List.foldl (\( from, to ) summedUp -> (Time.posixToMillis to - Time.posixToMillis from) + summedUp) 0


appendGenerator : Generator (Set Int) -> Generator Int -> Generator (Set Int)
appendGenerator list single =
    Random.map2 (\toAdd set -> Set.insert toAdd set) single list


singleMineIndexGenerator : Int -> List Int -> Generator Int
singleMineIndexGenerator head restOfPossibleIdx =
    Random.uniform head restOfPossibleIdx


emptySetGenerator : Generator (Set Int)
emptySetGenerator =
    Random.constant Set.empty


extractMinesAndFlags : PlayGameGrid -> { mines : Int, flags : Int }
extractMinesAndFlags grid =
    Grid.foldl
        (\cell acc ->
            case cell of
                GameCell MineCell Flagged ->
                    { mines = acc.mines + 1, flags = acc.flags + 1 }

                GameCell MineCell _ ->
                    { acc | mines = acc.mines + 1 }

                GameCell _ Flagged ->
                    { acc | flags = acc.flags + 1 }

                _ ->
                    acc
        )
        { mines = 0, flags = 0 }
        grid


combineGridInfosWithElapsedTime : { mines : Int, flags : Int } -> Int -> GameStats
combineGridInfosWithElapsedTime minesFlags elapsedTime =
    { mines = minesFlags.mines, flags = minesFlags.flags, elapsedTime = elapsedTime }


getRunningGameStats : GameModel -> Maybe GameStats
getRunningGameStats gameModel =
    case gameModel.gameBoardStatus of
        RunningGame grid ->
            calculateElapsedTimeMillis gameModel.gameRunningTimes
                |> combineGridInfosWithElapsedTime (extractMinesAndFlags grid)
                |> Just

        FinishedGame grid _ elapsed ->
            combineGridInfosWithElapsedTime (extractMinesAndFlags grid) elapsed
                |> Just

        WaitOnStart initGameData ->
            Just
                { mines = initGameData.mines
                , flags = 0
                , elapsedTime = 0
                }

        _ ->
            Nothing


type alias GameStats =
    { mines : Int
    , flags : Int
    , elapsedTime : Int
    }
