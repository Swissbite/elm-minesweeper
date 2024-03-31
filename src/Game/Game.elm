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
import Element exposing (DeviceClass(..), Element, px)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import Game.Internal exposing (..)
import Grid
import Json.Decode as Decode
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
            ( tooglePause model, Cmd.none )

        ClockTick posix ->
            updateTimePlayGame model posix

        NavigationEvent to ->
            case ( model.currentView, to ) of
                ( Game, Game ) ->
                    ( model, Cmd.none )

                ( Game, _ ) ->
                    ( tooglePause model, Cmd.none )

                ( _, Game ) ->
                    ( tooglePause model, Cmd.none )

                ( _, _ ) ->
                    ( model, Cmd.none )


tooglePause : Model -> Model
tooglePause model =
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


updateModelByClickOnGameCell : Coordinates -> Model -> ( Model, Cmd GameMsg )
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
                        calculateElapsedTimeMilis gameModel.gameRunningTimes |> FinishedGame updatedPlayGrid Lost

                    else if allFieldsRevealed then
                        calculateElapsedTimeMilis gameModel.gameRunningTimes |> FinishedGame updatedPlayGrid Won

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
        gameGridElement : PauseResumeState -> PlayGameGrid -> Element GameMsg
        gameGridElement pauseResumeState playGrid =
            case pauseResumeState of
                Paused ->
                    Lazy.lazy pausedGameView playGrid

                Resumed _ ->
                    Lazy.lazy runningGameView playGrid
    in
    case model.game.gameBoardStatus of
        NoGame _ ->
            Element.column [ Element.width Element.fill, Element.height Element.fill, Element.spacing 10 ] <|
                case model.device.class of
                    Phone ->
                        [ Styles.styledGameSelectionButton
                            { onPress = Just (CreateNewGame smallPlayground)
                            , label = Element.text "small"
                            }
                        , Styles.styledGameSelectionButton
                            { onPress = Just (CreateNewGame mediumPlayground)
                            , label = Element.text "medium"
                            }
                        , Styles.styledGameSelectionButton
                            { onPress = Just (CreateNewGame advancePlayground)
                            , label = Element.text "advanced"
                            }
                        , Styles.styledGameSelectionButton
                            { onPress = Just (CreateNewGame xxlPlayground)
                            , label = Element.text "xxl"
                            }
                        ]

                    _ ->
                        [ Element.row
                            [ Element.centerX, Element.centerY, Element.spacing 10 ]
                            [ Styles.styledGameSelectionButton
                                { onPress = Just (CreateNewGame smallPlayground)
                                , label = Element.text "small"
                                }
                            , Styles.styledGameSelectionButton
                                { onPress = Just (CreateNewGame mediumPlayground)
                                , label = Element.text "medium"
                                }
                            ]
                        , Element.row
                            [ Element.centerX, Element.centerY, Element.spacing 10 ]
                            [ Styles.styledGameSelectionButton
                                { onPress = Just (CreateNewGame advancePlayground)
                                , label = Element.text "advanced"
                                }
                            , Styles.styledGameSelectionButton
                                { onPress = Just (CreateNewGame xxlPlayground)
                                , label = Element.text "xxl"
                                }
                            ]
                        ]

        WaitOnStart initGameGrid ->
            Lazy.lazy
                (\initGrid ->
                    Element.column
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.padding 20
                        ]
                    <|
                        case model.device.class of
                            Phone ->
                                [ sidebarElement model, initGameGridView initGrid ]

                            Tablet ->
                                [ sidebarElement model, initGameGridView initGrid ]

                            _ ->
                                [ Element.row [ Element.centerX, Element.width Element.fill, Element.height Element.fill ] [ initGameGridView initGrid, sidebarElement model ]
                                ]
                )
                initGameGrid

        RunningGame playGrid ->
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.padding 20
                ]
            <|
                case model.device.class of
                    Phone ->
                        [ sidebarElement model, gameGridElement model.game.gamePauseResumeState playGrid ]

                    Tablet ->
                        [ sidebarElement model, gameGridElement model.game.gamePauseResumeState playGrid ]

                    _ ->
                        [ Element.row [ Element.centerX, Element.width Element.fill, Element.height Element.fill ] [ gameGridElement model.game.gamePauseResumeState playGrid, sidebarElement model ]
                        ]

        FinishedGame playGrid finishedStatus _ ->
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.padding 20
                , Element.inFront <|
                    sidebarElement model
                ]
                [ Lazy.lazy2 finishedGameView playGrid finishedStatus
                ]


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


sidebarElement : Model -> Element GameMsg
sidebarElement model =
    let
        toogleElement =
            case model.game.gameBoardStatus of
                RunningGame _ ->
                    Lazy.lazy mineToggleElement model.game.gameInteractionMode

                _ ->
                    Element.none

        toggleElementLabel =
            case model.game.gameBoardStatus of
                RunningGame _ ->
                    Element.el [ Font.bold ] <| Element.text "Reveal/Flag"

                _ ->
                    Element.none

        giveUpElement : Element GameMsg
        giveUpElement =
            case model.game.gameBoardStatus of
                FinishedGame _ _ _ ->
                    Element.none

                _ ->
                    Input.button [ Background.color Colors.black, Border.solid, Element.padding 10, Border.rounded 10, Font.color Colors.gold ]
                        { onPress = Just GoToStartPage
                        , label =
                            Element.text <|
                                case model.game.gameBoardStatus of
                                    RunningGame _ ->
                                        "Give up 💀"

                                    WaitOnStart _ ->
                                        "Cancel ❌"

                                    _ ->
                                        ""
                        }

        toogleGamePauseElement : Element GameMsg
        toogleGamePauseElement =
            case ( model.game.gameBoardStatus, model.game.gamePauseResumeState ) of
                ( RunningGame _, Paused ) ->
                    Input.button [ Border.solid, Element.padding 0, Border.rounded 10, Font.size 42 ]
                        { onPress = Just ToogleGamePause
                        , label = Element.text Styles.icons.resume
                        }

                ( RunningGame _, Resumed _ ) ->
                    Input.button [ Border.solid, Element.padding 0, Border.rounded 10, Font.size 42 ]
                        { onPress = Just ToogleGamePause
                        , label = Element.text Styles.icons.pause
                        }

                _ ->
                    Element.none

        gameInformationElements : List (Element GameMsg)
        gameInformationElements =
            case getRunningGameStats model.game of
                Nothing ->
                    []

                Just data ->
                    [ Element.el [ Font.bold ] <| Element.text <| String.concat [ Styles.icons.stopWatch, " ", milisToString data.elapsedTime ]
                    , Element.el [ Font.bold ] <| Element.text <| String.concat [ String.fromChar Styles.icons.untouchedBomb, " ", String.fromInt data.mines ]
                    , Element.el [ Font.bold ] <| Element.text <| String.concat [ String.fromChar Styles.icons.markerFlag, " ", String.fromInt data.flags ]
                    ]

        gameFinishedElements =
            case model.game.gameBoardStatus of
                FinishedGame playGameGrid gameResult _ ->
                    [ case gameResult of
                        Won ->
                            Element.text "You won!"

                        Lost ->
                            Element.text "You lost!"
                    , Input.button [ Background.color Colors.asparagus, Border.solid, Element.padding 10, Border.rounded 10 ]
                        { onPress = Just (CreateNewGame <| playGameGridToPlaygroundDefinition playGameGrid)
                        , label = Element.text "Start new game"
                        }
                    , Input.button [ Background.color Colors.saffron, Border.solid, Element.padding 10, Border.rounded 10 ]
                        { onPress = Just GoToStartPage
                        , label = Element.text "Back to overview"
                        }
                    ]

                _ ->
                    []
    in
    gameInformationElements
        ++ [ toggleElementLabel
           , toogleElement
           , giveUpElement
           , toogleGamePauseElement
           ]
        ++ gameFinishedElements
        ++ (case ( model.device.class == Phone, model.device.class == Tablet ) of
                ( False, False ) ->
                    [ Element.column [ Font.bold ] [ Element.text "Shortcuts:", Element.text "T: Toogle Selector", Element.text "P: Pause/Resume" ] ]

                ( _, _ ) ->
                    []
           )
        |> (case model.device.class of
                Phone ->
                    Element.wrappedRow [ Element.alignTop, Element.centerX, Element.padding 20, Element.spacing 10 ]

                Tablet ->
                    Element.wrappedRow [ Element.alignTop, Element.centerX, Element.padding 20, Element.spacing 10 ]

                _ ->
                    Element.column
                        [ Element.alignTop
                        , Element.alignRight
                        , Element.padding 20
                        , Element.spacing 10
                        ]
           )


styledToogleElement : Bool -> Element GameMsg
styledToogleElement =
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
            , icon = styledToogleElement
            }


initGameGridView : InitGameData -> Element GameMsg
initGameGridView initGameGrid =
    let
        indexedFn =
            initGameCellToElement initGameGrid

        gridWithElements =
            Grid.indexedMap indexedFn initGameGrid.grid

        gridAsListOfRows =
            Grid.rows gridWithElements |> Array.map Array.toList |> Array.map (\l -> Element.row [] l) |> Array.toList
    in
    Element.column [ Element.centerX, Element.centerY ] gridAsListOfRows


initGameCellToElement : InitGameData -> (Int -> Int -> InitGameCell -> Element GameMsg)
initGameCellToElement initGameGrid =
    \x y _ ->
        let
            coords =
                Coordinates x y
        in
        Element.el (Styles.untouchedCellStyle ++ [ Events.onClick <| ClickedOnInitGameCell initGameGrid coords ]) <| Element.text ""


gameView : PlayGameGrid -> (Grid.Grid GameCell -> Grid.Grid (Element GameMsg)) -> Element GameMsg
gameView playGameGrid gridGameToGridElementMapper =
    playGameGrid
        |> gridGameToGridElementMapper
        |> Grid.rows
        |> Array.map Array.toList
        |> Array.map (\l -> Element.row [] l)
        |> Array.toList
        |> Element.column [ Element.centerX, Element.centerY ]


runningGameView : PlayGameGrid -> Element GameMsg
runningGameView playGameGrid =
    gameView playGameGrid <| Grid.indexedMap runningGameCellToElement


pausedGameView : PlayGameGrid -> Element GameMsg
pausedGameView playGameGrid =
    Element.el
        [ Element.width Element.fill
        , Element.inFront <|
            Element.el
                [ Element.centerX
                , Element.centerY
                , Element.width <| px 400
                , Element.height <| px 400
                , Background.color <| Element.rgba255 255 0 0 0.5
                ]
            <|
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    , Font.extraBold
                    , Font.size 99
                    ]
                <|
                    Element.text "Paused"
        ]
    <|
        gameView playGameGrid <|
            Grid.map (\_ -> Element.el Styles.openedCellStyle Element.none)


runningGameCellToElement : Int -> Int -> GameCell -> Element GameMsg
runningGameCellToElement x y cell =
    case cell of
        GameCell _ Flagged ->
            Element.el (Styles.untouchedCellStyle ++ [ Events.onClick <| ClickOnGameCell { x = x, y = y } ]) <| Element.el [ Element.centerX, Element.centerY ] <| Element.text <| String.fromChar Styles.icons.markerFlag

        GameCell _ Untouched ->
            Element.el (Styles.untouchedCellStyle ++ [ Events.onClick <| ClickOnGameCell { x = x, y = y } ]) Element.none

        GameCell EmptyCell Opened ->
            Element.el Styles.openedCellStyle Element.none

        GameCell MineCell Opened ->
            Element.el Styles.openedCellStyle <| Element.el [ Element.centerX, Element.centerY ] <| Element.text <| String.fromChar Styles.icons.exploded

        GameCell (MineNeighbourCell neighbours) Opened ->
            Element.el (Styles.openedMineNeighbourCellStyle neighbours ++ [ Events.onClick <| ClickOnGameCell { x = x, y = y } ]) <| Element.el [ Element.centerX, Element.centerY ] <| Element.text (String.fromInt neighbours)


finishedGameView : PlayGameGrid -> GameResult -> Element GameMsg
finishedGameView playGameGrid _ =
    Element.column [ Element.centerX, Element.centerY ]
        [ finishedGridToView playGameGrid
        ]


finishedGridToView : PlayGameGrid -> Element GameMsg
finishedGridToView playGameGrid =
    playGameGrid
        |> Grid.map finishedGameCellToElement
        |> Grid.rows
        |> Array.map Array.toList
        |> Array.map (\l -> Element.row [] l)
        |> Array.toList
        |> Element.column [ Element.centerX, Element.centerY ]


finishedGameCellToElement : GameCell -> Element GameMsg
finishedGameCellToElement cell =
    case cell of
        GameCell MineCell Opened ->
            Element.el Styles.openedCellStyle <| Element.el [ Element.centerX, Element.centerY ] <| Element.text <| String.fromChar Styles.icons.exploded

        GameCell MineCell _ ->
            Element.el Styles.openedCellStyle <| Element.el [ Element.centerX, Element.centerY ] <| Element.text <| String.fromChar Styles.icons.untouchedBomb

        GameCell (MineNeighbourCell neighbours) Opened ->
            Element.el (Styles.openedMineNeighbourCellStyle neighbours) <| Element.el [ Element.centerX, Element.centerY ] <| Element.text (String.fromInt neighbours)

        GameCell EmptyCell Opened ->
            Element.el Styles.openedCellStyle Element.none

        GameCell _ Flagged ->
            Element.el Styles.untouchedCellStyle <| Element.el [ Element.centerX, Element.centerY ] <| Element.text <| String.fromChar Styles.icons.markerFlag

        _ ->
            Element.el Styles.untouchedCellStyle Element.none



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
generatePlayGameGrid : InitGameData -> Coordinates -> Generator PlayGameGrid
generatePlayGameGrid initGameGrid coords =
    let
        initialPossibilities : List Int
        initialPossibilities =
            generateListOfPossibleIndizes initGameGrid.grid coords

        gameGridWidth =
            Grid.width initGameGrid.grid

        gameGridHeight =
            Grid.height initGameGrid.grid

        minesIdxGenerator : Generator (Set Int)
        minesIdxGenerator =
            minesIndexGenerator initGameGrid.mines initialPossibilities emptySetGenerator

        minesIdxAsCoordinates : Set Int -> List Coordinates
        minesIdxAsCoordinates indizes =
            Set.toList indizes
                |> List.map (\i -> { x = modBy gameGridWidth i, y = i // gameGridWidth })

        minesIdxAsCoordinatesGenerator : Generator (Set Int) -> Generator (List Coordinates)
        minesIdxAsCoordinatesGenerator =
            Random.andThen (\set -> minesIdxAsCoordinates set |> Random.constant)
    in
    minesIdxAsCoordinatesGenerator minesIdxGenerator
        |> Random.andThen
            (\coordinates ->
                List.map coordinatesToPair coordinates
                    |> createPlayGameGrid gameGridWidth gameGridHeight
                    |> openCell coords
                    |> Random.constant
            )


coordinatesToPair : Coordinates -> ( Int, Int )
coordinatesToPair coords =
    ( coords.x, coords.y )


createPlayGameGrid : Int -> Int -> List ( Int, Int ) -> PlayGameGrid
createPlayGameGrid widht height mineCoordinates =
    let
        grid =
            Grid.repeat widht height <| GameCell EmptyCell Untouched

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


flagCell : Coordinates -> PlayGameGrid -> PlayGameGrid
flagCell coords playGrid =
    coordinatesToPair coords
        |> (\c ->
                Grid.get c playGrid
                    |> (\cell ->
                            case cell of
                                Just (GameCell gameCell Flagged) ->
                                    Grid.set c (GameCell gameCell Untouched) playGrid

                                Just (GameCell gameCell Untouched) ->
                                    Grid.set c (GameCell gameCell Flagged) playGrid

                                Just (GameCell (MineNeighbourCell neighbhours) Opened) ->
                                    if neighbhours == calculateFlaggedCellsArroundCoordinate coords playGrid then
                                        openSurroundingCells coords playGrid

                                    else
                                        playGrid

                                _ ->
                                    playGrid
                       )
           )


openCell : Coordinates -> PlayGameGrid -> PlayGameGrid
openCell coords playGrid =
    let
        coordinateAsPair =
            coordinatesToPair coords

        cell =
            Grid.get coordinateAsPair playGrid
    in
    case cell of
        Nothing ->
            playGrid

        Just (GameCell (MineNeighbourCell neighbhours) Opened) ->
            if neighbhours == calculateFlaggedCellsArroundCoordinate coords playGrid then
                openSurroundingCells coords playGrid

            else
                playGrid

        Just (GameCell cellType cellStatus) ->
            case ( cellType, cellStatus ) of
                ( _, Opened ) ->
                    playGrid

                ( _, Flagged ) ->
                    playGrid

                ( MineNeighbourCell neighbhours, _ ) ->
                    Grid.set coordinateAsPair (GameCell (MineNeighbourCell neighbhours) Opened) playGrid

                ( EmptyCell, _ ) ->
                    Grid.set coordinateAsPair (GameCell EmptyCell Opened) playGrid
                        |> (\nextGrid ->
                                calculateNeighbourCoordinates coords
                                    |> (\surroundingCoordinatesAsPair -> List.foldl (\coord grid -> openCell coord grid) nextGrid surroundingCoordinatesAsPair)
                           )

                ( MineCell, _ ) ->
                    Grid.set coordinateAsPair (GameCell MineCell Opened) playGrid


openSurroundingCells : Coordinates -> PlayGameGrid -> PlayGameGrid
openSurroundingCells coords playGrid =
    let
        mapCoordstoCoordsAndMaybeCellPair : Coordinates -> ( Coordinates, Maybe GameCell )
        mapCoordstoCoordsAndMaybeCellPair neighbourCoords =
            ( neighbourCoords, Grid.get (coordinatesToPair neighbourCoords) playGrid )

        foldCoordsCellPair : ( Coordinates, Maybe GameCell ) -> PlayGameGrid -> PlayGameGrid
        foldCoordsCellPair ( coord, maybeCell ) grid =
            case maybeCell of
                Just (GameCell _ Untouched) ->
                    openCell coord grid

                _ ->
                    grid
    in
    calculateNeighbourCoordinates coords
        |> List.map mapCoordstoCoordsAndMaybeCellPair
        |> List.foldl foldCoordsCellPair playGrid


calculateFlaggedCellsArroundCoordinate : Coordinates -> PlayGameGrid -> Int
calculateFlaggedCellsArroundCoordinate coords grid =
    calculateNeighbourCoordinates coords
        |> List.map coordinatesToPair
        |> List.map (\coordPair -> Grid.get coordPair grid)
        |> List.map
            (\maybeCell ->
                case maybeCell of
                    Just (GameCell _ Flagged) ->
                        1

                    _ ->
                        0
            )
        |> List.sum


calculateNeighbourCoordinates : Coordinates -> List Coordinates
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


calculateElapsedTimeMilis : List ( Time.Posix, Time.Posix ) -> Int
calculateElapsedTimeMilis =
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
            calculateElapsedTimeMilis gameModel.gameRunningTimes
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
