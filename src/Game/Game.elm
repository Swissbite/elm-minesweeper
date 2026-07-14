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


module Game.Game exposing (decodeStoredFinishedGameHistory, decodeStoredRunningGame, initModel, resumeGame, subscriptions, update, view)

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
import Html
import Html.Attributes as HA
import Json.Decode as Decode
import List
import Random exposing (Generator)
import Set exposing (Set)
import Styles exposing (..)
import Theme exposing (Theme)
import Time
import Types exposing (..)


initModel : PlayGroundDefinition -> GameModel
initModel definition =
    { gameBoardStatus = WaitOnStart <| createInitGameGrid definition
    , gameInteractionMode = Reveal
    , gameRunningTimes = []
    , gamePauseResumeState = Paused
    , lastClockTick = Time.millisToPosix 0
    }



----- Subscription -----


decodeStoredFinishedGameHistory : String -> List FinishedGameHistoryEntry
decodeStoredFinishedGameHistory string =
    Decode.decodeString decodeFinishedGameHistory string
        |> Result.withDefault []


decodeStoredRunningGame : String -> String -> Maybe GameModel
decodeStoredRunningGame browserSalt string =
    if String.startsWith "{" string then
        -- Legacy plain JSON save, written before the stored value was obfuscated.
        Decode.decodeString (decodeRunningGameEnvelope browserSalt) string
            |> Result.toMaybe

    else
        deobfuscateRunningGame browserSalt string
            |> Maybe.andThen
                (\envelope ->
                    Decode.decodeString (decodeRunningGameEnvelope browserSalt) envelope
                        |> Result.toMaybe
                )


{-| Resumes a paused running game by opening a fresh time segment. The Resumed
counter must stay one ahead of the recorded segments so that the next ClockTick
starts a new segment instead of extending the last one (see updateTimePlayGame).
-}
resumeGame : GameModel -> GameModel
resumeGame gameModel =
    case ( gameModel.gameBoardStatus, gameModel.gamePauseResumeState ) of
        ( RunningGame _, Paused ) ->
            { gameModel | gamePauseResumeState = Resumed (List.length gameModel.gameRunningTimes + 1) }

        _ ->
            gameModel


subscriptions : Model -> GameModel -> Sub GameMsg
subscriptions m gameModel =
    case ( m.currentView, gameModel.gameBoardStatus ) of
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


update : GameMsg -> GameModel -> Model -> ( Model, Cmd GameMsg )
update gameMsg gameModel model =
    case gameMsg of
        NoUpdate ->
            ( model, Cmd.none )

        GoToStartPage ->
            ( model, Cmd.none )

        ClickedOnInitGameCell initGame coords ->
            ( model, generatePlayGameGrid initGame coords |> Random.generate StartGame )

        StartGame playGrid ->
            let
                newGameModel =
                    { gameModel | gameBoardStatus = RunningGame playGrid, gameRunningTimes = [], gamePauseResumeState = Resumed 1 }
            in
            ( { model | game = Just newGameModel }, saveRunningGame model.runningGameSalt newGameModel )

        CreateNewGame _ ->
            ( model, Cmd.none )

        ResumeSavedGame ->
            ( model, Cmd.none )

        ClickOnGameCell coords ->
            updateModelByClickOnGameCell coords gameModel model

        ToogleGameCellInteractionMode ->
            let
                nextMode =
                    case gameModel.gameInteractionMode of
                        Reveal ->
                            Flag

                        Flag ->
                            Reveal
            in
            ( { model | game = Just { gameModel | gameInteractionMode = nextMode } }, Cmd.none )

        ToogleGamePause ->
            let
                newModel =
                    togglePause gameModel model
            in
            ( newModel, saveRunningGameOf newModel )

        ClockTick posix ->
            updateTimePlayGame gameModel model posix

        NavigationEvent to ->
            case ( model.currentView, to ) of
                ( Game, Game ) ->
                    ( model, Cmd.none )

                ( Game, _ ) ->
                    let
                        newModel =
                            togglePause gameModel model
                    in
                    ( newModel, saveRunningGameOf newModel )

                ( _, Game ) ->
                    ( togglePause gameModel model, Cmd.none )

                ( _, _ ) ->
                    ( model, Cmd.none )


{-| Persists the current running game of the top level model, if there is one.
-}
saveRunningGameOf : Model -> Cmd GameMsg
saveRunningGameOf model =
    model.game
        |> Maybe.map (saveRunningGame model.runningGameSalt)
        |> Maybe.withDefault Cmd.none


togglePause : GameModel -> Model -> Model
togglePause gameModel model =
    case ( gameModel.gameBoardStatus, gameModel.gamePauseResumeState ) of
        ( RunningGame _, Paused ) ->
            { model | game = Just (resumeGame gameModel) }

        ( RunningGame _, Resumed _ ) ->
            { model | game = Just { gameModel | gamePauseResumeState = Paused } }

        _ ->
            model


updateTimePlayGame : GameModel -> Model -> Time.Posix -> ( Model, Cmd GameMsg )
updateTimePlayGame gameModel model time =
    case ( gameModel.gameBoardStatus, gameModel.gamePauseResumeState ) of
        ( RunningGame _, Resumed timesResume ) ->
            let
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
                        ( time, time ) :: gameModel.gameRunningTimes

                newGameModel =
                    { gameModel | gameRunningTimes = newList, lastClockTick = time }
            in
            ( { model | game = Just newGameModel }, saveRunningGame model.runningGameSalt newGameModel )

        _ ->
            ( model, Cmd.none )


updateModelByClickOnGameCell : Coordinate -> GameModel -> Model -> ( Model, Cmd GameMsg )
updateModelByClickOnGameCell coords gameModel model =
    case ( gameModel.gamePauseResumeState, gameModel.gameBoardStatus ) of
        ( Resumed _, RunningGame playGrid ) ->
            let
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

                nextGameModel =
                    { gameModel | gameBoardStatus = nextGameBoardStatus }

                persistCmd =
                    case nextGameBoardStatus of
                        FinishedGame _ _ _ ->
                            Cmd.batch [ saveFinishedGameHistory nextHistoryList, clearRunningGame ]

                        _ ->
                            saveRunningGame model.runningGameSalt nextGameModel
            in
            ( { model | game = Just nextGameModel, playedGameHistory = nextHistoryList }, persistCmd )

        _ ->
            ( model, Cmd.none )



----- VIEW for Game -----


view : Model -> GameModel -> Element GameMsg
view model gameModel =
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
    case gameModel.gameBoardStatus of
        WaitOnStart initGameGrid ->
            let
                boardConfig =
                    boardConfigFromInitGrid initGameGrid
            in
            gameScreenLayout model gameModel boardConfig <|
                Lazy.lazy2 initGameGridView boardConfig initGameGrid

        RunningGame playGrid ->
            let
                boardConfig =
                    boardConfigFromPlayGrid playGrid
            in
            gameScreenLayout model gameModel boardConfig <|
                gameGridElement boardConfig gameModel.gamePauseResumeState playGrid

        FinishedGame playGrid finishedStatus _ ->
            let
                boardConfig =
                    boardConfigFromPlayGrid playGrid
            in
            gameScreenLayout model gameModel boardConfig <|
                Lazy.lazy3 finishedGameView boardConfig playGrid finishedStatus


type alias BoardViewConfig =
    { cellSize : Int
    , cols : Int
    , rows : Int
    , isMobile : Bool
    , theme : Theme
    }


boardViewConfig : Model -> Int -> Int -> BoardViewConfig
boardViewConfig model cols rows =
    { cellSize = Styles.cellPixelSize model.device { cols = cols, rows = rows }
    , cols = cols
    , rows = rows
    , isMobile = model.device.class == Phone || model.device.class == Tablet
    , theme = model.theme
    }


gameScreenLayout : Model -> GameModel -> BoardViewConfig -> Element GameMsg -> Element GameMsg
gameScreenLayout model gameModel boardConfig boardElement =
    if boardConfig.isMobile then
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding 12
            , Element.spacing 12
            ]
            [ mobileStatusBarElement model gameModel
            , Element.el [ Element.width Element.fill, Element.height Element.fill ] <| boardViewport boardConfig boardElement
            , mobileActionBarElement model gameModel
            ]

    else
        Element.row
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding 20
            , Element.spacing 20
            ]
            [ Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.htmlAttribute <| HA.style "min-width" "0"
                ]
              <|
                boardViewport boardConfig boardElement
            , sidebarElement model gameModel
            ]


boardViewport : BoardViewConfig -> Element GameMsg -> Element GameMsg
boardViewport boardConfig boardElement =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color (Colors.surface boardConfig.theme)
        , Border.color (Colors.cellBorderColor boardConfig.theme)
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
            [ Element.centerX
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


gameInformationElements : Model -> GameModel -> List (Element GameMsg)
gameInformationElements model gameModel =
    case getRunningGameStats gameModel of
        Nothing ->
            []

        Just data ->
            [ Styles.pillBadge model.theme <| String.concat [ Styles.icons.stopWatch, " ", millisToString data.elapsedTime ]
            , Styles.pillBadge model.theme <| String.concat [ String.fromChar Styles.icons.untouchedBomb, " ", String.fromInt data.mines ]
            , Styles.pillBadge model.theme <| String.concat [ String.fromChar Styles.icons.markerFlag, " ", String.fromInt data.flags ]
            ]


modeSelectorElements : Model -> GameModel -> List (Element GameMsg)
modeSelectorElements model gameModel =
    case gameModel.gameBoardStatus of
        RunningGame _ ->
            [ Element.row
                [ Element.spacing 8
                , Background.color (Colors.openedCellGray model.theme)
                , Border.rounded Styles.pillBorderRadius
                , Element.paddingXY 10 6
                ]
                [ Element.el [ Font.bold, Element.centerY, Element.width (Element.px 130) ] <|
                    Element.text <|
                        case gameModel.gameInteractionMode of
                            Reveal ->
                                "Mode: Reveal"

                            Flag ->
                                "Mode: Flag"
                , Lazy.lazy2 mineToggleElement model.theme gameModel.gameInteractionMode
                ]
            ]

        _ ->
            []


giveUpElements : Model -> GameModel -> List (Element GameMsg)
giveUpElements model gameModel =
    case gameModel.gameBoardStatus of
        FinishedGame _ _ _ ->
            []

        RunningGame _ ->
            [ Input.button [ Background.color (Colors.surface model.theme), Border.solid, Element.paddingXY 12 10, Border.rounded 10, Font.color (Colors.primary model.theme) ]
                { onPress = Just GoToStartPage
                , label = Element.text "Give up 💀"
                }
            ]

        WaitOnStart _ ->
            [ Input.button [ Background.color (Colors.surface model.theme), Border.solid, Element.paddingXY 12 10, Border.rounded 10, Font.color (Colors.primary model.theme) ]
                { onPress = Just GoToStartPage
                , label = Element.text "Cancel ❌"
                }
            ]


pauseToggleElements : Model -> GameModel -> List (Element GameMsg)
pauseToggleElements model gameModel =
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
    case ( gameModel.gameBoardStatus, gameModel.gamePauseResumeState ) of
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


gameActionElements : Model -> GameModel -> List (Element GameMsg)
gameActionElements model gameModel =
    modeSelectorElements model gameModel ++ giveUpElements model gameModel ++ pauseToggleElements model gameModel


gameFinishedElements : Model -> GameModel -> List (Element GameMsg)
gameFinishedElements model gameModel =
    case gameModel.gameBoardStatus of
        FinishedGame playGameGrid gameResult _ ->
            [ Styles.pillBadge model.theme <|
                case gameResult of
                    Won ->
                        "You won!"

                    Lost ->
                        "You lost!"
            , Input.button [ Background.color (Colors.primary model.theme), Border.solid, Element.paddingXY 12 10, Border.rounded 10, Font.color Colors.white ]
                { onPress = Just (CreateNewGame <| playGameGridToPlaygroundDefinition playGameGrid)
                , label = Element.text "Start new game"
                }
            , Input.button [ Background.color (Colors.warning model.theme), Border.solid, Element.paddingXY 12 10, Border.rounded 10, Font.color Colors.white ]
                { onPress = Just GoToStartPage
                , label = Element.text "Back to overview"
                }
            ]

        _ ->
            []


mobileStatusBarElement : Model -> GameModel -> Element GameMsg
mobileStatusBarElement model gameModel =
    wrapIfNotEmpty (gameInformationElements model gameModel)


mobileActionBarElement : Model -> GameModel -> Element GameMsg
mobileActionBarElement model gameModel =
    let
        actionElements =
            gameActionElements model gameModel ++ gameFinishedElements model gameModel
    in
    wrapIfNotEmpty actionElements


wrapIfNotEmpty : List (Element msg) -> Element msg
wrapIfNotEmpty elements =
    if List.isEmpty elements then
        Element.none

    else
        Element.wrappedRow
            [ Element.width Element.fill
            , Element.spacing 16
            , Element.spaceEvenly
            ]
            elements


sidebarElement : Model -> GameModel -> Element GameMsg
sidebarElement model gameModel =
    Element.column
        [ Element.width Element.shrink
        , Element.alignTop
        , Element.spacing 12
        ]
        (gameInformationElements model gameModel
            ++ gameActionElements model gameModel
            ++ gameFinishedElements model gameModel
            ++ [ Element.column [ Font.bold ]
                    [ Element.text "Shortcuts:"
                    , Element.text "T: Toggle Selector"
                    , Element.text "P: Pause/Resume"
                    ]
               ]
        )


styledToggleElement : Theme -> Bool -> Element GameMsg
styledToggleElement theme =
    Styles.toggleCheckboxWidget
        { offColor = Colors.untouchedCellGray theme
        , onColor = Colors.primary theme
        , sliderColor = Colors.white
        , toggleWidth = 60
        , toggleHeight = 28
        , onSymbol = Just Styles.icons.untouchedBomb
        , offSymbol = Just Styles.icons.markerFlag
        , tooltip = Just "Shortcut: T"
        }


mineToggleElement : Theme -> CellClickMode -> Element GameMsg
mineToggleElement theme gameInteractionMode =
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
            , icon = styledToggleElement theme
            }


initGameGridView : BoardViewConfig -> InitGameData -> Element GameMsg
initGameGridView boardConfig initGameGrid =
    let
        indexedFn =
            initGameCellToElement boardConfig.theme boardConfig.cellSize initGameGrid

        gridWithElements =
            Grid.indexedMap indexedFn initGameGrid.grid

        gridAsListOfRows =
            Grid.rows gridWithElements |> Array.map Array.toList |> Array.map (\l -> Element.row [] l) |> Array.toList
    in
    Element.column [ Element.alignTop ] gridAsListOfRows


initGameCellToElement : Theme -> Int -> InitGameData -> (Int -> Int -> InitGameCell -> Element GameMsg)
initGameCellToElement theme cellSize initGameGrid =
    \x y _ ->
        let
            coords =
                Coordinate x y
        in
        Element.el (Styles.untouchedCellStyle theme cellSize ++ [ Events.onClick <| ClickedOnInitGameCell initGameGrid coords ]) <| Element.text ""


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
    gameView playGameGrid <| Grid.indexedMap (runningGameCellToElement boardConfig.theme boardConfig.cellSize)


pausedGameView : BoardViewConfig -> PlayGameGrid -> Element GameMsg
pausedGameView boardConfig playGameGrid =
    Element.el
        [ Element.width Element.fill
        , Element.inFront <|
            Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Background.color <| Element.rgba255 20 20 40 0.82
                , Element.htmlAttribute <| HA.class "pause-overlay"
                ]
            <|
                Element.column
                    [ Element.centerX
                    , Element.centerY
                    , Element.spacing 16
                    ]
                    [ Element.el
                        [ Element.centerX
                        , Font.size <|
                            if boardConfig.isMobile then
                                mobilePauseOverlayFontSize

                            else
                                desktopPauseOverlayFontSize
                        , Element.htmlAttribute <| HA.class "hourglass-spin"
                        ]
                      <|
                        Element.text "⏳"
                    , Element.el
                        [ Element.centerX
                        , Font.extraBold
                        , Font.size <|
                            if boardConfig.isMobile then
                                mobilePauseOverlayFontSize // 2

                            else
                                desktopPauseOverlayFontSize // 2
                        , Font.color Colors.white
                        ]
                      <|
                        Element.text "Paused"
                    ]
        ]
    <|
        gameView playGameGrid <|
            Grid.map (\_ -> Element.el (Styles.openedCellStyle boardConfig.theme boardConfig.cellSize) Element.none)


runningGameCellToElement : Theme -> Int -> Int -> Int -> GameCell -> Element GameMsg
runningGameCellToElement theme cellSize x y cell =
    case cell of
        GameCell _ Flagged ->
            Element.el (Styles.untouchedCellStyle theme cellSize ++ [ Events.onClick <| ClickOnGameCell { x = x, y = y } ]) <| Element.el [ Element.centerX, Element.centerY ] <| Element.text <| String.fromChar Styles.icons.markerFlag

        GameCell _ Untouched ->
            Element.el (Styles.untouchedCellStyle theme cellSize ++ [ Events.onClick <| ClickOnGameCell { x = x, y = y } ]) Element.none

        GameCell EmptyCell Opened ->
            Element.el (Styles.openedCellStyle theme cellSize) Element.none

        GameCell MineCell Opened ->
            Element.el (Styles.openedCellStyle theme cellSize) <| Element.el [ Element.centerX, Element.centerY ] <| Element.text <| String.fromChar Styles.icons.exploded

        GameCell (MineNeighbourCell neighbours) Opened ->
            Element.el (Styles.openedMineNeighbourCellStyle theme cellSize neighbours ++ [ Events.onClick <| ClickOnGameCell { x = x, y = y } ]) <| Element.el [ Element.centerX, Element.centerY ] <| Element.text (String.fromInt neighbours)


finishedGameView : BoardViewConfig -> PlayGameGrid -> GameResult -> Element GameMsg
finishedGameView boardConfig playGameGrid result =
    let
        grid =
            finishedGridToView boardConfig playGameGrid
    in
    case result of
        Won ->
            Element.el
                [ Element.alignTop
                , Element.inFront confettiOverlay
                ]
                grid

        Lost ->
            Element.el
                [ Element.alignTop
                , Element.htmlAttribute <| HA.class "board-shake"
                ]
                grid


finishedGridToView : BoardViewConfig -> PlayGameGrid -> Element GameMsg
finishedGridToView boardConfig playGameGrid =
    playGameGrid
        |> Grid.map (finishedGameCellToElement boardConfig.theme boardConfig.cellSize)
        |> Grid.rows
        |> Array.map Array.toList
        |> Array.map (\l -> Element.row [] l)
        |> Array.toList
        |> Element.column [ Element.alignTop ]


finishedGameCellToElement : Theme -> Int -> GameCell -> Element GameMsg
finishedGameCellToElement theme cellSize cell =
    case cell of
        GameCell MineCell Opened ->
            Element.el (Styles.openedCellStyle theme cellSize) <| Element.el [ Element.centerX, Element.centerY ] <| Element.text <| String.fromChar Styles.icons.exploded

        GameCell MineCell _ ->
            Element.el (Styles.openedCellStyle theme cellSize) <| Element.el [ Element.centerX, Element.centerY ] <| Element.text <| String.fromChar Styles.icons.untouchedBomb

        GameCell (MineNeighbourCell neighbours) Opened ->
            Element.el (Styles.openedMineNeighbourCellStyle theme cellSize neighbours) <| Element.el [ Element.centerX, Element.centerY ] <| Element.text (String.fromInt neighbours)

        GameCell EmptyCell Opened ->
            Element.el (Styles.openedCellStyle theme cellSize) Element.none

        GameCell _ Flagged ->
            Element.el (Styles.untouchedCellStyle theme cellSize) <| Element.el [ Element.centerX, Element.centerY ] <| Element.text <| String.fromChar Styles.icons.markerFlag

        _ ->
            Element.el (Styles.untouchedCellStyle theme cellSize) Element.none


confettiOverlay : Element msg
confettiOverlay =
    Element.html <|
        Html.div
            [ HA.style "position" "absolute"
            , HA.style "top" "0"
            , HA.style "left" "0"
            , HA.style "right" "0"
            , HA.style "bottom" "0"
            , HA.style "overflow" "hidden"
            , HA.style "pointer-events" "none"
            ]
        <|
            List.map confettiParticle (List.range 0 39)


{-| Convert a count of deciseconds (tenths of a second) to a CSS time string,
e.g. 14 → "1.4s". Used for staggering confetti animation delays.
-}
decisecondsToCssTime : Int -> String
decisecondsToCssTime ds =
    String.fromInt (ds // 10) ++ "." ++ String.fromInt (modBy 10 ds) ++ "s"


{-| Confetti colors – a vivid rainbow palette kept separate from the game's
main color palette in Colors.elm because they are purely decorative and
animation-specific.
-}
confettiColors : List String
confettiColors =
    [ "#ff6b6b"
    , "#ffd93d"
    , "#6bcb77"
    , "#4d96ff"
    , "#c77dff"
    , "#ff9a3c"
    , "#ff6bca"
    , "#00c9a7"
    ]


confettiParticle : Int -> Html.Html msg
confettiParticle index =
    let
        colorCount =
            List.length confettiColors

        color =
            List.drop (modBy colorCount index) confettiColors
                |> List.head
                |> Maybe.withDefault "#ff6b6b"

        leftPct =
            String.fromInt (modBy 94 (index * 37 + 5)) ++ "%"

        delaySec =
            decisecondsToCssTime (modBy 20 (index * 7))

        durationSec =
            "2." ++ String.fromInt (modBy 10 (index * 3)) ++ "s"

        sizePx =
            modBy 6 index + 6

        heightPx =
            sizePx + modBy 5 (index * 7)

        borderRadius =
            case modBy 3 index of
                0 ->
                    "50%"

                1 ->
                    "2px"

                _ ->
                    "0"
    in
    Html.div
        [ HA.style "position" "absolute"
        , HA.style "width" (String.fromInt sizePx ++ "px")
        , HA.style "height" (String.fromInt heightPx ++ "px")
        , HA.style "background-color" color
        , HA.style "left" leftPct
        , HA.style "top" "0"
        , HA.style "border-radius" borderRadius
        , HA.style "will-change" "transform, opacity"
        , HA.style "animation" ("confetti-fall " ++ durationSec ++ " ease-in " ++ delaySec ++ " both")
        ]
        []



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


type alias GameStats =
    { mines : Int
    , flags : Int
    , elapsedTime : Int
    }
