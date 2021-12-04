module Main exposing (..)

import Browser
import Debug
import Element exposing (Color, Element, alignBottom, alignLeft, alignRight, centerX, centerY, column, el, fill, height, image, layout, maximum, padding, paddingXY, px, rgb, row, spacing, text, width)
import Element.Events as Events exposing (onClick)
import Element.Input exposing (button)
import Element.Background as Background
import Element.Border as Border
import Framework.Color as Color
import Element.Font as Font
import Html exposing (Html)
import Random exposing (Seed, Generator)
import Grid exposing (Grid)
import Set exposing (Set)
import Array
import Time



---- PROGRAM ----


main : Program Int Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }

--- VIEW ---

view : Model -> Html Msg
view model =
    layout [ width fill, height fill ] <|
        column
            [ spacing 20
            , width fill
            , height fill
            ]
            [ row [ paddingXY 20 0, spacing 20, width fill, height (fill |> maximum 75) ]
                [ image [ paddingXY 0 5, alignLeft, centerY, height fill ] { src = "./logo.svg", description = "Elm-Mine logo" }
                , el [ alignLeft, centerY ] <| text "Elm Minsesweeper"
                , button [ alignRight ] { onPress = Nothing, label = text "History" }
                , button [ alignRight ] { onPress = Nothing, label = text "About" }
                ]
            , gameBoardView model.gameStatus
            , row [ padding 20, width fill, height <| px 75, alignBottom ]
                [ el [ alignLeft ] <| text "Left footer"
                , el [ centerX ] <| text "center footer"
                , el [ alignRight ] <| text "right footer"
                ]
            ]
{-| Exposed function to render the main game view depending on the current GameStatus
-}
gameBoardView : GameStatus -> Element Msg
gameBoardView gameStatus =
    case gameStatus of
        NoGame ->
            gameBoardViewNoStatus

        ViewCustomGameDefinitionSetup ->
            Debug.todo "Implement view for custom game definition"

        InitGame seed definition ->
            row [ Element.width fill, Element.height fill ]
                [ initGameView seed definition
                , rightStatusBarView Nothing Nothing definition.amountOfMines
                ]

        _ ->
            el [ centerX, centerY ] <| text "Game Area from game"


gameBoardViewNoStatus : Element Msg
gameBoardViewNoStatus =
    let
        columnStyled : List (Element msg) -> Element msg
        columnStyled =
            column [ width fill, height fill, spacing 10 ]

        buttonStyled :
            { onPress : Maybe msg
            , label : Element msg
            }
            -> Element msg
        buttonStyled =
            button [ height fill, width fill, centerY, centerX, Background.color Color.grey_lighter ]

        createLabel : Maybe PlayGroundDefinition -> Element msg
        createLabel maybeDefinition =
            let
                styledLabelColumn : List (Element msg) -> Element msg
                styledLabelColumn =
                    Element.column [ width fill, height fill ]

                styledHeadlineText : String -> Element msg
                styledHeadlineText =
                    \t -> Element.el [ Font.bold, centerY, centerX ] <| text t

                styledLowerlineText : String -> Element msg
                styledLowerlineText =
                    \t -> Element.el [ centerY, centerX ] <| text t
            in
            case maybeDefinition of
                Just definition ->
                    styledLabelColumn
                        [ styledHeadlineText <| String.concat [ String.fromInt definition.dimensionX, " x ", String.fromInt definition.dimensionY ]
                        , styledLowerlineText <| String.concat [ String.fromInt definition.amountOfMines, "Mines" ]
                        ]

                Nothing ->
                    styledLabelColumn
                        [ styledHeadlineText "?"
                        , styledLowerlineText "User defined"
                        ]
    in
    row [ width fill, height fill, padding 10, spacing 10, Background.color Color.white_ter ]
        [ columnStyled
            [ buttonStyled { onPress = Just (GameMsg <| CreateGame defaultGameDefinitions.smallGame), label = createLabel <| Just defaultGameDefinitions.smallGame }
            , buttonStyled { onPress = Just (GameMsg <| CreateGame defaultGameDefinitions.largeGame), label = createLabel <| Just defaultGameDefinitions.largeGame }
            ]
        , columnStyled
            [ buttonStyled { onPress = Just (GameMsg <| CreateGame defaultGameDefinitions.mediumGame), label = createLabel <| Just defaultGameDefinitions.mediumGame }
            , buttonStyled { onPress = Nothing, label = createLabel Nothing }
            ]
        ]



activeScreenView : Model -> Element Msg
activeScreenView model =
    case model.activeSceen of
        GameScreen ->
            gameBoardView model.gameStatus

        _ ->
            Debug.todo "Implement other screens"

--- DEFINITONS ---
white : Color
white =
    rgb 1 1 1



---- MODEL ----


init : Int -> ( Model, Cmd Msg )
init currentTime =
    ( { gameStatus = NoGame, activeSceen = GameScreen, seed = Random.initialSeed currentTime }, Cmd.none )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameMsg updateMsg ->
            ( { model | gameStatus = interactWithGame updateMsg model }, Cmd.none )

        ChangeScreen screen ->
            screenChangeUpdate screen model
        UpdateSeed value ->  ({model | seed = Random.initialSeed value }, Cmd.none)
        NextTick posix -> (model, Cmd.none)


screenChangeUpdate : ActiveScreen -> Model -> ( Model, Cmd Msg )
screenChangeUpdate screen model =
    case screen of
        GameScreen ->
            ( { model | activeSceen = screen, gameStatus = togglePauseToTarget GameIsRunning model }, Cmd.none )

        GameHistoryScreen ->
            ( { model | activeSceen = screen, gameStatus = togglePauseToTarget GameIsPaused model }, Cmd.none )

        AboutScreen ->
            ( { model | activeSceen = screen, gameStatus = togglePauseToTarget GameIsPaused model }, Cmd.none )


type TargetGameStatus
    = GameIsPaused
    | GameIsRunning


togglePauseToTarget : TargetGameStatus -> Model -> GameStatus
togglePauseToTarget targetStatus model =
    case ( targetStatus, model.gameStatus ) of
        ( GameIsPaused, RunningGame _ ) ->
            interactWithGame TogglePause model

        ( GameIsRunning, PausedGame _ ) ->
            interactWithGame TogglePause model

        _ ->
            model.gameStatus


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameStatus of
        RunningGame _ -> Time.every 100 (\posix -> NextTick posix)
        _ ->
            Sub.none






defaultGameDefinitions : { smallGame : PlayGroundDefinition, mediumGame : PlayGroundDefinition, largeGame : PlayGroundDefinition }
defaultGameDefinitions =
    { smallGame = { amountOfMines = 10, dimensionX = 8, dimensionY = 8 }
    , mediumGame = { amountOfMines = 40, dimensionX = 16, dimensionY = 16 }
    , largeGame = { amountOfMines = 99, dimensionX = 30, dimensionY = 16 }
    }


rightStatusBarView : Maybe Int -> Maybe Int -> Int -> Element msg
rightStatusBarView flags elapsedTime amountOfMines =
    let
        timeText : String
        timeText =
            case elapsedTime of
                Nothing ->
                    "--:--"

                Just seconds ->
                    String.concat [ String.fromInt <| seconds // 60, ":", String.fromInt <| modBy 60 seconds ]

        missingFlags : String
        missingFlags =
            case flags of
                Nothing ->
                    String.concat <| List.append [ "-" ] <| List.repeat calculatePrefixZeroOfFlags "-"

                Just f ->
                    String.concat
                        [ String.concat <| List.repeat calculatePrefixZeroOfFlags "0"
                        , String.fromInt <| amountOfMines - f
                        ]

        calculatePrefixZeroOfFlags : Int
        calculatePrefixZeroOfFlags =
            let
                getBase10Truncated : Int -> Int
                getBase10Truncated =
                    \n -> truncate <| logBase 10 <| toFloat n

                amountOfMinesLogBase10 : Int
                amountOfMinesLogBase10 =
                    getBase10Truncated amountOfMines

                amountOfFlagsLogBase10 : Int
                amountOfFlagsLogBase10 =
                    case flags of
                        Nothing ->
                            1

                        Just 0 ->
                            1

                        Just f ->
                            getBase10Truncated f
            in
            amountOfMinesLogBase10 - amountOfFlagsLogBase10
    in
    Element.column [ width (Element.maximum 150 fill), Element.padding 10, Element.alignRight, Element.alignTop ]
        [ Element.el [ Font.bold, Font.size 30, Element.centerX ] <| text icons.stopWatch
        , Element.el [ Element.centerX, Element.paddingXY 0 10 ] <| text timeText
        , Element.el [ Font.bold, Element.paddingEach { bottom = 0, top = 30, left = 0, right = 0 }, Font.size 30, Element.centerX ] <| text icons.markerFlag
        , Element.el [ Element.centerX, Element.paddingXY 0 10 ] <| text missingFlags
        ]


initGameView : Seed -> PlayGroundDefinition -> Element Msg
initGameView seed definition =
    let
        grid : Grid (Element Msg)
        grid =
            Grid.initialize definition.dimensionX definition.dimensionY (\x y -> Element.el (basicMineCellStyle ++ [ onClick (GameMsg <| ClickCell { columnIndex = x, rowIndex = y }) ]) <| text "")

        rows : List (Element Msg)
        rows =
            Grid.rows grid
                |> Array.map (\r -> Element.row [] <| Array.toList r)
                |> Array.toList
    in
    Element.column [ centerX, centerY ] rows


{-| The initial play ground definition, can be set on start a game. The function adjustGameDefinition adjusts the input values to a valid setup.
A game field should be at least 4 x 4, have ad least 1 mine and not more than the size of the fields - 9

    adjustGameDefinition { dimensionX = 1, dimensionY = 1, amountOfMines = 99 } == { dimensionX = 4, dimensionY = 4, amountOfMines = 7 }

    adjustGameDefinition { dimensionX = 1, dimensionY = 1, amountOfMines = -99 } == { dimensionX = 4, dimensionY = 4, amountOfMines = 1 }

-}
adjustGameDefinition : PlayGroundDefinition -> PlayGroundDefinition
adjustGameDefinition initGameDefinition =
    let
        dimensionX =
            if initGameDefinition.dimensionX < 4 then
                4

            else
                initGameDefinition.dimensionX

        dimensionY =
            if initGameDefinition.dimensionY < 4 then
                4

            else
                initGameDefinition.dimensionY

        fields =
            dimensionX * dimensionY

        maxMines =
            fields - 9

        minMines =
            1

        amountOfMines =
            if initGameDefinition.amountOfMines < minMines then
                minMines

            else if initGameDefinition.amountOfMines > maxMines then
                maxMines

            else
                initGameDefinition.amountOfMines
    in
    { dimensionX = dimensionX, dimensionY = dimensionY, amountOfMines = amountOfMines }


emptyGameGridToStart : PlayGroundDefinition -> Grid Cell
emptyGameGridToStart playgroundDefinition =
    Grid.initialize playgroundDefinition.dimensionX playgroundDefinition.dimensionY (\x y -> { cellType = EmptyCell, status = Closed, coordinate = { rowIndex = x, columnIndex = y } })


initialGameClickToStart : Seed -> CellCoordinates -> PlayGroundDefinition -> Grid Cell
initialGameClickToStart seed clickedCell playGroundDefinition =
    Debug.todo "Implement"


mineIndexGenerator : Seed -> CellCoordinates -> PlayGroundDefinition -> List CellCoordinates
mineIndexGenerator seed initialCell playgroundDefinition =
    let
        randomIntGenerator : Generator Int
        randomIntGenerator =
            Random.int 0 (playgroundDefinition.dimensionY * playgroundDefinition.dimensionX - 1)

        disallowedIndexes : Set Int
        disallowedIndexes =
            initialClickOpenedFields initialCell playgroundDefinition |> List.map (coordinatesToListIndex playgroundDefinition) |> Set.fromList

        alreadyOccupiedIndices =
            Set.union disallowedIndexes

        generateMine : Seed -> Set Int -> ( Int, Seed )
        generateMine currentSeed alreadyAMine =
            case Random.step randomIntGenerator seed of
                ( n, s ) ->
                    if Set.member n <| alreadyOccupiedIndices alreadyAMine then
                        generateMine s alreadyAMine

                    else
                        ( n, s )

        generateMines : Seed -> Set Int -> Set Int
        generateMines currentSeed currentMines =
            case playgroundDefinition.amountOfMines - Set.size currentMines of
                0 ->
                    currentMines

                _ ->
                    generateMine currentSeed currentMines |> (\( mineIdx, nextSeed ) -> Set.insert mineIdx currentMines |> (\nextMines -> generateMines nextSeed nextMines))
    in
    generateMines seed Set.empty |> Set.toList |> List.map (listIndexToCoordinates playgroundDefinition)


initialClickOpenedFields : CellCoordinates -> PlayGroundDefinition -> List CellCoordinates
initialClickOpenedFields initialCell playgroundDefinition =
    let
        openCellsMinRowIndex =
            max 0 (initialCell.rowIndex - 1)

        openCellsMaxRowIndex =
            min (playgroundDefinition.dimensionY - 1) (initialCell.rowIndex + 1)

        openCellsMinColIndex =
            max 0 (initialCell.columnIndex - 1)

        openCellsMaxColIndex =
            min (playgroundDefinition.dimensionX - 1) (initialCell.columnIndex + 1)
    in
    List.range openCellsMinRowIndex openCellsMaxRowIndex
        |> List.concatMap
            (\rowIndex ->
                List.range openCellsMinColIndex openCellsMaxColIndex
                    |> List.map
                        (\colIndex ->
                            { rowIndex = rowIndex, columnIndex = colIndex }
                        )
            )


listIndexToCoordinates : PlayGroundDefinition -> Int -> CellCoordinates
listIndexToCoordinates definition listIndex =
    { rowIndex = listIndex // definition.dimensionY - 1, columnIndex = modBy definition.dimensionX listIndex }


coordinatesToListIndex : PlayGroundDefinition -> CellCoordinates -> Int
coordinatesToListIndex definition coordinate =
    definition.dimensionY * coordinate.rowIndex + coordinate.columnIndex


interactWithGame : GameUpdateMsg -> Model -> GameStatus
interactWithGame updateMsg model =
    case updateMsg of
        TogglePause ->
            togglePause model.gameStatus

        CreateGame definition ->
            adjustGameDefinition definition |> InitGame model.seed

        ClickCell coordinates ->
            clickOnCell model.gameStatus coordinates

        CellClickMode mode ->
            setCellClickInteractionMode mode model.gameStatus

        Tick time ->
            updateTime model.gameStatus time

        CustomGameDefinition ->
            ViewCustomGameDefinitionSetup


setCellClickInteractionMode : MouseClickMode -> GameStatus -> GameStatus
setCellClickInteractionMode mode gameStatus =
    case gameStatus of
        PausedGame gameModel ->
            PausedGame { gameModel | mode = mode }

        RunningGame gameModel ->
            RunningGame { gameModel | mode = mode }

        others ->
            others


togglePause : GameStatus -> GameStatus
togglePause gameStatus =
    case gameStatus of
        RunningGame gameModel ->
            PausedGame gameModel

        PausedGame gameModel ->
            RunningGame gameModel

        others ->
            others


clickOnCell : GameStatus -> CellCoordinates -> GameStatus
clickOnCell gameStatus cellCoordinates =
    case gameStatus of
        RunningGame gameModel ->
            Debug.todo "Implement cell handling"

        InitGame playgroundDefinition seed ->
            Debug.todo "Start game"

        noOp ->
            noOp


updateTime : GameStatus -> Time.Posix -> GameStatus
updateTime gameStatus time =
    case gameStatus of
        RunningGame gameModel ->
            Debug.todo "Update tick"

        other ->
            other




icons =
    { markerFlag = "⚑"
    , untouchedBomb = "💣"
    , exploded = "💥"
    , stopWatch = "⏱️"
    }


basicMineCellStyle : List (Element.Attribute msg)
basicMineCellStyle =
    [ Element.width <| Element.px 40
    , Element.height <| Element.px 40
    , Background.color Color.grey_lighter
    , Border.color Color.grey
    , Border.solid
    , Border.width 1
    ]


emptyCellStyle : List (Element.Attribute msg)
emptyCellStyle =
    basicMineCellStyle ++ [ Background.color Color.white ]


{-| Nothing to say
-}
type Msg
    = GameMsg GameUpdateMsg
    | ChangeScreen ActiveScreen
    | NextTick Time.Posix
    | UpdateSeed Int


type alias Model =
    { gameStatus : GameStatus
    , activeSceen : ActiveScreen
    , seed : Seed
    }


type GameStatus
    = NoGame
    | ViewCustomGameDefinitionSetup
    | InitGame Seed PlayGroundDefinition
    | RunningGame GameModel
    | FinishedGame FinishStatus GameModel
    | PausedGame GameModel


{-| All messages for anything within the game board. Represents messages for cell clicks, creation of games, running a game etc.
-}
type GameUpdateMsg
    = CreateGame PlayGroundDefinition
    | CustomGameDefinition
    | ClickCell CellCoordinates
    | TogglePause
    | CellClickMode MouseClickMode
    | Tick Time.Posix


type MouseClickMode
    = OpenCell
    | FlagCell


type ActiveScreen
    = GameScreen
    | GameHistoryScreen
    | AboutScreen


type alias PlayGroundDefinition =
    { dimensionX : Int
    , dimensionY : Int
    , amountOfMines : Int
    }


type alias CellCoordinates =
    { rowIndex : Int
    , columnIndex : Int
    }


{-| Represents if the game has ben won or lost and how long it took to win or loose the game.
-}
type FinishStatus
    = Won GameDurationInSeconds
    | Lost GameDurationInSeconds


type alias GameModel =
    { board : Grid Cell
    , elapsedTime : Int
    , mode : MouseClickMode
    }


type alias Cell =
    { cellType : GameCellType
    , status : GameCellStatus
    , coordinate : CellCoordinates
    }


type GameCellType
    = Mine
    | EmptyCell
    | MinesAround Int


type GameCellStatus
    = Closed
    | Marked
    | Open


type alias GameDurationInSeconds =
    Int
