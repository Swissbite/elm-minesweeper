module Game.Game exposing (decodeStoredFinishedGameHistory, subscriptions, update, view)

{-| Game module for rendering the complete game, as long as the currentView in the model is set to Game.
Exposes the basic update / view / subscription functions, so that Main.elm can use them.
-}

import Array as Array
import Colors
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Element.Lazy as Lazy
import Game.Internal exposing (..)
import Grid
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Ports
import Random as Random exposing (Generator)
import Set exposing (Set)
import Styles exposing (..)
import Time
import Types exposing (..)



----- Subscription -----


decodeStoredFinishedGameHistory : String -> List FinishedGameHistoryEntry
decodeStoredFinishedGameHistory string =
    Decode.decodeString deocdeFinishedGameHistory string
        |> Result.withDefault []


subscriptions : Model -> Sub GameMsg
subscriptions _ =
    Time.every 500 (\posix -> ClockTick posix)



----- UPDATE -----


update : GameMsg -> Model -> ( Model, Cmd GameMsg )
update gameMsg model =
    case gameMsg of
        GoToStartPage ->
            ( { model | gameBoardStatus = NoGame PreSelect }, Cmd.none )

        ClickedOnInitGameCell initGame coords ->
            ( model, generatePlayGameGrid initGame coords |> Random.generate StartGame )

        StartGame playGrid ->
            ( { model | gameBoardStatus = RunningGame playGrid, gameRunningTimes = [], gamePauseResumeState = Resumed 1 }, Cmd.none )

        CreateNewGame playgroundDefinition ->
            ( { model | gameBoardStatus = WaitOnStart <| createInitGameGrid playgroundDefinition, gameInteractionMode = Reveal }, Cmd.none )

        ClickOnGameCell coords ->
            updateModelByClickOnGameCell coords model

        ToogleGameCellInteractionMode ->
            let
                nextMode =
                    case model.gameInteractionMode of
                        Reveal ->
                            Flag

                        Flag ->
                            Reveal
            in
            ( { model | gameInteractionMode = nextMode }, Cmd.none )

        ToogleGamePause ->
            case ( model.gameBoardStatus, model.gamePauseResumeState ) of
                ( RunningGame _, Paused ) ->
                    ( { model | gamePauseResumeState = Resumed (List.length model.gameRunningTimes + 1) }, Cmd.none )

                ( RunningGame _, Resumed _ ) ->
                    ( { model | gamePauseResumeState = Paused }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ClockTick posix ->
            updateTimePlayGame model posix


updateTimePlayGame : Model -> Time.Posix -> ( Model, Cmd GameMsg )
updateTimePlayGame model time =
    case ( model.gameBoardStatus, model.gamePauseResumeState ) of
        ( RunningGame _, Resumed timesResume ) ->
            let
                shouldReplaceHead =
                    List.length model.gameRunningTimes == timesResume

                newList =
                    if shouldReplaceHead then
                        case model.gameRunningTimes of
                            [] ->
                                [ ( time, time ) ]

                            ( start, _ ) :: xs ->
                                ( start, time ) :: xs

                    else
                        ( time, time ) :: model.gameRunningTimes
            in
            ( { model | gameRunningTimes = newList }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateModelByClickOnGameCell : Coordinates -> Model -> ( Model, Cmd GameMsg )
updateModelByClickOnGameCell coords model =
    case ( model.gamePauseResumeState, model.gameBoardStatus ) of
        ( Resumed _, RunningGame playGrid ) ->
            let
                updatedPlayGrid : PlayGameGrid
                updatedPlayGrid =
                    case model.gameInteractionMode of
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
                        calculateElapsedTimeMilis model.gameRunningTimes |> FinishedGame updatedPlayGrid Lost

                    else if allFieldsRevealed then
                        calculateElapsedTimeMilis model.gameRunningTimes |> FinishedGame updatedPlayGrid Won

                    else
                        RunningGame updatedPlayGrid

                nextHistoryList =
                    case nextGameBoardStatus of
                        FinishedGame grid result time ->
                            FinishedGameHistoryEntry grid result time :: model.playedGameHistory

                        _ ->
                            model.playedGameHistory
            in
            ( { model | gameBoardStatus = nextGameBoardStatus, playedGameHistory = nextHistoryList }, saveFinishedGameHistory nextHistoryList )

        _ ->
            ( model, Cmd.none )


deocdeFinishedGameHistory : Decoder (List FinishedGameHistoryEntry)
deocdeFinishedGameHistory =
    Decode.list decodeFinishedGameHistoryEntry


decodeFinishedGameHistoryEntry : Decoder FinishedGameHistoryEntry
decodeFinishedGameHistoryEntry =
    Decode.map3
        (\grid result time -> FinishedGameHistoryEntry grid result time)
        (Decode.field "grid" decodeGrid)
        (Decode.field "result" decodeResult)
        (Decode.field "time" Decode.int)


decodeGrid : Decoder PlayGameGrid
decodeGrid =
    Decode.list (Decode.list gameCellDecoder)
        |> Decode.andThen
            (\gridAsList ->
                case Grid.fromList gridAsList of
                    Just grid ->
                        Decode.succeed grid

                    Nothing ->
                        Decode.fail "Could not decode grid"
            )


decodeResult : Decoder GameResult
decodeResult =
    Decode.string
        |> Decode.andThen
            (\resultAsString ->
                String.toLower resultAsString
                    |> (\lowerResult ->
                            case lowerResult of
                                "won" ->
                                    Decode.succeed Won

                                "lost" ->
                                    Decode.succeed Lost

                                _ ->
                                    Decode.fail "Unknown game result string"
                       )
            )


saveFinishedGameHistory : List FinishedGameHistoryEntry -> Cmd msg
saveFinishedGameHistory finishedGameHistory =
    Encode.list finishedGameHistoryEntryEncoder finishedGameHistory
        |> Encode.encode 0
        |> Ports.storeFinishedGameHistory


finishedGameHistoryEntryEncoder : FinishedGameHistoryEntry -> Encode.Value
finishedGameHistoryEntryEncoder (FinishedGameHistoryEntry grid result time) =
    let
        gridJson =
            Grid.rows grid
                |> Encode.array (Encode.array gameCellEncoder)

        resultJson =
            case result of
                Won ->
                    Encode.string "won"

                Lost ->
                    Encode.string "lost"

        timeJson =
            Encode.int time
    in
    Encode.object [ ( "grid", gridJson ), ( "result", resultJson ), ( "time", timeJson ) ]


gameCellDecoder : Decoder GameCell
gameCellDecoder =
    let
        singleFieldsToCell : CellType -> Maybe Int -> CellStatus -> Maybe GameCell
        singleFieldsToCell cellType maybeMineCount cellStatus =
            case ( cellType, maybeMineCount ) of
                ( MineCell, _ ) ->
                    Just (GameCell MineCell cellStatus)

                ( EmptyCell, _ ) ->
                    Just (GameCell EmptyCell cellStatus)

                ( MineNeighbourCell _, Just count ) ->
                    Just (GameCell (MineNeighbourCell count) cellStatus)

                _ ->
                    Nothing
    in
    Decode.map3 singleFieldsToCell
        (Decode.field "cellType" decodeCellType)
        (Decode.field "minesOnNeighbourCell" <| Decode.oneOf [ Decode.null Nothing, Decode.map Just Decode.int ])
        (Decode.field "cellStatus" decodeCellStatus)
        |> Decode.andThen
            (\maybeGameCell ->
                case maybeGameCell of
                    Just gameCell ->
                        Decode.succeed gameCell

                    Nothing ->
                        Decode.fail "Could not decode game cell"
            )


decodeCellType : Decoder CellType
decodeCellType =
    Decode.string
        |> Decode.andThen
            (\cellTypeAsString ->
                case cellTypeAsString of
                    "mine" ->
                        Decode.succeed MineCell

                    "mineCell" ->
                        Decode.succeed MineCell

                    "emptyCell" ->
                        Decode.succeed EmptyCell

                    "mineNeighbourCell" ->
                        Decode.succeed (MineNeighbourCell -1)

                    _ ->
                        Decode.fail "Invalid cell type"
            )


decodeCellStatus : Decoder CellStatus
decodeCellStatus =
    Decode.string
        |> Decode.andThen
            (\cellStatusAsString ->
                case String.toLower cellStatusAsString of
                    "untouched" ->
                        Decode.succeed Untouched

                    "flagged" ->
                        Decode.succeed Flagged

                    "opened" ->
                        Decode.succeed Opened

                    _ ->
                        Decode.fail "Invalid cell status"
            )


gameCellEncoder : GameCell -> Encode.Value
gameCellEncoder (GameCell cellType cellStatus) =
    let
        encodedType =
            (case cellType of
                MineCell ->
                    "mineCell"

                MineNeighbourCell _ ->
                    "mineNeighbourCell"

                EmptyCell ->
                    "emptyCell"
            )
                |> Encode.string

        encodedMinesOnNeighbourCell =
            case cellType of
                MineNeighbourCell i ->
                    Encode.int i

                _ ->
                    Encode.null

        encodedCellStatus =
            case cellStatus of
                Untouched ->
                    Encode.string "untouched"

                Flagged ->
                    Encode.string "flagged"

                Opened ->
                    Encode.string "opened"
    in
    Encode.object [ ( "cellType", encodedType ), ( "minesOnNeighbourCell", encodedMinesOnNeighbourCell ), ( "cellStatus", encodedCellStatus ) ]



----- VIEW for Game -----


view : Model -> Element GameMsg
view model =
    case model.gameBoardStatus of
        NoGame _ ->
            Element.row [ Element.width Element.fill, Element.height Element.fill ]
                [ Element.column [ Element.alignLeft, Element.alignTop ]
                    [ Element.text "Game History"
                    , Lazy.lazy finishedGameHistoryList model.playedGameHistory
                    ]
                , Element.column [ Element.width Element.fill, Element.height Element.fill, Element.spacing 10 ]
                    [ Element.row
                        [ Element.centerX, Element.centerY, Element.spacing 10 ]
                        [ Styles.styledGameCelectionButton
                            { onPress = Just (CreateNewGame smallPlayground)
                            , label = Element.text "small"
                            }
                        , Styles.styledGameCelectionButton
                            { onPress = Just (CreateNewGame mediumPlayground)
                            , label = Element.text "medium"
                            }
                        ]
                    , Element.row
                        [ Element.centerX, Element.centerY, Element.spacing 10 ]
                        [ Styles.styledGameCelectionButton
                            { onPress = Just (CreateNewGame advancePlayground)
                            , label = Element.text "advanced"
                            }
                        , Styles.styledGameCelectionButton
                            { onPress = Just (CreateNewGame xxlPlayground)
                            , label = Element.text "xxl"
                            }
                        ]
                    ]
                ]

        WaitOnStart initGameGrid ->
            Lazy.lazy
                (\initGrid ->
                    Element.column [ Element.width Element.fill, Element.height Element.fill ]
                        [ dummyToogleElement
                        , initGameGridView initGrid
                        ]
                )
                initGameGrid

        RunningGame playGrid ->
            Element.column [ Element.width Element.fill, Element.height Element.fill ]
                [ Lazy.lazy mineToggleElement model.gameInteractionMode
                , Lazy.lazy runningGameView playGrid
                ]

        FinishedGame playGrid finishedStatus _ ->
            Lazy.lazy2 finishedGameView playGrid finishedStatus


finishedGameHistoryList : List FinishedGameHistoryEntry -> Element GameMsg
finishedGameHistoryList finishedGameHistory =
    let
        gameStatistics : FinishedGameHistoryEntry -> { result : GameResult, playedSeconds : String, playedMinutes : String, gridHeight : Int, gridWidth : Int, mines : Int }
        gameStatistics entry =
            { result =
                case entry of
                    FinishedGameHistoryEntry _ res _ ->
                        res
            , playedSeconds =
                case entry of
                    FinishedGameHistoryEntry _ _ time ->
                        time
                            // 1000
                            |> modBy 60
                            |> (\s ->
                                    if s < 10 then
                                        String.concat [ "0", String.fromInt s ]

                                    else
                                        String.fromInt s
                               )
            , playedMinutes =
                case entry of
                    FinishedGameHistoryEntry _ _ time ->
                        time
                            // 1000
                            // 60
                            |> String.fromInt
            , gridHeight =
                case entry of
                    FinishedGameHistoryEntry grid _ _ ->
                        Grid.height grid
            , gridWidth =
                case entry of
                    FinishedGameHistoryEntry grid _ _ ->
                        Grid.width grid
            , mines =
                case entry of
                    FinishedGameHistoryEntry grid _ _ ->
                        Grid.foldl
                            (\cell count ->
                                case cell of
                                    GameCell MineCell _ ->
                                        count + 1

                                    _ ->
                                        count
                            )
                            0
                            grid
            }
    in
    case finishedGameHistory of
        [] ->
            Element.el [ Element.paddingXY 0 30 ] <| Element.text "No games played yet"

        _ ->
            Element.table [ Element.spacing 10, Element.paddingXY 0 30 ]
                { data = List.map gameStatistics finishedGameHistory
                , columns =
                    [ { header = Element.text <| String.fromList [ Styles.icons.victory, '/', Styles.icons.exploded ]
                      , width = Element.fillPortion 1
                      , view =
                            \entry ->
                                case entry.result of
                                    Won ->
                                        Element.text <| String.fromChar Styles.icons.victory

                                    Lost ->
                                        Element.text <| String.fromChar Styles.icons.exploded
                      }
                    , { header = Element.text "↔️"
                      , width = Element.fillPortion 2
                      , view =
                            \entry -> Element.text <| String.fromInt entry.gridWidth
                      }
                    , { header = Element.text "↕️"
                      , width = Element.fillPortion 2
                      , view =
                            \entry -> Element.text <| String.fromInt entry.gridHeight
                      }
                    , { header = Element.text <| String.fromChar Styles.icons.untouchedBomb
                      , width = Element.fillPortion 2
                      , view =
                            \entry -> Element.text <| String.fromInt entry.mines
                      }
                    , { header = Element.text <| String.fromChar Styles.icons.stopWatch
                      , width = Element.fillPortion 4
                      , view =
                            \entry ->
                                Element.text <| String.concat [ entry.playedMinutes, ":", entry.playedSeconds ]
                      }
                    ]
                }


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


dummyToogleElement : Element GameMsg
dummyToogleElement =
    Element.el [ Element.centerX, Element.centerY, Element.paddingXY 0 10 ] <| styledToogleElement False


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


initGameGridView : InitGameGrid -> Element GameMsg
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


initGameCellToElement : InitGameGrid -> (Int -> Int -> InitGameCell -> Element GameMsg)
initGameCellToElement initGameGrid =
    \x y _ ->
        let
            coords =
                Coordinates x y
        in
        Element.el (Styles.untouchedCellStyle ++ [ Events.onClick <| ClickedOnInitGameCell initGameGrid coords ]) <| Element.text ""


runningGameView : PlayGameGrid -> Element GameMsg
runningGameView playGameGrid =
    playGameGrid
        |> Grid.indexedMap runningGameCellToElement
        |> Grid.rows
        |> Array.map Array.toList
        |> Array.map (\l -> Element.row [] l)
        |> Array.toList
        |> Element.column [ Element.centerX, Element.centerY ]


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
finishedGameView playGameGrid gameResult =
    Element.column [ Element.centerX, Element.centerY ]
        [ Element.el [ Element.centerX, Element.centerY, Element.paddingXY 0 10 ] <|
            case gameResult of
                Won ->
                    Element.text "You won!"

                Lost ->
                    Element.text "You lost!"
        , Element.row [ Element.centerX, Element.centerY, Element.spacing 20, Border.solid, Border.rounded 25, Element.paddingXY 0 10 ]
            [ Input.button [ Background.color Colors.asparagus, Border.solid, Element.padding 10, Border.rounded 10 ]
                { onPress = Just (CreateNewGame <| playGameGridToPlaygroundDefinition playGameGrid)
                , label = Element.text "Start new game"
                }
            , Input.button [ Background.color Colors.saffron, Border.solid, Element.padding 10, Border.rounded 10 ]
                { onPress = Just GoToStartPage
                , label = Element.text "Back to overview"
                }
            ]
        , finishedGridToView playGameGrid
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


createInitGameGrid : PlayGroundDefinition -> InitGameGrid
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
generatePlayGameGrid : InitGameGrid -> Coordinates -> Generator PlayGameGrid
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
