module Game.Game exposing (decodeStoredFinishedGameHistory, initModel, subscriptions, update, view)

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
import Json.Decode as Decode
import Random as Random exposing (Generator)
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
subscriptions _ =
    Time.every 500 (\posix -> ClockTick posix)



----- UPDATE -----


update : GameMsg -> Model -> ( Model, Cmd GameMsg )
update gameMsg model =
    case gameMsg of
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
            let
                gameModel =
                    model.game
            in
            case ( gameModel.gameBoardStatus, gameModel.gamePauseResumeState ) of
                ( RunningGame _, Paused ) ->
                    ( { model | game = { gameModel | gamePauseResumeState = Resumed (List.length gameModel.gameRunningTimes + 1) } }, Cmd.none )

                ( RunningGame _, Resumed _ ) ->
                    ( { model | game = { gameModel | gamePauseResumeState = Paused } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ClockTick posix ->
            updateTimePlayGame model posix


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
            ( { model | game = { gameModel | gameRunningTimes = newList } }, Cmd.none )

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
    case model.game.gameBoardStatus of
        NoGame _ ->
            Element.column [ Element.width Element.fill, Element.height Element.fill, Element.spacing 10 ]
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
                [ Lazy.lazy mineToggleElement model.game.gameInteractionMode
                , Lazy.lazy runningGameView playGrid
                ]

        FinishedGame playGrid finishedStatus _ ->
            Lazy.lazy2 finishedGameView playGrid finishedStatus


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
