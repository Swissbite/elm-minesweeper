module Main exposing (..)

import Array
import Browser
import Element exposing (Element, fill)
import Element.Events as Events
import Element.Input as Input
import Element.Background as Background
import Element.Border as Border
import Grid exposing (Grid)
import Html exposing (Html)
import Html.Attributes exposing (coords)
import Random exposing (Generator)
import Set exposing (Set)
import Styles
import Types exposing (..)



--- PROGRAM ---


main : Program Int Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }



--- UPDATE / INIT / SUBSCRIPTIONS ---


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedOnInitGameCell initGame coords ->
            ( model, generatePlayGameGrid initGame coords |> Random.generate StartGame )

        StartGame playGrid ->
            ( { model | gameBoardStatus = RunningGame playGrid }, Cmd.none )

        ClickOnGameCell coords ->
            case ( model.gameInteractionMode, model.gameBoardStatus ) of
                ( Reveal, RunningGame playGrid ) ->
                    ( { model | gameBoardStatus = gameBoardStatus (openCell coords playGrid) }, Cmd.none )

                ( Flag, RunningGame playGrid ) ->
                    ( { model | gameBoardStatus = gameBoardStatus (flagCell coords playGrid) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

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
        CreateNewGame playgroundDefinition ->
            ({ gameBoardStatus = WaitOnStart <| createInitGameGrid playgroundDefinition, gameInteractionMode = Reveal }, Cmd.none)


init : Int -> ( Model, Cmd Msg )
init _ =
    ( { gameBoardStatus = WaitOnStart <| createInitGameGrid smallPlayground, gameInteractionMode = Reveal }, Cmd.none )


smallPlayground : PlayGroundDefinition
smallPlayground =
    { cols = 8
    , rows = 8
    , mines = 10
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--- VIEW ---


view : Model -> Html Msg
view m =
    Element.layout [ Element.width Element.fill, Element.height Element.fill ] <|
        Element.column [ Element.width fill, Element.height fill, Element.centerX ]
            [ Element.el [ Element.centerX ] <| Element.text "Minesweeper"
            , selectBoardView m.gameBoardStatus m.gameInteractionMode
            ]


selectBoardView : GameBoardStatus -> CellClickMode -> Element Msg
selectBoardView status gameInteractionMode =
    case status of
        WaitOnStart initGameGrid ->
            Element.column [ Element.width fill, Element.height fill ]
                [ dummyToogleElement
                , initGameGridView initGameGrid
                ]

        RunningGame playGrid ->
            Element.column [ Element.width fill, Element.height fill ]
                [ mineToggleElement gameInteractionMode
                , runningGameView playGrid
                ]
        FinishedGame playGrid finishedStatus ->
            finishedGameView playGrid finishedStatus


dummyToogleElement: Element Msg
dummyToogleElement = Element.el [ Element.centerX, Element.centerY, Element.paddingXY 0 10 ] <| styledToogleElement False

styledToogleElement: Bool -> Element Msg
styledToogleElement = Styles.toggleCheckboxWidget
                    { offColor = Styles.lightGrey
                    , onColor = Styles.green
                    , sliderColor = Styles.white
                    , toggleWidth = 60
                    , toggleHeight = 28
                    , onSymbol = Just Styles.icons.untouchedBomb
                    , offSymbol = Just Styles.icons.markerFlag
                    }
mineToggleElement : CellClickMode -> Element Msg
mineToggleElement gameInteractionMode =
    Element.el [ Element.centerX, Element.centerY, Element.paddingXY 0 10 ] <|
        Input.checkbox [ Element.centerX, Element.centerY] <|
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


initGameGridView : InitGameGrid -> Element Msg
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


initGameCellToElement : InitGameGrid -> (Int -> Int -> InitGameCell -> Element Msg)
initGameCellToElement initGameGrid =
    \x y _ ->
        let
            coords =
                Coordinates x y
        in
        Element.el (Styles.untouchedCellStyle ++ [ Events.onClick <| ClickedOnInitGameCell initGameGrid coords ]) <| Element.text ""


runningGameView : PlayGameGrid -> Element Msg
runningGameView playGameGrid =
    playGameGrid
        |> Grid.indexedMap runningGameCellToElement
        |> Grid.rows
        |> Array.map Array.toList
        |> Array.map (\l -> Element.row [] l)
        |> Array.toList
        |> Element.column [ Element.centerX, Element.centerY ]


runningGameCellToElement : Int -> Int -> GameCell -> Element Msg
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
            Element.el (Styles.openedMineNeighbourCellStyle neighbours) <| Element.el [ Element.centerX, Element.centerY ] <| Element.text (String.fromInt neighbours)


finishedGameView: PlayGameGrid -> GameResult -> Element Msg
finishedGameView playGameGrid gameResult =
    Element.column [ Element.centerX, Element.centerY ] [
        Element.el [Element.centerX, Element.centerY, Element.paddingXY 0 10] <|
        case gameResult of
            Won ->
                Element.text "You won!"

            Lost ->
                Element.text "You lost!"
        , Element.row [Element.centerX, Element.centerY, Element.spacing 20, Border.solid, Border.rounded 25, Element.paddingXY 0 10] 
            [ Input.button [Background.color Styles.asparagus, Border.solid, Element.padding 10, Border.rounded 10] {
                onPress  = Just (CreateNewGame <| playGameGridToPlaygroundDefinition playGameGrid)
                , label = Element.text "Start new game"
                }
            , Input.button [Background.color Styles.saffron, Border.solid, Element.padding 10, Border.rounded 10] {
                onPress  = Nothing
                , label = Element.text "Back to overview"
                }
            
        ]
        , finishedGridToView playGameGrid
        ]
    

finishedGridToView: PlayGameGrid -> Element Msg
finishedGridToView playGameGrid =
    playGameGrid
        |> Grid.map finishedGameCellToElement
        |> Grid.rows
        |> Array.map Array.toList
        |> Array.map (\l -> Element.row [] l)
        |> Array.toList
        |> Element.column [ Element.centerX, Element.centerY ]
finishedGameCellToElement: GameCell -> Element Msg
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
        
        _ -> Element.el Styles.untouchedCellStyle Element.none

--- HELPER ---


createInitGameGrid : PlayGroundDefinition -> InitGameGrid
createInitGameGrid definition =
    let 
        sanitized = sanitizePlaygroundDefinition definition
    in 
    { grid = Grid.repeat sanitized.cols sanitized.rows InitGameCell
    , mines = sanitized.mines
    }

playGameGridToPlaygroundDefinition: PlayGameGrid -> PlayGroundDefinition
playGameGridToPlaygroundDefinition grid =
    let
        foldLFn: GameCell -> Int -> Int
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

sanitizePlaygroundDefinition: PlayGroundDefinition -> PlayGroundDefinition
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

        surroundingCoordinatesAsPair =
            [ { x = coords.x - 1, y = coords.y - 1 }
            , { x = coords.x - 1, y = coords.y }
            , { x = coords.x - 1, y = coords.y + 1 }
            , { x = coords.x, y = coords.y - 1 }
            , { x = coords.x, y = coords.y + 1 }
            , { x = coords.x + 1, y = coords.y - 1 }
            , { x = coords.x + 1, y = coords.y }
            , { x = coords.x + 1, y = coords.y + 1 }
            ]
    in
    case cell of
        Nothing ->
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
                        |> (\g -> List.foldl (\coord grid -> openCell coord grid) g surroundingCoordinatesAsPair)

                ( MineCell, _ ) ->
                    Grid.set coordinateAsPair (GameCell MineCell Opened) playGrid


gameBoardStatus : PlayGameGrid -> GameBoardStatus
gameBoardStatus playGrid =
    let
        isExplodedMine : GameCell -> Bool -> Bool
        isExplodedMine cell exploded =
            case cell of
                GameCell MineCell Opened ->
                    True

                _ ->
                    exploded

        hasAnExplodedMine =
            Grid.foldl isExplodedMine False playGrid

        isMissingFildToOpen : GameCell -> Bool -> Bool
        isMissingFildToOpen cell anotherfound =
            case cell of
                GameCell EmptyCell state ->
                    case state of
                        Opened ->
                            anotherfound

                        _ ->
                            True

                GameCell (MineNeighbourCell _) state ->
                    case state of
                        Opened ->
                            anotherfound

                        _ ->
                            True

                _ ->
                    anotherfound

        hasRemainingFields =
            Grid.foldl isMissingFildToOpen False playGrid
    in
    if hasAnExplodedMine then
        FinishedGame playGrid Lost

    else if not hasRemainingFields then
        FinishedGame playGrid Won

    else
        RunningGame playGrid


generateListOfPossibleIndizes : Grid InitGameCell -> Coordinates -> List Int
generateListOfPossibleIndizes initGrid clickedOn =
    let
        gridHeight =
            Grid.height initGrid

        foldFn : Maybe Int -> List Int -> List Int
        foldFn x acc =
            case x of
                Nothing ->
                    acc

                Just idx ->
                    idx :: acc
    in
    Grid.indexedMap
        (\x y _ ->
            if x == clickedOn.x && y == clickedOn.y then
                Nothing

            else
                Just (x + gridHeight * y)
        )
        initGrid
        |> Grid.foldr foldFn []


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
