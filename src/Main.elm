module Main exposing (..)

import Array
import Browser
import Element exposing (Element, fill)
import Element.Events as Events
import Grid exposing (Grid)
import Html exposing (Html)
import Html.Attributes exposing (coords)
import Random exposing (Generator)
import Set exposing (Set)
import Styles exposing (..)
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
        Msg ->
            ( model, Cmd.none )

        ClickedOnInitGameCell initGame coords ->
            ( model, Random.generate StartGame <| generatePlayGameGrid initGame coords )

        StartGame playGrid ->
            ( { model | gameBoardStatus = RunningGame playGrid }, Cmd.none )


init : Int -> ( Model, Cmd Msg )
init _ =
    ( { gameBoardStatus = WaitOnStart <| createInitGameGrid smallPlayground }, Cmd.none )


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
            , selectBoardView m.gameBoardStatus
            ]


selectBoardView : GameBoardStatus -> Element Msg
selectBoardView status =
    case status of
        WaitOnStart initGameGrid ->
            initGameGridView initGameGrid

        _ ->
            Maybe.withDefault (Element.text "Hubsi") Nothing


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



--- HELPER ---


createInitGameGrid : PlayGroundDefinition -> InitGameGrid
createInitGameGrid definition =
    { grid = Grid.repeat definition.cols definition.rows InitGameCell
    , mines = definition.mines
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


openCell : Coordinates -> PlayGameGrid -> PlayGameGrid
openCell coords playGrid =
    let
        coordinateAsPair =
            ( coords.x, coords.y )

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
                        |> (\g -> List.foldl (\coord grid -> openCell coord grid) g surroundingCoordinatesAsPair)

                ( EmptyCell, _ ) ->
                    Grid.set coordinateAsPair (GameCell EmptyCell Opened) playGrid

                ( MineCell, _ ) ->
                    Grid.set coordinateAsPair (GameCell MineCell Opened) playGrid


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
