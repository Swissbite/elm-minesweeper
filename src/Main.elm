module Main exposing (..)

--import Styles exposing (..)

import Browser
import Element exposing (Element)
import Grid
import Html exposing (Html)
import Random exposing (Generator)
import Set exposing (Set)
import Styles exposing (..)
import Types exposing (..)
import Element exposing (fill)
import Grid exposing (initialize)



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

        ClickedOnInitGameCell def coords ->
            ( model, Cmd.none )

        StartGame playGrid ->
            ( model, Cmd.none )


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
    Element.layout [ Element.width Element.fill, Element.height Element.fill ] <| Element.column [Element.width fill, Element.centerX] [
        Element.el [Element.centerX] <| Element.text "Minesweeper"
    ,   Maybe.withDefault (Element.text "Hubsi") Nothing --(Just (Element.el [Element.centerX] <| Element.text "Minesweeper")) 
    ]

initGameGridView: InitGameGrid -> Element Msg
initGameGridView initGameGrid =
    Element.column [Element.width fill, Element.height fill, Element.centerX, Element.centerY] []

--- HELPER ---


createInitGameGrid : PlayGroundDefinition -> InitGameGrid
createInitGameGrid definition =
    { grid = Grid.repeat definition.cols definition.rows InitGameCell
    , mines = definition.mines
    }


generateListOfPossibleIndizes : InitGameGrid -> Coordinates -> List Int
generateListOfPossibleIndizes initGameGrid clickedOn =
    let
        initGrid =
            initGameGrid.grid

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


appendGenerator : Generator Int -> Generator (Set Int) -> Generator (Set Int)
appendGenerator single list =
    Random.map2 (\toAdd set -> Set.insert toAdd set) single list


singleMineIndexGenerator : Int -> List Int -> Generator Int
singleMineIndexGenerator head restOfPossibleIdx =
    Random.uniform head restOfPossibleIdx


emptyListGenerator : Generator (Set Int)
emptyListGenerator =
    Random.constant Set.empty


amountOfNeighboursToInt : AmountOfNeighbours -> Int
amountOfNeighboursToInt neighbours =
    case neighbours of
        OneMine ->
            1

        TwoMines ->
            2

        ThreeMines ->
            3

        FourMines ->
            4

        FiveMines ->
            5

        SixMines ->
            6

        SevenMines ->
            7

        EightMines ->
            8


intToAmountOfNeighbours : Int -> Result String AmountOfNeighbours
intToAmountOfNeighbours mines =
    case mines of
        1 ->
            Ok OneMine

        2 ->
            Ok TwoMines

        3 ->
            Ok ThreeMines

        4 ->
            Ok FourMines

        5 ->
            Ok FiveMines

        6 ->
            Ok SixMines

        7 ->
            Ok SevenMines

        8 ->
            Ok EightMines

        _ ->
            Err "Amount of mines must be between 1 and 8"
