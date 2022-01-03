module GameTests exposing (..)

import Expect
import Fuzz exposing (intRange)
import Game.Game as Game
import Game.Internal as GameInternal
import Grid
import Set exposing (Set)
import Test exposing (..)
import Types exposing (..)


all : Test
all =
    describe "Check generation of a playgrid is reasonable"
        [ test "the list of all possible indizes is correct at index 0 0" <|
            \_ ->
                let
                    coords =
                        { x = 0, y = 0 }

                    width =
                        16

                    height =
                        30

                    openingArea =
                        calculateSurroundingAreaFields coords width height

                    filteredAllowedIndizes =
                        List.range 0 (width * height - 1)
                            |> List.filter (\i -> not (Set.member i openingArea))
                in
                GameInternal.generateListOfPossibleIndizes (Grid.repeat width height InitGameCell) coords
                    |> Expect.equal filteredAllowedIndizes
        , fuzz2
            (intRange 0 15)
            (intRange 0 29)
            "the list of all possible indizes in a 16 x 30 grid for fuzzed coordinates click must be correct"
          <|
            \x y ->
                let
                    coords =
                        { x = x, y = y }

                    width =
                        16

                    height =
                        30

                    openingArea =
                        calculateSurroundingAreaFields coords width height

                    filteredAllowedIndizes =
                        List.range 0 (width * height - 1)
                            |> List.filter (\i -> not (Set.member i openingArea))
                in
                GameInternal.generateListOfPossibleIndizes (Grid.repeat width height InitGameCell) coords
                    |> Expect.equal filteredAllowedIndizes
        ]


calculateSurroundingAreaFields : Coordinates -> Int -> Int -> Set Int
calculateSurroundingAreaFields coords width height =
    List.range (max 0 (coords.x - 1)) (min (width - 1) (coords.x + 1))
        |> List.concatMap (\x -> List.map (\y -> y * width + x) <| List.range (max 0 (coords.y - 1)) (min (height - 1) (coords.y + 1)))
        |> Set.fromList


decoderTests : Test
decoderTests =
    describe "Test the decoding of finished game history"
        [ test "Empty list should work" <|
            \_ -> Game.decodeStoredFinishedGameHistory "[]" |> Expect.equal []
        , test "Bulshit data should just return an empty list" <|
            \_ -> Game.decodeStoredFinishedGameHistory "asdf" |> Expect.equal []
        , test "A bit more advanced list should work" <|
            \_ ->
                case Tuple.second extendedGridValue of
                    Nothing ->
                        Expect.fail "Testdata seems to be invalid"

                    Just x ->
                        Game.decodeStoredFinishedGameHistory (Tuple.first extendedGridValue) |> Expect.equal x
        ]


extendedGridValue =
    ( """
    [
        { "grid":
            [ [ {"cellType":"mineNeighbourCell","minesOnNeighbourCell":1,"cellStatus":"opened"}
              , {"cellType":"mineNeighbourCell","minesOnNeighbourCell":1,"cellStatus":"flagged"}
              ]
            , [ {"cellType":"mineNeighbourCell","minesOnNeighbourCell":1,"cellStatus":"untouched"}
              , {"cellType":"mineCell","minesOnNeighbourCell":null,"cellStatus":"opened"}
              ]
            ]
        , "result":"lost"
        , "time":1000
        }
    ]
"""
        |> String.replace "\t" ""
        |> String.replace "\n" ""
        |> String.replace " " ""
    , Maybe.map (\grid -> [ FinishedGameHistoryEntry grid Lost 1000 ]) <|
        Grid.fromList
            [ [ GameCell (MineNeighbourCell 1) Opened, GameCell (MineNeighbourCell 1) Flagged ]
            , [ GameCell (MineNeighbourCell 1) Untouched, GameCell MineCell Opened ]
            ]
    )
