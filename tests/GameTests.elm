module GameTests exposing (..)

import Expect
import Game.Game as Game
import Game.Internal as GameInternal
import Grid
import Test exposing (..)
import Types exposing (..)


all : Test
all =
    describe "Check generation of a playgrid is reasonable"
        [ test "the list of all possible indizes is correct" <|
            \_ ->
                GameInternal.generateListOfPossibleIndizes (Grid.repeat 16 30 InitGameCell) { x = 0, y = 0 }
                    |> Expect.equal (List.range 1 (16 * 30 - 1))
        ]


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
