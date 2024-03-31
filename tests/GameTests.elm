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


module GameTests exposing (..)

import Expect
import Fuzz exposing (intRange)
import Game.Game as Game
import Game.Internal as GameInternal
import Grid
import Set exposing (Set)
import Test exposing (..)
import Time
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
        , test "Upgrade from version 0 to current version should work" <|
            \_ ->
                case Tuple.second extendedGridValue of
                    Nothing ->
                        Expect.fail "Testdata seems to be invalid"

                    Just x ->
                        Game.decodeStoredFinishedGameHistory (Tuple.first extendedGridValue) |> Expect.equal x
        ]


extendedGridValue : ( String, Maybe (List FinishedGameHistoryEntry) )
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
    , Maybe.map (\grid -> [ FinishedGameHistoryEntry grid Lost 1000 (Time.millisToPosix 0) ]) <|
        Grid.fromList
            [ [ GameCell (MineNeighbourCell 1) Opened, GameCell (MineNeighbourCell 1) Flagged ]
            , [ GameCell (MineNeighbourCell 1) Untouched, GameCell MineCell Opened ]
            ]
    )


gameHistoryEncoderTest : Test
gameHistoryEncoderTest =
    describe "Testing the encoding to JSON String"
        [ test "Empty entries should generate correct uptodate version" <|
            \_ -> GameInternal.encodeFinishedGameHistory (Tuple.first emptyHistoryToEncodedVersion01) |> Expect.equal (Tuple.second emptyHistoryToEncodedVersion01)
        ]


emptyHistoryToEncodedVersion01 : ( List FinishedGameHistoryEntry, String )
emptyHistoryToEncodedVersion01 =
    ( []
    , """
    { "version": 1, "entries": [] }
    """
        |> String.replace "\t" ""
        |> String.replace "\n" ""
        |> String.replace " " ""
    )


severalWonLostEntriesEncodedVersion01 : String
severalWonLostEntriesEncodedVersion01 =
    """
    {
        "version": 1,
        "entries": [
            {
                "grid": [
                    [
                        {
                            "cellType": "mineNeighbourCell",
                            "minesOnNeighbourCell": 1,
                            "cellStatus": "opened"
                        },
                        {
                            "cellType": "mineNeighbourCell",
                            "minesOnNeighbourCell": 1,
                            "cellStatus": "flagged"
                        }
                    ],
                    [
                        {
                            "cellType": "mineNeighbourCell",
                            "minesOnNeighbourCell": 1,
                            "cellStatus": "untouched"
                        },
                        {
                            "cellType": "mineCell",
                            "minesOnNeighbourCell": null,
                            "cellStatus": "opened"
                        }
                    ]
                ],
                "result": "won",
                "duration": 1000
                "posix": 10
            },
            {
                "grid": [
                    [
                        {
                            "cellType": "mineNeighbourCell",
                            "minesOnNeighbourCell": 1,
                            "cellStatus": "opened"
                        },
                        {
                            "cellType": "mineNeighbourCell",
                            "minesOnNeighbourCell": 1,
                            "cellStatus": "flagged"
                        }
                    ],
                    [
                        {
                            "cellType": "mineNeighbourCell",
                            "minesOnNeighbourCell": 1,
                            "cellStatus": "untouched"
                        },
                        {
                            "cellType": "mineCell",
                            "minesOnNeighbourCell": null,
                            "cellStatus": "opened"
                        }
                    ]
                ],
                "result": "lost",
                "duration": 1000
            }
        ]
    }
    """
