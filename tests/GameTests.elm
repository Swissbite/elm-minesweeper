module GameTests exposing (..)

import Expect
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
