{-
   -- This file is part of Elm Minesweeper.
   --
   -- Elm Minesweeper is free software: you can redistribute it and/or modify it under
   -- the terms of the GNU General Public License as published by the Free Software
   -- Foundation, either version 3 of the License, or (at your option) any later version.
   --
   -- Elm Minesweeper is distributed in the hope that it will be useful, but WITHOUT ANY
   -- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
   -- PARTICULAR PURPOSE. See the GNU General Public License for more details.
   --
   -- You should have received a copy of the GNU General Public License along with
   -- Elm Minesweeper. If not, see <https://www.gnu.org/licenses/>.
   --
-}


module Tests exposing (..)

import Expect
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ test "Addition" <|
            \_ ->
                Expect.equal 10 (3 + 7)
        , test "String.left" <|
            \_ ->
                Expect.equal "a" (String.left 1 "abcdefg")
        ]
