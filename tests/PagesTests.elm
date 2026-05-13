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


module PagesTests exposing (all)

import Expect
import Pages
import Test exposing (..)


all : Test
all =
    describe "Static pages"
        [ test "published page can be found by slug" <|
            \_ ->
                Pages.findPublishedBySlug "about"
                    |> Maybe.map .title
                    |> Expect.equal (Just "Über das Projekt")
        , test "unknown slug is not found" <|
            \_ ->
                Pages.findPublishedBySlug "missing-page"
                    |> Expect.equal Nothing
        , test "navigation entries expose the published page" <|
            \_ ->
                Pages.navigationEntries
                    |> List.map .slug
                    |> Expect.equal [ "about" ]
        ]
