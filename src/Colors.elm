{-
   This file is part of Elm Minesweeper.

   Elm Minesweeper is free software: you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later version.

   Elm Minesweeper is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
   PARTICULAR PURPOSE. See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along with
   Elm Minesweeper. If not, see <https://www.gnu.org/licenses/>.

-}


module Colors exposing (asparagus, babyBlue, black, caputMortuum, cellBorderColor, cerise, deepskyblue, eggplant, fieryRose, gold, green, hotpink, lightGrey, lightgreen, openedCellGray, saffron, smitten, tomato, untouchedCellGray, white)

import Element exposing (Color, rgb255, rgba)


black : Color
black =
    rgba 0 0 0 1


tomato : Color
tomato =
    rgb255 255 99 71


deepskyblue : Color
deepskyblue =
    rgb255 0 191 255


gold : Color
gold =
    rgb255 255 215 0


lightgreen : Color
lightgreen =
    rgb255 144 238 144


hotpink : Color
hotpink =
    rgb255 255 105 180


saffron : Color
saffron =
    Element.rgba255 227 181 5 1


fieryRose : Color
fieryRose =
    Element.rgba255 245 100 118 1


cerise : Color
cerise =
    Element.rgba255 228 63 111 1


smitten : Color
smitten =
    Element.rgba255 190 62 130 1


eggplant : Color
eggplant =
    Element.rgba255 94 67 82 1


caputMortuum : Color
caputMortuum =
    Element.rgba255 80 36 25 1


asparagus : Color
asparagus =
    Element.rgba255 126 161 114 1


babyBlue : Color
babyBlue =
    Element.rgba255 108 212 255 1


untouchedCellGray : Color
untouchedCellGray =
    Element.rgba255 190 190 190 0.8


openedCellGray : Color
openedCellGray =
    Element.rgba255 190 190 190 0.2


cellBorderColor : Color
cellBorderColor =
    Element.rgba 100 100 100 1


lightGrey : Color
lightGrey =
    rgb255 187 187 187


green : Color
green =
    rgb255 39 203 139


white : Color
white =
    rgb255 255 255 255
