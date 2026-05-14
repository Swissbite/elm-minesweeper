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


module Colors exposing (background, black, cellBorderColor, danger, mine1, mine2, mine3, mine4, mine5, mine6, mine7, mine8, openedCellGray, primary, surface, textDim, textMain, transparent, untouchedCellGray, warning, white)

import Element exposing (Color, rgb, rgb255, rgba)
import Theme exposing (Theme(..))


transparent : Color
transparent =
    rgba 0 0 0 0


black : Color
black =
    rgb255 0 0 0


white : Color
white =
    rgb 1 1 1


background : Theme -> Color
background theme =
    case theme of
        Light ->
            rgb255 245 245 245

        -- #F5F5F5
        Dark ->
            rgb255 18 18 18



-- #121212


surface : Theme -> Color
surface theme =
    case theme of
        Light ->
            rgb 1 1 1

        -- #FFFFFF
        Dark ->
            rgb255 42 42 53



-- #2A2A35


primary : Theme -> Color
primary theme =
    case theme of
        Light ->
            rgb255 76 175 80

        -- #4CAF50
        Dark ->
            rgb255 76 175 80



-- #4CAF50


danger : Theme -> Color
danger theme =
    case theme of
        Light ->
            rgb255 244 67 54

        -- #F44336
        Dark ->
            rgb255 244 67 54



-- #F44336


warning : Theme -> Color
warning theme =
    case theme of
        Light ->
            rgb255 255 152 0

        -- #FF9800
        Dark ->
            rgb255 255 193 7



-- #FFC107


textMain : Theme -> Color
textMain theme =
    case theme of
        Light ->
            rgb255 33 33 33

        -- #212121
        Dark ->
            rgb255 224 224 224



-- #E0E0E0


textDim : Theme -> Color
textDim theme =
    case theme of
        Light ->
            rgb255 117 117 117

        -- #757575
        Dark ->
            rgb255 160 160 176



-- #A0A0B0


untouchedCellGray : Theme -> Color
untouchedCellGray theme =
    case theme of
        Light ->
            rgb255 224 224 224

        -- #E0E0E0
        Dark ->
            rgb255 51 51 68



-- #333344


openedCellGray : Theme -> Color
openedCellGray theme =
    case theme of
        Light ->
            rgb255 200 200 200

        -- #C8C8C8
        Dark ->
            rgb255 34 34 46



-- #22222E


cellBorderColor : Theme -> Color
cellBorderColor theme =
    case theme of
        Light ->
            rgb255 189 189 189

        -- #BDBDBD
        Dark ->
            rgb255 68 68 85



-- #444455


mine1 : Theme -> Color
mine1 theme =
    case theme of
        Light ->
            rgb255 25 118 210

        -- Darker Blue
        Dark ->
            rgb255 100 181 246



-- #64B5F6


mine2 : Theme -> Color
mine2 theme =
    case theme of
        Light ->
            rgb255 56 142 60

        -- Darker Green
        Dark ->
            rgb255 129 199 132



-- #81C784


mine3 : Theme -> Color
mine3 theme =
    case theme of
        Light ->
            rgb255 211 47 47

        -- Darker Red
        Dark ->
            rgb255 229 115 115



-- #E57373


mine4 : Theme -> Color
mine4 theme =
    case theme of
        Light ->
            rgb255 123 31 162

        -- Darker Purple
        Dark ->
            rgb255 186 104 200



-- #BA68C8


mine5 : Theme -> Color
mine5 theme =
    case theme of
        Light ->
            rgb255 230 74 25

        -- Darker Orange
        Dark ->
            rgb255 255 138 101



-- #FF8A65


mine6 : Theme -> Color
mine6 theme =
    case theme of
        Light ->
            rgb255 0 151 167

        -- Darker Cyan
        Dark ->
            rgb255 77 208 225



-- #4DD0E1


mine7 : Theme -> Color
mine7 theme =
    case theme of
        Light ->
            rgb255 175 180 43

        -- Darker Yellow/Lime
        Dark ->
            rgb255 220 231 117



-- #DCE775


mine8 : Theme -> Color
mine8 theme =
    case theme of
        Light ->
            rgb255 93 64 55

        -- Darker Brown
        Dark ->
            rgb255 161 136 127



-- #A1887F
