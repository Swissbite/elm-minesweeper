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


module ErrorPage404 exposing (view)

import Colors
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Styles
import Types exposing (Model)


view : Model -> Element msg
view _ =
    Element.column [ Element.width Element.fill, Element.height Element.fill, Background.image "./white-smoke-wallpaper-abstract-desktop-background.jpg" ]
        [ Element.textColumn [ Element.centerX, Element.centerY, Element.width Element.fill ]
            [ Element.paragraph [ Element.centerX, Element.centerY, Font.size 200, Element.padding 20 ] [ Element.text <| String.fromChar Styles.icons.exploded ]
            , Element.paragraph [ Element.centerX, Element.centerY, Font.color Colors.white, Element.padding 20 ] [ Element.text "Error 404 - Dangerous zone. Maybe you did not see the warnings. Follow the route back." ]
            ]
        , Element.paragraph [ Element.alignBottom, Element.alignRight, Element.width Element.fill, Font.color Colors.lightGrey ] [ Element.link [ Element.alignRight ] { url = "https://www.freepik.com/free-photo/white-smoke-wallpaper-abstract-desktop-background_18416635.htm", label = Element.text "Background image created by rawpixel.com on Freepik" } ]
        ]
