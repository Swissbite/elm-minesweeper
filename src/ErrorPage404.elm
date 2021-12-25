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
            , Element.paragraph [ Element.centerX, Element.centerY, Font.color Colors.white, Element.padding 20 ] [ Element.text "Error 404 - Dangerous zone. Maybe you did nod see the warnings. Follow the route back." ]
            ]
        , Element.paragraph [ Element.alignBottom, Element.alignRight, Element.width Element.fill, Font.color Colors.lightGrey ] [ Element.link [ Element.alignRight ] { url = "https://www.freepik.com/photos/background", label = Element.text "Background photo created by rawpixel.com - www.freepik.com" } ]
        ]
