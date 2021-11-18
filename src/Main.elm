module Main exposing (..)
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Border as Border

main : Program () (Maybe b) msg
main = Browser.document
    { init = \_ -> (Nothing, Cmd.none)
    , view = view
    , update = \_ _ -> (Nothing, Cmd.none)
    , subscriptions = \_ -> Sub.none
    }

view : Maybe a -> Browser.Document msg
view _ =
    { title = "Minesweeper - WIP"
    , body = [body]
    }

body = Element.layout []
               myRowOfStuff

myRowOfStuff : Element msg
myRowOfStuff =
    row [ width fill, centerY, spacing 30 ]
        [ myElement
        , myElement
        , el [ alignRight ] myElement
        ]


myElement : Element msg
myElement =
    el
        [ Background.color (rgb255 240 0 245)
        , Font.color (rgb255 255 255 255)
        , Border.rounded 3
        , padding 30
        ]
        (text "stylish!")