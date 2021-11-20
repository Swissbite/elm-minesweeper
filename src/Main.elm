module Main exposing (..)

import Browser
import Framework.Typography exposing (h1)
import Html exposing (Html)
import Element exposing (Color, Element, alignRight, centerX, centerY, clip, column, el, fill, height, image, layout, maximum, minimum, padding, px, rgb, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font

---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }

---- VIEW ----


view : Model -> Html Msg
view model = layout [ width fill, height fill ] <|
                     column
                         [ centerX
                         , spacing 20
                         ]
                         [ image [width <| px 200]
                             { src = "./logo.svg"
                             , description = "An image"
                             }
                         , h1 [] <| text "Heading"
                         ]
white : Color
white =
    rgb 1 1 1
---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
  ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
