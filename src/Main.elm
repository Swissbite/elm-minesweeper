module Main exposing (..)

import Browser
import Framework.Typography exposing (h1)
import Game exposing (GameStatus(..), GameUpdateMsg(..), PlayGroundDefinition)
import Html exposing (Html)
import Element exposing (Color, Element, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, clip, column, el, fill, height, image, layout, maximum, minimum, padding, px, rgb, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Time

---- PROGRAM ----
icons =
  {
    markerFlag = "⚑",
    untouchedBomb = "💣",
    exploded = "💥"
  }

main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }

view : Model -> Html Msg
view model = layout [ width fill, height fill ] <|
                     column
                         [ spacing 20
                         , width fill
                         , height fill
                         ]
                         [ row [padding 20, spacing 20, width fill]
                          [ el [alignRight] <| text "Link 1"
                          , el [alignRight] <| text "Link 2"
                          ]
                         , row [padding 20, width fill, height fill]
                           [ el [alignLeft, alignTop] <| text "Left status column"
                           , el [centerX, centerY] <| text "Game Area"
                           , el [alignRight, alignTop] <| text "Right status column"
                           ]

                         , row [padding 20, width fill, height <| px 100, alignBottom]
                           [ el [alignLeft] <| text "Left footer"
                           , el [centerX] <| text "center footer"
                           , el [alignRight] <| text "right footer"
                           ]

                         ]
white : Color
white =
    rgb 1 1 1
---- MODEL ----


type alias Model =
    {gameStatus: GameStatus}


init : ( Model, Cmd Msg )
init =
  ( {gameStatus = NoGame}, Cmd.none )



---- UPDATE ----


type Msg = GameMsg GameUpdateMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameMsg updateMsg -> ({model | gameStatus = Game.interactWithGame updateMsg model.gameStatus}, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.gameStatus of
    RunningGame _ ->  Time.every 100 (\posix -> GameMsg (Tick posix))
    _ -> Sub.none
