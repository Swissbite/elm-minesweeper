module Main exposing (..)

import Application exposing (..)
import Browser
import Debug
import Element exposing (Color, Element, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, column, el, fill, height, image, layout, maximum, padding, paddingXY, px, rgb, row, spacing, text, width)
import Element.Input exposing (button)
import Game exposing (GameStatus(..), gameBoardView)
import Html exposing (Html)
import Time



---- PROGRAM ----


icons =
    { markerFlag = "⚑"
    , untouchedBomb = "💣"
    , exploded = "💥"
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
view model =
    layout [ width fill, height fill ] <|
        column
            [ spacing 20
            , width fill
            , height fill
            ]
            [ row [ paddingXY 20 0, spacing 20, width fill, height (fill |> maximum 75) ]
                [ image [ paddingXY 0 5, alignLeft, centerY, height fill ] { src = "./logo.svg", description = "Elm-Mine logo" }
                , el [ alignLeft, centerY ] <| text "Elm Minsesweeper"
                , button [ alignRight ] { onPress = Nothing, label = text "History" }
                , button [ alignRight ] { onPress = Nothing, label = text "About" }
                ]
            , gameBoardView model.gameStatus
            , row [ padding 20, width fill, height <| px 75, alignBottom ]
                [ el [ alignLeft ] <| text "Left footer"
                , el [ centerX ] <| text "center footer"
                , el [ alignRight ] <| text "right footer"
                ]
            ]


activeScreenView : Model -> Element Msg
activeScreenView model =
    case model.activeSceen of
        GameScreen ->
            gameBoardView model.gameStatus

        _ ->
            Debug.todo "Implement other screens"


white : Color
white =
    rgb 1 1 1



---- MODEL ----


type alias Model =
    { gameStatus : GameStatus
    , activeSceen : ActiveScreen
    }


init : ( Model, Cmd Msg )
init =
    ( { gameStatus = NoGame, activeSceen = GameScreen }, Cmd.none )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameMsg updateMsg ->
            ( { model | gameStatus = Game.interactWithGame updateMsg model.gameStatus }, Cmd.none )

        ChangeScreen screen ->
            screenChangeUpdate screen model


screenChangeUpdate : ActiveScreen -> Model -> ( Model, Cmd Msg )
screenChangeUpdate screen model =
    case screen of
        GameScreen ->
            ( { model | activeSceen = screen, gameStatus = togglePauseToTarget GameIsRunning model.gameStatus }, Cmd.none )

        GameHistroyScreen ->
            ( { model | activeSceen = screen, gameStatus = togglePauseToTarget GameIsPaused model.gameStatus }, Cmd.none )

        AboutScreen ->
            ( { model | activeSceen = screen, gameStatus = togglePauseToTarget GameIsPaused model.gameStatus }, Cmd.none )


type TargetGameStatus
    = GameIsPaused
    | GameIsRunning


togglePauseToTarget : TargetGameStatus -> GameStatus -> GameStatus
togglePauseToTarget targetStatus currentStatus =
    case ( targetStatus, currentStatus ) of
        ( GameIsPaused, RunningGame _ ) ->
            Game.interactWithGame TogglePause currentStatus

        ( GameIsRunning, PausedGame _ ) ->
            Game.interactWithGame TogglePause currentStatus

        _ ->
            currentStatus


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameStatus of
        RunningGame _ ->
            Time.every 100 (\posix -> GameMsg (Tick posix))

        _ ->
            Sub.none
