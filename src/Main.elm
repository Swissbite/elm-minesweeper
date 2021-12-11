module Main exposing (..)

import Browser
import Element exposing (Element, fill)
import Element.Lazy as Lazy
import Game.Game as Game
import Html exposing (Html)
import Tuple
import Types exposing (..)



--- PROGRAM ---


main : Program Int Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }



--- UPDATE / INIT / SUBSCRIPTIONS ---


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameView gameMsg ->
            Game.update gameMsg model
                |> Tuple.mapSecond (Cmd.map GameView)


init : Int -> ( Model, Cmd Msg )
init _ =
    ( { currentView = Game, gameBoardStatus = NoGame PreSelect, gameInteractionMode = Reveal, gameRunningTimes = [], gamePauseResumeState = Paused, playedGameHistory = [] }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map GameView (Game.subscriptions model)
        ]



--- VIEW ---


view : Model -> Html Msg
view m =
    Element.layout [ Element.width Element.fill, Element.height Element.fill ] <|
        Element.column [ Element.width fill, Element.height fill, Element.centerX ]
            [ Lazy.lazy (\t -> Element.el [ Element.centerX ] <| Element.text t) "Minesweeper"
            , Lazy.lazy selectBoardView m
            ]


selectBoardView : Model -> Element Msg
selectBoardView model =
    case model.currentView of
        Game ->
            Game.view model
                |> Element.map GameView
