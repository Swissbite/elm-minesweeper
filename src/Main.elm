module Main exposing (..)

import Browser exposing (Document, UrlRequest(..))
import Browser.Events as Events
import Browser.Navigation exposing (Key)
import Element exposing (Element, fill)
import Element.Lazy as Lazy
import ErrorPage404
import Game.Game as Game
import Tuple
import Types exposing (..)
import Url exposing (Url)



--- PROGRAM ---


main : Program Flags Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlChange =
            \url ->
                Debug.log "onUrlChange" url
                    |> (\_ -> Navigation Noop)
        , onUrlRequest =
            \request ->
                Debug.log "onUrlRequest" request
                    |> (\_ -> Navigation Noop)
        }



--- UPDATE / INIT / SUBSCRIPTIONS ---


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameView gameMsg ->
            Game.update gameMsg model
                |> Tuple.mapSecond (Cmd.map GameView)

        Navigation _ ->
            ( model, Cmd.none )

        SetScreenSize x y ->
            ( { model | device = Element.classifyDevice { width = x, height = y } }, Cmd.none )


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    Debug.log "init url" url
        |> (\_ ->
                Debug.log "init key" key
                    |> (\_ -> ( { device = Element.classifyDevice { width = flags.width, height = flags.height }, currentView = Game, gameBoardStatus = NoGame PreSelect, gameInteractionMode = Reveal, gameRunningTimes = [], gamePauseResumeState = Paused, playedGameHistory = Game.decodeStoredFinishedGameHistory flags.history }, Cmd.none ))
           )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map GameView (Game.subscriptions model)
        , Events.onResize (\values -> SetScreenSize values)
        ]



--- VIEW ---


view : Model -> Document Msg
view m =
    { title = "Elm - Minesweeper"
    , body =
        [ Element.layout [ Element.width Element.fill, Element.height Element.fill ] <|
            Element.column [ Element.width fill, Element.height fill, Element.centerX ]
                [ Lazy.lazy (\t -> Element.el [ Element.centerX ] <| Element.text t) "Minesweeper"
                , Lazy.lazy selectBoardView m
                ]
        ]
    }


selectBoardView : Model -> Element Msg
selectBoardView model =
    case model.currentView of
        Game ->
            Game.view model
                |> Element.map GameView

        Error404 ->
            ErrorPage404.view model


navigationHeader : Model -> Element Msg
navigationHeader _ =
    Element.row [ Element.width Element.fill ] []
