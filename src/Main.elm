module Main exposing (..)

import Browser exposing (Document, UrlRequest(..))
import Browser.Events as Events
import Browser.Navigation as Navigation exposing (Key)
import Dict exposing (Dict)
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
                Internal url
                    |> Navigation
        , onUrlRequest =
            \request ->
                Navigation request
        }



--- UPDATE / INIT / SUBSCRIPTIONS / URL Handling ---


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameView gameMsg ->
            Game.update gameMsg model
                |> Tuple.mapSecond (Cmd.map GameView)

        Navigation request ->
            navigationHandling request model

        SetScreenSize x y ->
            ( { model | device = Element.classifyDevice { width = x, height = y } }, Cmd.none )


knownPathsToViewMap : Dict String View
knownPathsToViewMap =
    Dict.fromList [ ( "/", Game ) ]


navigationHandling : UrlRequest -> Model -> ( Model, Cmd Msg )
navigationHandling request model =
    case request of
        Internal url ->
            Dict.get url.path knownPathsToViewMap
                |> Maybe.withDefault Error404
                |> (\v -> ( { model | currentView = v }, Cmd.none ))

        External url ->
            ( model, Navigation.load url )


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        basicInitModel : Model
        basicInitModel =
            { key = key
            , device =
                Element.classifyDevice
                    { width = flags.width
                    , height = flags.height
                    }
            , currentView = Game
            , gameBoardStatus = NoGame PreSelect
            , gameInteractionMode = Reveal
            , gameRunningTimes = []
            , gamePauseResumeState = Paused
            , playedGameHistory = Game.decodeStoredFinishedGameHistory flags.history
            }

        navigationMsg : Msg
        navigationMsg =
            Navigation (Internal url)
    in
    update navigationMsg basicInitModel


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
