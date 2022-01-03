module Main exposing (..)

import Browser exposing (Document, UrlRequest(..))
import Browser.Events as Events
import Browser.Navigation as Navigation exposing (Key)
import Element exposing (Element, fill)
import Element.Lazy as Lazy
import ErrorPage404
import Game.Game as Game
import Game.History as GameHistory
import Tuple
import Types exposing (..)
import Url exposing (Url)
import Url.Parser as UP exposing ((</>), (<?>))



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
            Navigation
        }



--- UPDATE / INIT / SUBSCRIPTIONS / URL Handling ---


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameView gameMsg ->
            Game.update gameMsg model
                |> Tuple.mapSecond (Cmd.map GameView)

        GameHistory gameHistoryMsg ->
            GameHistory.update gameHistoryMsg model
                |> Tuple.mapSecond (Cmd.map GameHistory)

        Navigation request ->
            navigationHandling request model

        SetScreenSize x y ->
            ( { model | device = Element.classifyDevice { width = x, height = y } }, Cmd.none )


viewRouteParser : UP.Parser (View -> a) a
viewRouteParser =
    UP.oneOf
        [ UP.map Game UP.top
        , UP.map gameHistoryQueryToView (UP.s "history" <?> GameHistory.queryParser)
        ]


gameHistoryQueryToView : GameHistory.GameHistoryQuery -> View
gameHistoryQueryToView query =
    History query.displayMode query.orderBy query.orderDirection


mayBeQueryParamsToHistoryView : Maybe GameHistoryDisplayMode -> Maybe GameHistoryOrderBy -> Maybe OrderDirection -> View
mayBeQueryParamsToHistoryView maybeMode maybeOrderBy maybeSort =
    History (Maybe.withDefault DisplayAll maybeMode) (Maybe.withDefault ByEntryId maybeOrderBy) (Maybe.withDefault Ascending maybeSort)


navigationHandling : UrlRequest -> Model -> ( Model, Cmd Msg )
navigationHandling request model =
    case request of
        Internal url ->
            UP.parse viewRouteParser url
                |> Maybe.withDefault Error404
                |> (\parsedView ->
                        if model.currentView == parsedView then
                            ( model, Cmd.none )

                        else
                            ( { model | currentView = parsedView }, Navigation.pushUrl model.key (Url.toString url) )
                   )

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
            , game = Game.initModel
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
                [ navigationView
                , Lazy.lazy selectBoardView m
                ]
        ]
    }


navigationView : Element Msg
navigationView =
    Element.row [ Element.width Element.fill ]
        [ Element.el [ Element.alignLeft, Element.paddingXY 10 10 ] <| Element.text "Minesweeper"
        , Element.link [ Element.alignRight, Element.paddingXY 10 10 ] { url = "/", label = Element.text "Game" }
        , Element.link [ Element.alignRight, Element.paddingXY 10 10 ] { url = "/history", label = Element.text "History" }
        ]


selectBoardView : Model -> Element Msg
selectBoardView model =
    case model.currentView of
        Game ->
            Game.view model
                |> Element.map GameView

        Error404 ->
            ErrorPage404.view model

        History _ _ _ ->
            GameHistory.view model
                |> Element.map GameHistory


navigationHeader : Model -> Element Msg
navigationHeader _ =
    Element.row [ Element.width Element.fill ] []
