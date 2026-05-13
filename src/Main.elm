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


module Main exposing (..)

import Browser exposing (Document, UrlRequest(..))
import Browser.Events as Events
import Browser.Navigation as Navigation exposing (Key)
import Colors
import Element exposing (Element, fill)
import Element.Background as Background
import Element.Lazy as Lazy
import ErrorPage404
import Game.Game as Game
import Game.History as GameHistory
import Html.Attributes as HA
import Tuple
import Types exposing (..)
import Url exposing (Url)
import Url.Parser as UP exposing ((</>), (<?>))


githubPagePathPrefix : String
githubPagePathPrefix =
    "elm-minesweeper"


compactVerticalPadding : Int
compactVerticalPadding =
    8


defaultVerticalPadding : Int
defaultVerticalPadding =
    10



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
                url
                    |> Internal
                    |> Navigation
        , onUrlRequest =
            \request ->
                request
                    |> Navigation
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
        , UP.map Game (UP.s githubPagePathPrefix)
        , UP.map gameHistoryQueryToView (UP.s "history" <?> GameHistory.queryParser)
        , UP.map gameHistoryQueryToView (UP.s githubPagePathPrefix </> UP.s "history" <?> GameHistory.queryParser)
        ]


gameHistoryQueryToView : GameHistory.GameHistoryQuery -> View
gameHistoryQueryToView query =
    History query.displayMode query.orderBy query.orderDirection


mayBeQueryParamsToHistoryView : Maybe GameHistoryDisplayMode -> Maybe GameHistoryOrderBy -> Maybe OrderDirection -> View
mayBeQueryParamsToHistoryView maybeMode maybeOrderBy maybeSort =
    History (Maybe.withDefault DisplayAll maybeMode) (Maybe.withDefault ByPosix maybeOrderBy) (Maybe.withDefault Ascending maybeSort)


navigationHandling : UrlRequest -> Model -> ( Model, Cmd Msg )
navigationHandling request model =
    case request of
        Internal url ->
            url
                |> UP.parse viewRouteParser
                |> Maybe.withDefault Error404
                |> (\parsedView ->
                        if model.currentView == parsedView then
                            ( model, Cmd.none )

                        else
                            Game.update (NavigationEvent parsedView) model
                                |> Tuple.mapBoth
                                    (\m -> { m | currentView = parsedView })
                                    (\cmd ->
                                        Cmd.batch
                                            [ Cmd.map GameView cmd
                                            , Navigation.pushUrl model.key (Url.toString url)
                                            ]
                                    )
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
            , containsGithubPrefixInPath = flags.initPath |> hasGithubPathPrefix
            , playedGameHistory = Game.decodeStoredFinishedGameHistory flags.history
            }

        navigationMsg : Msg
        navigationMsg =
            Navigation (Internal url)
    in
    update navigationMsg basicInitModel


hasGithubPathPrefix : String -> Bool
hasGithubPathPrefix initPath =
    String.startsWith ("/" ++ githubPagePathPrefix) initPath


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
        [ Element.layout
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.clipX
            , Element.htmlAttribute <| HA.style "overflow-x" "hidden"
            ]
          <|
            Element.column [ Element.width fill, Element.height fill, Element.centerX, Element.spacingXY 0 0 ]
                [ navigationView m.containsGithubPrefixInPath
                , Lazy.lazy selectBoardView m
                , footerView m
                ]
        ]
    }


navigationView : Bool -> Element Msg
navigationView containsGithubPrefixInPath =
    let
        pathWithTrailingSlash : String
        pathWithTrailingSlash =
            if containsGithubPrefixInPath then
                "/" ++ githubPagePathPrefix ++ "/"

            else
                "/"
    in
    Element.wrappedRow
        [ Element.width Element.fill
        , Background.color Colors.openedCellGray
        , Element.paddingXY 10 8
        , Element.spacingXY 16 8
        ]
        [ Element.el [ Element.alignLeft ] <| Element.text "Elm Minesweeper"
        , Element.row [ Element.alignRight, Element.spacing 20 ]
            [ Element.link [] { url = pathWithTrailingSlash ++ "", label = Element.text "Game" }
            , Element.link [] { url = pathWithTrailingSlash ++ "history", label = Element.text "History" }
            ]
        ]


footerView : Model -> Element Msg
footerView model =
    Element.wrappedRow
        [ Element.width Element.fill
        , Element.paddingXY 10
            (if model.device.class == Element.Phone then
                compactVerticalPadding

             else
                defaultVerticalPadding
            )
        , Element.spacingXY 20 8
        , Element.spaceEvenly
        ]
        [ Element.el [] <| Element.text "(c) 2026 David Daester"
        , Element.link []
            { url = "https://github.com/Swissbite/elm-minesweeper"
            , label =
                Element.image
                    [ Element.height <|
                        Element.px <|
                            if model.device.class == Element.Phone then
                                22

                            else
                                25
                    ]
                    { src = "./github-mark.svg", description = "GitHub logo" }
            }
        , Element.image
            [ Element.height <|
                Element.px <|
                    if model.device.class == Element.Phone then
                        24

                    else
                        31
            ]
            { src = "./agplv3-88x31.png", description = "AGPLv3 license logo" }
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
