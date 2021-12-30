module Game.History exposing (GameHistoryQuery, queryParser, update, view)

import Dict
import Element exposing (Element)
import Game.Internal exposing (..)
import Grid
import List
import Styles
import Types exposing (..)
import Url.Parser.Query as Query


view : Model -> Element GameHistoryMsg
view model =
    Element.column [ Element.centerX, Element.centerY ]
        [ Element.text "Hello. Your view should be history"
        , extractQueryParameterFromView model |> manualStringDecodedParams |> Element.text
        ]


manualStringDecodedParams : Maybe GameHistoryQuery -> String
manualStringDecodedParams params =
    case params of
        Nothing ->
            "No Param. URL Decoding issue"

        Just data ->
            """{ "orderBy": \"""" ++ orderByToString data.orderBy ++ """", "direction": \"""" ++ orderDirectionToString data.orderDirection ++ """" """


extractQueryParameterFromView : Model -> Maybe GameHistoryQuery
extractQueryParameterFromView model =
    case model.currentView of
        History mode orderBy direction ->
            Just
                { orderBy = orderBy
                , orderDirection = direction
                , displayMode = mode
                }

        _ ->
            Nothing


orderByToString : GameHistoryOrderBy -> String
orderByToString orderBy =
    case orderBy of
        ByDuration ->
            "duration"

        ByFieldSize ->
            "size"

        ByEntryId ->
            "date"


orderDirectionToString : OrderDirection -> String
orderDirectionToString direction =
    case direction of
        Ascending ->
            "asc"

        Descending ->
            "desc"


displayModeToString : GameHistoryDisplayMode -> String
displayModeToString mode =
    case mode of
        DisplayAll ->
            "all"

        DisplayWon ->
            "won"

        DisplayLost ->
            "lost"


type alias GameHistoryQuery =
    { orderBy : GameHistoryOrderBy
    , orderDirection : OrderDirection
    , displayMode : GameHistoryDisplayMode
    }


queryParser : Query.Parser GameHistoryQuery
queryParser =
    Query.map3
        (\orderBy direction mode ->
            { orderBy = orderBy
            , orderDirection = direction
            , displayMode = mode
            }
        )
        (Query.map (Maybe.withDefault ByEntryId) (Query.enum "orderBy" <| Dict.fromList [ ( "date", ByEntryId ), ( "size", ByFieldSize ), ( "duration", ByDuration ) ]))
        (Query.map (Maybe.withDefault Descending) (Query.enum "direction" <| Dict.fromList [ ( "asc", Ascending ), ( "desc", Descending ) ]))
        (Query.map (Maybe.withDefault DisplayAll) (Query.enum "mode" <| Dict.fromList [ ( "all", DisplayAll ), ( "lost", DisplayLost ), ( "won", DisplayWon ) ]))


update : GameHistoryMsg -> Model -> ( Model, Cmd GameHistoryMsg )
update msg model =
    case msg of
        DeleteAll ->
            ( { model | playedGameHistory = [] }, saveFinishedGameHistory [] )

        DeleteLost ->
            let
                onlyWonFilter : FinishedGameHistoryEntry -> Bool
                onlyWonFilter e =
                    case e of
                        FinishedGameHistoryEntry _ Won _ ->
                            True

                        _ ->
                            False

                onlyWon =
                    List.filter onlyWonFilter model.playedGameHistory
            in
            ( { model | playedGameHistory = onlyWon }, saveFinishedGameHistory onlyWon )

        SetDisplayMode mode ->
            case model.currentView of
                History _ order direction ->
                    ( { model | currentView = History mode order direction }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetOrderBy order direction ->
            case model.currentView of
                History mode _ _ ->
                    ( { model | currentView = History mode order direction }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


finishedGameHistoryList : List FinishedGameHistoryEntry -> Element GameHistoryMsg
finishedGameHistoryList finishedGameHistory =
    let
        gameStatistics : FinishedGameHistoryEntry -> { result : GameResult, playedSeconds : String, playedMinutes : String, gridHeight : Int, gridWidth : Int, mines : Int }
        gameStatistics entry =
            { result =
                case entry of
                    FinishedGameHistoryEntry _ res _ ->
                        res
            , playedSeconds =
                case entry of
                    FinishedGameHistoryEntry _ _ time ->
                        time
                            // 1000
                            |> modBy 60
                            |> (\s ->
                                    if s < 10 then
                                        String.concat [ "0", String.fromInt s ]

                                    else
                                        String.fromInt s
                               )
            , playedMinutes =
                case entry of
                    FinishedGameHistoryEntry _ _ time ->
                        time
                            // 1000
                            // 60
                            |> String.fromInt
            , gridHeight =
                case entry of
                    FinishedGameHistoryEntry grid _ _ ->
                        Grid.height grid
            , gridWidth =
                case entry of
                    FinishedGameHistoryEntry grid _ _ ->
                        Grid.width grid
            , mines =
                case entry of
                    FinishedGameHistoryEntry grid _ _ ->
                        Grid.foldl
                            (\cell count ->
                                case cell of
                                    GameCell MineCell _ ->
                                        count + 1

                                    _ ->
                                        count
                            )
                            0
                            grid
            }
    in
    case finishedGameHistory of
        [] ->
            Element.el [ Element.paddingXY 0 30 ] <| Element.text "No games played yet"

        _ ->
            Element.table [ Element.spacing 10, Element.paddingXY 0 30 ]
                { data = List.map gameStatistics finishedGameHistory
                , columns =
                    [ { header = Element.text <| String.fromList [ Styles.icons.victory, '/', Styles.icons.exploded ]
                      , width = Element.fillPortion 1
                      , view =
                            \entry ->
                                case entry.result of
                                    Won ->
                                        Element.text <| String.fromChar Styles.icons.victory

                                    Lost ->
                                        Element.text <| String.fromChar Styles.icons.exploded
                      }
                    , { header = Element.text "↔️"
                      , width = Element.fillPortion 2
                      , view =
                            \entry -> Element.text <| String.fromInt entry.gridWidth
                      }
                    , { header = Element.text "↕️"
                      , width = Element.fillPortion 2
                      , view =
                            \entry -> Element.text <| String.fromInt entry.gridHeight
                      }
                    , { header = Element.text <| String.fromChar Styles.icons.untouchedBomb
                      , width = Element.fillPortion 2
                      , view =
                            \entry -> Element.text <| String.fromInt entry.mines
                      }
                    , { header = Element.text <| String.fromChar Styles.icons.stopWatch
                      , width = Element.fillPortion 4
                      , view =
                            \entry ->
                                Element.text <| String.concat [ entry.playedMinutes, ":", entry.playedSeconds ]
                      }
                    ]
                }
