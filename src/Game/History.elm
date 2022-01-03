module Game.History exposing (GameHistoryQuery, queryParser, update, view)

import Dict
import Element exposing (Element)
import Game.Internal exposing (..)
import Grid
import List
import Styles
import Time
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


sortAndFilterListByQueryParameter : Maybe GameHistoryQuery -> List FinishedGameHistoryEntry -> List FinishedGameHistoryEntry
sortAndFilterListByQueryParameter maybeParams list =
    case maybeParams of
        Nothing ->
            list

        Just params ->
            list
                |> List.filter (filterByDisplayMode params.displayMode)
                |> List.sortWith (sortAndOrder params.orderBy params.orderDirection)


filterByDisplayMode : GameHistoryDisplayMode -> (FinishedGameHistoryEntry -> Bool)
filterByDisplayMode displayMode =
    \entry ->
        case displayMode of
            DisplayAll ->
                True

            DisplayWon ->
                entry.result == Won

            DisplayLost ->
                entry.result == Lost


sortAndOrder : GameHistoryOrderBy -> OrderDirection -> (FinishedGameHistoryEntry -> FinishedGameHistoryEntry -> Order)
sortAndOrder orderBy direction =
    \e1 e2 ->
        case ( orderBy, direction ) of
            ( ByDuration, order ) ->
                if e1.duration == e2.duration then
                    EQ

                else if e1.duration < e2.duration then
                    if order == Ascending then
                        LT

                    else
                        GT

                else if order == Ascending then
                    GT

                else
                    LT

            ( ByFieldSize, order ) ->
                let
                    e1Size =
                        Grid.width e1.grid * Grid.height e1.grid

                    e2Size =
                        Grid.width e2.grid * Grid.height e2.grid
                in
                if e1Size == e2Size then
                    EQ

                else if e1Size < e2Size then
                    if order == Ascending then
                        LT

                    else
                        GT

                else if order == Ascending then
                    GT

                else
                    LT

            ( ByEntryId, order ) ->
                let
                    e1TimeInt =
                        Time.posixToMillis e1.playFinish

                    e2TimeInt =
                        Time.posixToMillis e2.playFinish
                in
                if e1TimeInt == e2TimeInt then
                    EQ

                else if e1TimeInt < e2TimeInt then
                    if order == Ascending then
                        LT

                    else
                        GT

                else if order == Ascending then
                    GT

                else
                    LT


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
                    e.result == Won

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
            { result = entry.result
            , playedSeconds =
                entry.duration
                    // 1000
                    |> modBy 60
                    |> (\s ->
                            if s < 10 then
                                String.concat [ "0", String.fromInt s ]

                            else
                                String.fromInt s
                       )
            , playedMinutes =
                entry.duration
                    // 1000
                    // 60
                    |> String.fromInt
            , gridHeight = Grid.height entry.grid
            , gridWidth = Grid.width entry.grid
            , mines =
                Grid.foldl
                    (\cell count ->
                        case cell of
                            GameCell MineCell _ ->
                                count + 1

                            _ ->
                                count
                    )
                    0
                    entry.grid
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
