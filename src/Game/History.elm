module Game.History exposing (GameHistoryQuery, queryParser, update, view)

import Colors
import Dict
import Element exposing (Column, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Game.Internal exposing (..)
import Grid
import List
import Styles
import Time
import Types exposing (..)
import Url.Parser.Query as Query


view : Model -> Element GameHistoryMsg
view model =
    Element.column [ Element.centerX, Element.height Element.fill, Element.spacing 10 ]
        [ Element.el [ Font.semiBold, Font.size 30, Element.centerX ] <| Element.text "Your play history"
        , Element.row [ Element.width Element.fill, Element.spacing 10 ]
            [ Input.button [ Background.color Colors.saffron, Border.solid, Element.padding 10, Border.rounded 10, Element.centerX ]
                { label = Element.text "Delete lost games"
                , onPress = Just DeleteLost
                }
            , Input.button [ Background.color Colors.tomato, Border.solid, Element.padding 10, Border.rounded 10, Element.centerX ]
                { label = Element.text "Delete history"
                , onPress = Just DeleteAll
                }
            ]
        , sortableHistoryTable model
        ]


sortableHistoryTable : Model -> Element GameHistoryMsg
sortableHistoryTable model =
    let
        currentQueryParameter : GameHistoryQuery
        currentQueryParameter =
            extractQueryParameterFromView model |> extractQueryParameterFromViewWithDefaults
    in
    Element.table [ Element.spacingXY 20 10, Element.centerX, Element.width Element.fill, Element.paddingXY 20 10 ]
        { data = sortAndFilterListByQueryParameter (extractQueryParameterFromView model) model.playedGameHistory
        , columns =
            [ historyColumns ByResult currentQueryParameter
            , historyColumns ByPosix currentQueryParameter
            , historyColumns ByFieldSize currentQueryParameter
            , historyColumns ByMines currentQueryParameter
            , historyColumns ByDuration currentQueryParameter
            ]
        }


historyColumns : GameHistoryOrderBy -> (GameHistoryQuery -> Column FinishedGameHistoryEntry GameHistoryMsg)
historyColumns orderBy =
    case orderBy of
        ByResult ->
            byResultColumn

        ByFieldSize ->
            byFieldSizeColumn

        ByDuration ->
            byDurationColumn

        ByPosix ->
            byPosixColumn

        ByMines ->
            byMinesColumn


byResultColumn : GameHistoryQuery -> Column FinishedGameHistoryEntry GameHistoryMsg
byResultColumn query =
    { header =
        Element.row
            [ Events.onClick <|
                case ( query.orderBy, query.orderDirection ) of
                    ( ByResult, Ascending ) ->
                        SetOrderBy ByResult Descending

                    _ ->
                        SetOrderBy ByResult Ascending
            ]
            [ Element.text "Result "
            , Element.text <|
                case ( query.orderBy, query.orderDirection ) of
                    ( ByResult, Ascending ) ->
                        String.fromChar Styles.icons.upSign

                    ( ByResult, Descending ) ->
                        String.fromChar Styles.icons.downSign

                    _ ->
                        ""
            ]
    , width = Element.fill |> Element.minimum 300
    , view =
        \entry ->
            case entry.result of
                Won ->
                    Element.text <| String.fromChar Styles.icons.victory

                Lost ->
                    Element.text <| String.fromChar Styles.icons.exploded
    }


byDurationColumn : GameHistoryQuery -> Column FinishedGameHistoryEntry GameHistoryMsg
byDurationColumn query =
    { header =
        Element.row
            [ Events.onClick <|
                case ( query.orderBy, query.orderDirection ) of
                    ( ByDuration, Ascending ) ->
                        SetOrderBy ByDuration Descending

                    _ ->
                        SetOrderBy ByDuration Ascending
            ]
            [ Element.text <| String.fromChar Styles.icons.stopWatch
            , Element.text " Duration "
            , Element.text <|
                case ( query.orderBy, query.orderDirection ) of
                    ( ByDuration, Ascending ) ->
                        String.fromChar Styles.icons.upSign

                    ( ByDuration, Descending ) ->
                        String.fromChar Styles.icons.downSign

                    _ ->
                        ""
            ]
    , width = Element.fill |> Element.minimum 300
    , view =
        \entry ->
            Element.row []
                [ entry.duration // 1000 // 60 |> String.fromInt |> Element.text
                , Element.text ":"
                , entry.duration
                    // 1000
                    |> modBy 60
                    |> String.fromInt
                    |> (\s ->
                            if String.length s == 1 then
                                String.append "0" s

                            else
                                s
                       )
                    |> Element.text
                ]
    }


byFieldSizeColumn : GameHistoryQuery -> Column FinishedGameHistoryEntry GameHistoryMsg
byFieldSizeColumn query =
    { header =
        Element.row
            [ Element.centerX
            , Element.width Element.fill
            , Events.onClick <|
                case ( query.orderBy, query.orderDirection ) of
                    ( ByFieldSize, Ascending ) ->
                        SetOrderBy ByFieldSize Descending

                    _ ->
                        SetOrderBy ByFieldSize Ascending
            ]
            [ Element.text <| String.fromChar Styles.icons.stopWatch
            , Element.text " Field size "
            , Element.text <|
                case ( query.orderBy, query.orderDirection ) of
                    ( ByFieldSize, Ascending ) ->
                        String.fromChar Styles.icons.upSign

                    ( ByFieldSize, Descending ) ->
                        String.fromChar Styles.icons.downSign

                    _ ->
                        ""
            ]
    , width = Element.fill |> Element.minimum 300
    , view =
        \entry ->
            Element.row [ Element.centerX, Element.width Element.fill ]
                [ Grid.width entry.grid |> String.fromInt |> Element.text
                , Element.text " x "
                , Grid.height entry.grid |> String.fromInt |> Element.text
                ]
    }


byMinesColumn : GameHistoryQuery -> Column FinishedGameHistoryEntry GameHistoryMsg
byMinesColumn query =
    { header =
        Element.row
            [ Events.onClick <|
                case ( query.orderBy, query.orderDirection ) of
                    ( ByMines, Ascending ) ->
                        SetOrderBy ByMines Descending

                    _ ->
                        SetOrderBy ByMines Ascending
            ]
            [ Element.text <| String.fromChar Styles.icons.untouchedBomb
            , Element.text " Mines "
            , Element.text <|
                case ( query.orderBy, query.orderDirection ) of
                    ( ByMines, Ascending ) ->
                        String.fromChar Styles.icons.upSign

                    ( ByMines, Descending ) ->
                        String.fromChar Styles.icons.downSign

                    _ ->
                        ""
            ]
    , width = Element.fill |> Element.minimum 300
    , view =
        \entry ->
            countMines entry.grid |> String.fromInt |> Element.text
    }


byPosixColumn : GameHistoryQuery -> Column FinishedGameHistoryEntry GameHistoryMsg
byPosixColumn query =
    { header =
        Element.row
            [ Element.centerX
            , Element.width Element.fill
            , Events.onClick <|
                case ( query.orderBy, query.orderDirection ) of
                    ( ByPosix, Ascending ) ->
                        SetOrderBy ByPosix Descending

                    _ ->
                        SetOrderBy ByPosix Ascending
            ]
            [ Element.text "Date "
            , Element.text <|
                case ( query.orderBy, query.orderDirection ) of
                    ( ByPosix, Ascending ) ->
                        String.fromChar Styles.icons.upSign

                    ( ByPosix, Descending ) ->
                        String.fromChar Styles.icons.downSign

                    _ ->
                        ""
            ]
    , width = Element.fill |> Element.minimum 300
    , view =
        let
            toNumberMonthAsString : Time.Month -> String
            toNumberMonthAsString month =
                case month of
                    Time.Jan ->
                        "01"

                    Time.Feb ->
                        "02"

                    Time.Mar ->
                        "03"

                    Time.Apr ->
                        "04"

                    Time.May ->
                        "05"

                    Time.Jun ->
                        "06"

                    Time.Jul ->
                        "07"

                    Time.Aug ->
                        "08"

                    Time.Sep ->
                        "09"

                    Time.Oct ->
                        "10"

                    Time.Nov ->
                        "11"

                    Time.Dec ->
                        "12"
        in
        \entry ->
            Element.row [ Element.centerX, Element.width Element.fill ]
                [ Time.toYear Time.utc entry.playFinish |> String.fromInt |> Element.text
                , Element.text "-"
                , Time.toMonth Time.utc entry.playFinish |> toNumberMonthAsString |> Element.text
                , Element.text "-"
                , Time.toDay Time.utc entry.playFinish
                    |> String.fromInt
                    |> (\s ->
                            if String.length s == 1 then
                                "0" ++ s

                            else
                                s
                       )
                    |> Element.text
                ]
    }


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
            ( ByResult, order ) ->
                if e1.result == e2.result then
                    EQ

                else
                    case ( e1.result, order ) of
                        ( Won, Ascending ) ->
                            GT

                        ( Won, Descending ) ->
                            LT

                        ( Lost, Ascending ) ->
                            LT

                        ( Lost, Descending ) ->
                            GT

            ( ByDuration, order ) ->
                if e1.duration == e2.duration then
                    EQ

                else
                    case ( e1.duration < e2.duration, order ) of
                        ( True, Ascending ) ->
                            LT

                        ( True, Descending ) ->
                            GT

                        ( False, Ascending ) ->
                            GT

                        ( False, Descending ) ->
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

                else
                    case ( e1Size < e2Size, order ) of
                        ( True, Ascending ) ->
                            LT

                        ( True, Descending ) ->
                            GT

                        ( False, Ascending ) ->
                            GT

                        ( False, Descending ) ->
                            LT

            ( ByPosix, order ) ->
                let
                    e1TimeInt =
                        Time.posixToMillis e1.playFinish

                    e2TimeInt =
                        Time.posixToMillis e2.playFinish
                in
                if e1TimeInt == e2TimeInt then
                    EQ

                else
                    case ( e1TimeInt < e2TimeInt, order ) of
                        ( True, Ascending ) ->
                            LT

                        ( True, Descending ) ->
                            GT

                        ( False, Ascending ) ->
                            GT

                        ( False, Descending ) ->
                            LT

            ( ByMines, order ) ->
                let
                    e1Mines =
                        countMines e1.grid

                    e2Mines =
                        countMines e2.grid
                in
                if e1Mines == e2Mines then
                    EQ

                else
                    case ( e1Mines < e2Mines, order ) of
                        ( True, Ascending ) ->
                            LT

                        ( True, Descending ) ->
                            GT

                        ( False, Ascending ) ->
                            GT

                        ( False, Descending ) ->
                            LT


countMines : PlayGameGrid -> Int
countMines =
    Grid.foldl
        (\cell acc ->
            case cell of
                GameCell MineCell _ ->
                    acc + 1

                _ ->
                    acc
        )
        0


extractQueryParameterFromViewWithDefaults : Maybe GameHistoryQuery -> GameHistoryQuery
extractQueryParameterFromViewWithDefaults =
    Maybe.withDefault { orderBy = ByPosix, orderDirection = Descending, displayMode = DisplayAll }


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
        (Query.map
            (Maybe.withDefault ByPosix)
            (Query.enum "orderBy" <|
                Dict.fromList
                    [ ( "date", ByPosix )
                    , ( "size", ByFieldSize )
                    , ( "duration", ByDuration )
                    , ( "result", ByResult )
                    , ( "mines", ByMines )
                    ]
            )
        )
        (Query.map
            (Maybe.withDefault Descending)
            (Query.enum "direction" <|
                Dict.fromList
                    [ ( "asc", Ascending )
                    , ( "desc", Descending )
                    ]
            )
        )
        (Query.map
            (Maybe.withDefault DisplayAll)
            (Query.enum "mode" <|
                Dict.fromList
                    [ ( "all", DisplayAll )
                    , ( "lost", DisplayLost )
                    , ( "won", DisplayWon )
                    ]
            )
        )


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
