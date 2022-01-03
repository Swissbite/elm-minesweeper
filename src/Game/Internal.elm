module Game.Internal exposing (..)

import Grid exposing (Grid)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Ports
import Time
import Types exposing (..)


generateListOfPossibleIndizes : Grid InitGameCell -> Coordinates -> List Int
generateListOfPossibleIndizes initGrid clickedOn =
    let
        gridWidth =
            Grid.width initGrid

        gridHeight =
            Grid.height initGrid

        foldFn : Maybe Int -> List Int -> List Int
        foldFn x acc =
            case x of
                Nothing ->
                    acc

                Just idx ->
                    idx :: acc

        openingAreaCoordinates =
            List.range (max 0 clickedOn.x - 1) (min (gridWidth - 1) (clickedOn.x + 1))
                |> List.concatMap (\x -> List.map (\y -> { x = x, y = y }) <| List.range (max 0 clickedOn.y - 1) (min (gridHeight - 1) (clickedOn.y + 1)))
    in
    Grid.indexedMap
        (\x y _ ->
            if List.member { x = x, y = y } openingAreaCoordinates then
                Nothing

            else
                Just (x + gridWidth * y)
        )
        initGrid
        |> Grid.foldr foldFn []


deocdeFinishedGameHistory : Decoder (List FinishedGameHistoryEntry)
deocdeFinishedGameHistory =
    Decode.map (\history -> history.entries)
        (Decode.oneOf
            [ decodeVersion1GameHistory
            , decodeVersion0GameHistory
            ]
        )


decodeVersion1GameHistory : Decoder FinishedGameHistory
decodeVersion1GameHistory =
    Decode.map2
        (\version entries ->
            { version = version, entries = entries }
        )
        (Decode.field "version" Decode.int
            |> Decode.andThen
                (\v ->
                    if v == 1 then
                        Decode.succeed v

                    else
                        Decode.fail "Version is not 1. Wrong decoder"
                )
        )
        (Decode.field "entries" (Decode.list decodeFinishedGameHistoryEntry))


decodeVersion0GameHistory : Decoder FinishedGameHistory
decodeVersion0GameHistory =
    Decode.map
        (\entries ->
            { version = 1, entries = entries }
        )
        (Decode.list decodeFinishedGameHistoryEntry)


decodeFinishedGameHistoryEntry : Decoder FinishedGameHistoryEntry
decodeFinishedGameHistoryEntry =
    Decode.map4
        (\grid result time posix ->
            { grid = grid
            , result = result
            , duration = time
            , playFinish = Time.millisToPosix (Maybe.withDefault 0 posix)
            }
        )
        (Decode.field "grid" decodeGrid)
        (Decode.field "result" decodeResult)
        (Decode.field "time" Decode.int)
        (Decode.maybe <| Decode.field "posix" Decode.int)


decodeGrid : Decoder PlayGameGrid
decodeGrid =
    Decode.list (Decode.list gameCellDecoder)
        |> Decode.andThen
            (\gridAsList ->
                case Grid.fromList gridAsList of
                    Just grid ->
                        Decode.succeed grid

                    Nothing ->
                        Decode.fail "Could not decode grid"
            )


decodeResult : Decoder GameResult
decodeResult =
    Decode.string
        |> Decode.andThen
            (\resultAsString ->
                String.toLower resultAsString
                    |> (\lowerResult ->
                            case lowerResult of
                                "won" ->
                                    Decode.succeed Won

                                "lost" ->
                                    Decode.succeed Lost

                                _ ->
                                    Decode.fail "Unknown game result string"
                       )
            )


finishedGameHistoryEntryEncoder : FinishedGameHistoryEntry -> Encode.Value
finishedGameHistoryEntryEncoder entry =
    let
        gridJson =
            Grid.rows entry.grid
                |> Encode.array (Encode.array gameCellEncoder)

        resultJson =
            case entry.result of
                Won ->
                    Encode.string "won"

                Lost ->
                    Encode.string "lost"

        timeJson =
            Encode.int entry.duration

        posix =
            Time.posixToMillis entry.playFinish
                |> Encode.int
    in
    Encode.object [ ( "grid", gridJson ), ( "result", resultJson ), ( "time", timeJson ), ( "posix", posix ) ]


gameCellDecoder : Decoder GameCell
gameCellDecoder =
    let
        singleFieldsToCell : CellType -> Maybe Int -> CellStatus -> Maybe GameCell
        singleFieldsToCell cellType maybeMineCount cellStatus =
            case ( cellType, maybeMineCount ) of
                ( MineCell, _ ) ->
                    Just (GameCell MineCell cellStatus)

                ( EmptyCell, _ ) ->
                    Just (GameCell EmptyCell cellStatus)

                ( MineNeighbourCell _, Just count ) ->
                    Just (GameCell (MineNeighbourCell count) cellStatus)

                _ ->
                    Nothing
    in
    Decode.map3 singleFieldsToCell
        (Decode.field "cellType" decodeCellType)
        (Decode.field "minesOnNeighbourCell" <| Decode.oneOf [ Decode.null Nothing, Decode.map Just Decode.int ])
        (Decode.field "cellStatus" decodeCellStatus)
        |> Decode.andThen
            (\maybeGameCell ->
                case maybeGameCell of
                    Just gameCell ->
                        Decode.succeed gameCell

                    Nothing ->
                        Decode.fail "Could not decode game cell"
            )


decodeCellType : Decoder CellType
decodeCellType =
    Decode.string
        |> Decode.andThen
            (\cellTypeAsString ->
                case cellTypeAsString of
                    "mine" ->
                        Decode.succeed MineCell

                    "mineCell" ->
                        Decode.succeed MineCell

                    "emptyCell" ->
                        Decode.succeed EmptyCell

                    "mineNeighbourCell" ->
                        Decode.succeed (MineNeighbourCell -1)

                    _ ->
                        Decode.fail "Invalid cell type"
            )


decodeCellStatus : Decoder CellStatus
decodeCellStatus =
    Decode.string
        |> Decode.andThen
            (\cellStatusAsString ->
                case String.toLower cellStatusAsString of
                    "untouched" ->
                        Decode.succeed Untouched

                    "flagged" ->
                        Decode.succeed Flagged

                    "opened" ->
                        Decode.succeed Opened

                    _ ->
                        Decode.fail "Invalid cell status"
            )


gameCellEncoder : GameCell -> Encode.Value
gameCellEncoder (GameCell cellType cellStatus) =
    let
        encodedType =
            (case cellType of
                MineCell ->
                    "mineCell"

                MineNeighbourCell _ ->
                    "mineNeighbourCell"

                EmptyCell ->
                    "emptyCell"
            )
                |> Encode.string

        encodedMinesOnNeighbourCell =
            case cellType of
                MineNeighbourCell i ->
                    Encode.int i

                _ ->
                    Encode.null

        encodedCellStatus =
            case cellStatus of
                Untouched ->
                    Encode.string "untouched"

                Flagged ->
                    Encode.string "flagged"

                Opened ->
                    Encode.string "opened"
    in
    Encode.object [ ( "cellType", encodedType ), ( "minesOnNeighbourCell", encodedMinesOnNeighbourCell ), ( "cellStatus", encodedCellStatus ) ]


saveFinishedGameHistory : List FinishedGameHistoryEntry -> Cmd msg
saveFinishedGameHistory finishedGameHistory =
    Encode.list finishedGameHistoryEntryEncoder finishedGameHistory
        |> Encode.encode 0
        |> Ports.storeFinishedGameHistory
