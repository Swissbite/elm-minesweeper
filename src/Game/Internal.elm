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


module Game.Internal exposing (..)

{-|

    Internal helper functions for game logic, parsing and transforming data.
    Reasons for this module:

    1. Write identical helper functions for the game and the history just once
    2. Expose only the necessary functions (view and update) in Game.elm or History.elm
    3. Make complicated functions testable without exposing them in Game.elm or History.elm
    4. No circular includes

-}

import Grid exposing (Grid)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Ports
import Time
import Types exposing (..)


generateListOfPossibleIndices : Grid InitGameCell -> Coordinate -> List Int
generateListOfPossibleIndices initGrid clickedOn =
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
        (Decode.field "entries" (Decode.list decodeFinishedGameHistoryEntryVersion1))


decodeVersion0GameHistory : Decoder FinishedGameHistory
decodeVersion0GameHistory =
    Decode.map
        (\entries ->
            { version = 1, entries = entries }
        )
        (Decode.list decodeFinishedGameHistoryEntryVersion0)


decodeFinishedGameHistoryEntryVersion1 : Decoder FinishedGameHistoryEntry
decodeFinishedGameHistoryEntryVersion1 =
    Decode.map4
        (\grid result duration posix ->
            { grid = grid
            , result = result
            , duration = duration
            , playFinish = Time.millisToPosix posix
            }
        )
        (Decode.field "grid" decodeGrid)
        (Decode.field "result" decodeResult)
        (Decode.field "duration" Decode.int)
        (Decode.field "posix" Decode.int)


decodeFinishedGameHistoryEntryVersion0 : Decoder FinishedGameHistoryEntry
decodeFinishedGameHistoryEntryVersion0 =
    Decode.map3
        (\grid result time ->
            { grid = grid
            , result = result
            , duration = time
            , playFinish = Time.millisToPosix 0
            }
        )
        (Decode.field "grid" decodeGrid)
        (Decode.field "result" decodeResult)
        (Decode.field "time" Decode.int)


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
    Encode.object [ ( "grid", gridJson ), ( "result", resultJson ), ( "duration", timeJson ), ( "posix", posix ) ]


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


encodeFinishedGameHistory : List FinishedGameHistoryEntry -> String
encodeFinishedGameHistory finishedGameHistory =
    Encode.object
        [ ( "version", Encode.int 1 )
        , ( "entries", Encode.list finishedGameHistoryEntryEncoder finishedGameHistory )
        ]
        |> Encode.encode 0


saveFinishedGameHistory : List FinishedGameHistoryEntry -> Cmd msg
saveFinishedGameHistory finishedGameHistory =
    encodeFinishedGameHistory finishedGameHistory
        |> Ports.storeFinishedGameHistory


millisToString : Int -> String
millisToString millis =
    let
        seconds =
            millis
                // 1000
                |> modBy 60
                |> String.fromInt
                |> (\s ->
                        if String.length s < 2 then
                            "0" ++ s

                        else
                            s
                   )

        minutes =
            millis
                // 1000
                // 60
                |> String.fromInt
    in
    minutes ++ ":" ++ seconds
