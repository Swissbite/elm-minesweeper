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

import Bitwise
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


decodeFinishedGameHistory : Decoder (List FinishedGameHistoryEntry)
decodeFinishedGameHistory =
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


staticChecksumSalt : String
staticChecksumSalt =
    "elm-minesweeper-running-game-v1"


{-| djb2 hash (xor variant) over the UTF-16 code units of a string.

    The accumulator is normalized to [0, 2^32) via shiftRightZfBy 0 after every
    step, so the intermediate product hash * 33 stays below 2^38 and therefore
    within the exactly representable Int range. Bitwise.xor truncates the
    product to 32 bits before the unsigned normalization.

-}
djb2Hash : String -> Int
djb2Hash =
    String.foldl
        (\char hash ->
            (hash * 33)
                |> Bitwise.xor (Char.toCode char)
                |> Bitwise.shiftRightZfBy 0
        )
        5381


{-| Salted checksum over the compact JSON encoding of a running game. Guards the
stored game (board and elapsed time segments alike) against casual manual edits
in localStorage - determined tampering is out of scope for a pure SPA. The
browser salt is generated randomly on first app start and kept in localStorage,
so a valid checksum cannot be derived from the source code alone and saves are
not portable between browsers.
-}
runningGameChecksum : String -> String -> Int
runningGameChecksum browserSalt payload =
    djb2Hash (staticChecksumSalt ++ browserSalt ++ payload)


runningGameValue : PlayGameGrid -> GameModel -> Encode.Value
runningGameValue grid gameModel =
    Encode.object
        [ ( "grid", Grid.rows grid |> Encode.array (Encode.array gameCellEncoder) )
        , ( "runningTimes"
          , Encode.list
                (\( start, end ) ->
                    Encode.object
                        [ ( "start", Encode.int (Time.posixToMillis start) )
                        , ( "end", Encode.int (Time.posixToMillis end) )
                        ]
                )
                gameModel.gameRunningTimes
          )
        , ( "lastClockTick", Encode.int (Time.posixToMillis gameModel.lastClockTick) )
        , ( "interactionMode"
          , Encode.string
                (case gameModel.gameInteractionMode of
                    Reveal ->
                        "reveal"

                    Flag ->
                        "flag"
                )
          )
        ]


encodeRunningGame : String -> GameModel -> Maybe String
encodeRunningGame browserSalt gameModel =
    case gameModel.gameBoardStatus of
        RunningGame grid ->
            let
                gameValue =
                    runningGameValue grid gameModel
            in
            Encode.object
                [ ( "version", Encode.int 1 )
                , ( "checksum", Encode.int (runningGameChecksum browserSalt (Encode.encode 0 gameValue)) )
                , ( "game", gameValue )
                ]
                |> Encode.encode 0
                |> Just

        _ ->
            Nothing


saveRunningGame : String -> GameModel -> Cmd msg
saveRunningGame browserSalt gameModel =
    encodeRunningGame browserSalt gameModel
        |> Maybe.map Ports.storeRunningGame
        |> Maybe.withDefault Cmd.none


clearRunningGame : Cmd msg
clearRunningGame =
    Ports.clearRunningGame ()


{-| Decodes the versioned envelope around a stored running game. The game is
decoded first, canonically re-encoded and re-hashed; any checksum mismatch,
unknown version or malformed payload rejects the whole save.
-}
decodeRunningGameEnvelope : String -> Decoder GameModel
decodeRunningGameEnvelope browserSalt =
    Decode.map2 Tuple.pair
        (Decode.field "version" Decode.int
            |> Decode.andThen
                (\v ->
                    if v == 1 then
                        Decode.succeed v

                    else
                        Decode.fail "Unsupported running game version"
                )
        )
        (Decode.field "checksum" Decode.int)
        |> Decode.andThen
            (\( _, expectedChecksum ) ->
                Decode.field "game" runningGameSnapshotDecoder
                    |> Decode.andThen
                        (\gameModel ->
                            case gameModel.gameBoardStatus of
                                RunningGame grid ->
                                    if runningGameChecksum browserSalt (Encode.encode 0 (runningGameValue grid gameModel)) == expectedChecksum then
                                        Decode.succeed gameModel

                                    else
                                        Decode.fail "Running game checksum mismatch"

                                _ ->
                                    Decode.fail "Stored game is not a running game"
                        )
            )


{-| A restored game is always paused - the player resumes it explicitly from the
selection view, which keeps the pause/segment invariant of updateTimePlayGame
intact.
-}
runningGameSnapshotDecoder : Decoder GameModel
runningGameSnapshotDecoder =
    Decode.map4
        (\grid times tick mode ->
            { gameBoardStatus = RunningGame grid
            , gameInteractionMode = mode
            , gameRunningTimes = times
            , gamePauseResumeState = Paused
            , lastClockTick = Time.millisToPosix tick
            }
        )
        (Decode.field "grid" decodeGrid)
        (Decode.field "runningTimes" (Decode.list decodeTimeSegment))
        (Decode.field "lastClockTick" Decode.int)
        (Decode.field "interactionMode" decodeInteractionMode)


decodeTimeSegment : Decoder ( Time.Posix, Time.Posix )
decodeTimeSegment =
    Decode.map2 (\start end -> ( Time.millisToPosix start, Time.millisToPosix end ))
        (Decode.field "start" Decode.int)
        (Decode.field "end" Decode.int)


decodeInteractionMode : Decoder CellClickMode
decodeInteractionMode =
    Decode.string
        |> Decode.andThen
            (\modeAsString ->
                case modeAsString of
                    "reveal" ->
                        Decode.succeed Reveal

                    "flag" ->
                        Decode.succeed Flag

                    _ ->
                        Decode.fail "Invalid interaction mode"
            )


calculateElapsedTimeMillis : List ( Time.Posix, Time.Posix ) -> Int
calculateElapsedTimeMillis =
    List.foldl (\( from, to ) summedUp -> (Time.posixToMillis to - Time.posixToMillis from) + summedUp) 0


playGameGridToPlaygroundDefinition : PlayGameGrid -> PlayGroundDefinition
playGameGridToPlaygroundDefinition grid =
    let
        foldLFn : GameCell -> Int -> Int
        foldLFn cell count =
            case cell of
                GameCell MineCell _ ->
                    count + 1

                _ ->
                    count
    in
    { cols = Grid.width grid
    , rows = Grid.height grid
    , mines = Grid.foldl foldLFn 0 grid
    }


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


flagCell : Coordinate -> PlayGameGrid -> PlayGameGrid
flagCell coords playGrid =
    coordinateToPair coords
        |> (\c ->
                Grid.get c playGrid
                    |> (\cell ->
                            case cell of
                                Just (GameCell gameCell Flagged) ->
                                    Grid.set c (GameCell gameCell Untouched) playGrid

                                Just (GameCell gameCell Untouched) ->
                                    Grid.set c (GameCell gameCell Flagged) playGrid

                                Just (GameCell (MineNeighbourCell neighbours) Opened) ->
                                    if neighbours == calculateFlaggedCellsAroundCoordinate coords playGrid then
                                        openSurroundingCells coords playGrid

                                    else
                                        playGrid

                                _ ->
                                    playGrid
                       )
           )


openCell : Coordinate -> PlayGameGrid -> PlayGameGrid
openCell coords playGrid =
    let
        coordinateAsPair =
            coordinateToPair coords

        cell =
            Grid.get coordinateAsPair playGrid
    in
    case cell of
        Nothing ->
            playGrid

        Just (GameCell (MineNeighbourCell neighbours) Opened) ->
            if neighbours == calculateFlaggedCellsAroundCoordinate coords playGrid then
                openSurroundingCells coords playGrid

            else
                playGrid

        Just (GameCell cellType cellStatus) ->
            case ( cellType, cellStatus ) of
                ( _, Opened ) ->
                    playGrid

                ( _, Flagged ) ->
                    playGrid

                ( MineNeighbourCell neighbours, _ ) ->
                    Grid.set coordinateAsPair (GameCell (MineNeighbourCell neighbours) Opened) playGrid

                ( EmptyCell, _ ) ->
                    Grid.set coordinateAsPair (GameCell EmptyCell Opened) playGrid
                        |> (\nextGrid ->
                                calculateNeighbourCoordinates coords
                                    |> (\surroundingCoordinatesAsPair -> List.foldl (\coordinate grid -> openCell coordinate grid) nextGrid surroundingCoordinatesAsPair)
                           )

                ( MineCell, _ ) ->
                    Grid.set coordinateAsPair (GameCell MineCell Opened) playGrid


openSurroundingCells : Coordinate -> PlayGameGrid -> PlayGameGrid
openSurroundingCells coordinate playGrid =
    let
        mapCoordinateToTupleCoordinateAndMaybeGameCell : Coordinate -> ( Coordinate, Maybe GameCell )
        mapCoordinateToTupleCoordinateAndMaybeGameCell neighbourCoordinate =
            ( neighbourCoordinate, Grid.get (coordinateToPair neighbourCoordinate) playGrid )

        foldGridOpenUntouchedCellsToGrid : ( Coordinate, Maybe GameCell ) -> PlayGameGrid -> PlayGameGrid
        foldGridOpenUntouchedCellsToGrid ( coordinateToCheck, maybeCell ) grid =
            case maybeCell of
                Just (GameCell _ Untouched) ->
                    openCell coordinateToCheck grid

                _ ->
                    grid
    in
    calculateNeighbourCoordinates coordinate
        |> List.map mapCoordinateToTupleCoordinateAndMaybeGameCell
        |> List.foldl foldGridOpenUntouchedCellsToGrid playGrid


calculateFlaggedCellsAroundCoordinate : Coordinate -> PlayGameGrid -> Int
calculateFlaggedCellsAroundCoordinate coords grid =
    calculateNeighbourCoordinates coords
        |> List.map coordinateToPair
        |> List.map (\coordinateAsPair -> Grid.get coordinateAsPair grid)
        |> List.map
            (\maybeCell ->
                case maybeCell of
                    Just (GameCell _ Flagged) ->
                        1

                    _ ->
                        0
            )
        |> List.sum


calculateNeighbourCoordinates : Coordinate -> List Coordinate
calculateNeighbourCoordinates coords =
    [ { x = coords.x - 1, y = coords.y - 1 }
    , { x = coords.x - 1, y = coords.y }
    , { x = coords.x - 1, y = coords.y + 1 }
    , { x = coords.x, y = coords.y - 1 }
    , { x = coords.x, y = coords.y + 1 }
    , { x = coords.x + 1, y = coords.y - 1 }
    , { x = coords.x + 1, y = coords.y }
    , { x = coords.x + 1, y = coords.y + 1 }
    ]


isAMineExploded : PlayGameGrid -> Bool
isAMineExploded =
    let
        isExplodedMine : GameCell -> Bool -> Bool
        isExplodedMine cell exploded =
            case cell of
                GameCell MineCell Opened ->
                    True

                _ ->
                    exploded
    in
    Grid.foldl isExplodedMine False


areAllNoMineFieldsRevealed : PlayGameGrid -> Bool
areAllNoMineFieldsRevealed =
    let
        isMissingFieldOpen : GameCell -> Bool -> Bool
        isMissingFieldOpen cell allRevealed =
            case cell of
                GameCell EmptyCell state ->
                    case state of
                        Opened ->
                            allRevealed

                        _ ->
                            False

                GameCell (MineNeighbourCell _) state ->
                    case state of
                        Opened ->
                            allRevealed

                        _ ->
                            False

                _ ->
                    allRevealed
    in
    Grid.foldl isMissingFieldOpen True


coordinateToPair : Coordinate -> ( Int, Int )
coordinateToPair coords =
    ( coords.x, coords.y )
