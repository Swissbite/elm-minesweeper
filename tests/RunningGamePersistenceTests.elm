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


module RunningGamePersistenceTests exposing (..)

import Expect
import Fuzz
import Game.Game as Game
import Game.Internal as GameInternal
import Grid
import Json.Encode as Encode
import Test exposing (..)
import Time
import Types exposing (..)


testSalt : String
testSalt =
    "0123456789abcdef"


runningGameModel : Maybe GameModel
runningGameModel =
    Grid.fromList
        [ [ GameCell (MineNeighbourCell 1) Opened, GameCell (MineNeighbourCell 1) Flagged ]
        , [ GameCell (MineNeighbourCell 1) Untouched, GameCell MineCell Untouched ]
        ]
        |> Maybe.map
            (\grid ->
                { gameBoardStatus = RunningGame grid
                , gameInteractionMode = Flag
                , gameRunningTimes = [ ( Time.millisToPosix 1000, Time.millisToPosix 5000 ) ]
                , gamePauseResumeState = Resumed 1
                , lastClockTick = Time.millisToPosix 5000
                }
            )


checksumTests : Test
checksumTests =
    describe "djb2 checksum"
        [ test "empty string hashes to the djb2 seed" <|
            \_ ->
                GameInternal.djb2Hash "" |> Expect.equal 5381
        , test "known answer for a single character" <|
            \_ ->
                -- (5381 * 33) xor 97 = 177604
                GameInternal.djb2Hash "a" |> Expect.equal 177604
        , fuzz Fuzz.string "hash is always within the unsigned 32 bit range" <|
            \randomString ->
                GameInternal.djb2Hash randomString
                    |> Expect.all
                        [ Expect.atLeast 0
                        , Expect.lessThan 4294967296
                        ]
        , fuzz Fuzz.string "checksum combines the static and the browser salt with the payload" <|
            \randomString ->
                GameInternal.runningGameChecksum testSalt randomString
                    |> Expect.equal (GameInternal.djb2Hash (GameInternal.staticChecksumSalt ++ testSalt ++ randomString))
        ]


roundTripTests : Test
roundTripTests =
    describe "Encode and decode a running game"
        [ test "a running game round-trips and is restored as paused" <|
            \_ ->
                case runningGameModel of
                    Nothing ->
                        Expect.fail "Testdata seems to be invalid"

                    Just gameModel ->
                        GameInternal.encodeRunningGame testSalt gameModel
                            |> Maybe.andThen (Game.decodeStoredRunningGame testSalt)
                            |> Expect.equal (Just { gameModel | gamePauseResumeState = Paused })
        ]


tamperTests : Test
tamperTests =
    describe "Tampered or invalid stored games are rejected"
        [ test "a flipped cell status invalidates the checksum" <|
            \_ ->
                case runningGameModel |> Maybe.andThen (GameInternal.encodeRunningGame testSalt) of
                    Nothing ->
                        Expect.fail "Testdata seems to be invalid"

                    Just encoded ->
                        String.replace "\"cellStatus\":\"untouched\"" "\"cellStatus\":\"opened\"" encoded
                            |> Game.decodeStoredRunningGame testSalt
                            |> Expect.equal Nothing
        , test "a manipulated time segment invalidates the checksum" <|
            \_ ->
                case runningGameModel |> Maybe.andThen (GameInternal.encodeRunningGame testSalt) of
                    Nothing ->
                        Expect.fail "Testdata seems to be invalid"

                    Just encoded ->
                        String.replace "\"start\":1000" "\"start\":4000" encoded
                            |> Game.decodeStoredRunningGame testSalt
                            |> Expect.equal Nothing
        , test "a save is rejected under a different browser salt" <|
            \_ ->
                case runningGameModel |> Maybe.andThen (GameInternal.encodeRunningGame testSalt) of
                    Nothing ->
                        Expect.fail "Testdata seems to be invalid"

                    Just encoded ->
                        Game.decodeStoredRunningGame "another-salt" encoded
                            |> Expect.equal Nothing
        , test "a wrong checksum is rejected" <|
            \_ ->
                envelopeWith 1 0
                    |> Maybe.map (Game.decodeStoredRunningGame testSalt)
                    |> Expect.equal (Just Nothing)
        , test "an unknown version is rejected even with a valid checksum" <|
            \_ ->
                case runningGameModel of
                    Nothing ->
                        Expect.fail "Testdata seems to be invalid"

                    Just gameModel ->
                        case gameModel.gameBoardStatus of
                            RunningGame grid ->
                                envelopeWith 2 (GameInternal.runningGameChecksum testSalt (Encode.encode 0 (GameInternal.runningGameValue grid gameModel)))
                                    |> Maybe.map (Game.decodeStoredRunningGame testSalt)
                                    |> Expect.equal (Just Nothing)

                            _ ->
                                Expect.fail "Testdata seems to be invalid"
        , test "garbage input is rejected" <|
            \_ ->
                Game.decodeStoredRunningGame testSalt "asdf" |> Expect.equal Nothing
        , test "an empty string is rejected" <|
            \_ ->
                Game.decodeStoredRunningGame testSalt "" |> Expect.equal Nothing
        ]


{-| Builds the storage envelope around the shared test game with an arbitrary
version and checksum, bypassing encodeRunningGame's canonical checksum.
-}
envelopeWith : Int -> Int -> Maybe String
envelopeWith version checksum =
    runningGameModel
        |> Maybe.andThen
            (\gameModel ->
                case gameModel.gameBoardStatus of
                    RunningGame grid ->
                        Encode.object
                            [ ( "version", Encode.int version )
                            , ( "checksum", Encode.int checksum )
                            , ( "game", GameInternal.runningGameValue grid gameModel )
                            ]
                            |> Encode.encode 0
                            |> Just

                    _ ->
                        Nothing
            )


resumeInvariantTests : Test
resumeInvariantTests =
    describe "Resuming a game keeps the pause/segment invariant"
        [ test "resuming a paused game sets the resume counter one ahead of the recorded segments" <|
            \_ ->
                case runningGameModel of
                    Nothing ->
                        Expect.fail "Testdata seems to be invalid"

                    Just gameModel ->
                        { gameModel
                            | gamePauseResumeState = Paused
                            , gameRunningTimes =
                                [ ( Time.millisToPosix 1000, Time.millisToPosix 5000 )
                                , ( Time.millisToPosix 7000, Time.millisToPosix 9000 )
                                ]
                        }
                            |> Game.resumeGame
                            |> .gamePauseResumeState
                            |> Expect.equal (Resumed 3)
        , test "resuming an already running game changes nothing" <|
            \_ ->
                case runningGameModel of
                    Nothing ->
                        Expect.fail "Testdata seems to be invalid"

                    Just gameModel ->
                        Game.resumeGame gameModel
                            |> Expect.equal gameModel
        ]


snapshotScopeTests : Test
snapshotScopeTests =
    describe "Only running games are persisted"
        [ test "a game waiting on the first click is not encoded" <|
            \_ ->
                { gameBoardStatus = WaitOnStart { grid = Grid.repeat 4 4 InitGameCell, mines = 3 }
                , gameInteractionMode = Reveal
                , gameRunningTimes = []
                , gamePauseResumeState = Paused
                , lastClockTick = Time.millisToPosix 0
                }
                    |> GameInternal.encodeRunningGame testSalt
                    |> Expect.equal Nothing
        , test "a finished game is not encoded" <|
            \_ ->
                case runningGameModel of
                    Nothing ->
                        Expect.fail "Testdata seems to be invalid"

                    Just gameModel ->
                        case gameModel.gameBoardStatus of
                            RunningGame grid ->
                                { gameModel | gameBoardStatus = FinishedGame grid Lost 4000 }
                                    |> GameInternal.encodeRunningGame testSalt
                                    |> Expect.equal Nothing

                            _ ->
                                Expect.fail "Testdata seems to be invalid"
        ]
