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


module ResumeSelectionTests exposing (..)

{-| Regression tests for the start page "Resume game" tile.

The tile used to read only `model.savedGame`, which is populated from local
storage once on startup. A game the player left via the nav bar or the help
page stayed in `model.game` and never reached `model.savedGame`, so the tile
only appeared after a hard reload. `Game.resumableGame` reconciles both
fields; these tests pin down which games are offered for resuming.

-}

import Expect
import Game.Game as Game
import Grid
import Test exposing (..)
import Time
import Types exposing (..)


sampleGrid : PlayGameGrid
sampleGrid =
    Grid.repeat 2 2 (GameCell EmptyCell Untouched)


{-| A running game as it looks after the player left it: paused, with a couple
of recorded time segments.
-}
runningGame : GameModel
runningGame =
    { gameBoardStatus = RunningGame sampleGrid
    , gameInteractionMode = Reveal
    , gameRunningTimes = [ ( Time.millisToPosix 1000, Time.millisToPosix 5000 ) ]
    , gamePauseResumeState = Paused
    , lastClockTick = Time.millisToPosix 5000
    }


waitOnStartGame : GameModel
waitOnStartGame =
    { gameBoardStatus = WaitOnStart { grid = Grid.repeat 4 4 InitGameCell, mines = 3 }
    , gameInteractionMode = Reveal
    , gameRunningTimes = []
    , gamePauseResumeState = Paused
    , lastClockTick = Time.millisToPosix 0
    }


finishedGame : GameModel
finishedGame =
    { runningGame | gameBoardStatus = FinishedGame sampleGrid Lost 4000 }


resumableGameTests : Test
resumableGameTests =
    describe "Game.resumableGame decides what the start page offers to resume"
        [ test "offers nothing when neither a live nor a stored game exists" <|
            \_ ->
                Game.resumableGame Nothing Nothing
                    |> Expect.equal Nothing
        , test "offers a running game restored from storage after a reload" <|
            \_ ->
                Game.resumableGame Nothing (Just runningGame)
                    |> Expect.equal (Just runningGame)
        , test "offers the live running game even when nothing was stored yet (the reported bug)" <|
            \_ ->
                -- The player started a game, then left to the help page and hit
                -- "Start Playing": the game lives in model.game, savedGame is Nothing.
                Game.resumableGame (Just runningGame) Nothing
                    |> Expect.equal (Just runningGame)
        , test "does not offer a live game that is still waiting on the first click" <|
            \_ ->
                Game.resumableGame (Just waitOnStartGame) Nothing
                    |> Expect.equal Nothing
        , test "does not offer a finished live game" <|
            \_ ->
                Game.resumableGame (Just finishedGame) Nothing
                    |> Expect.equal Nothing
        , test "does not offer a finished stored game" <|
            \_ ->
                Game.resumableGame Nothing (Just finishedGame)
                    |> Expect.equal Nothing
        , test "prefers the live running game over a stored one" <|
            \_ ->
                let
                    storedGame =
                        { runningGame | gameInteractionMode = Flag }
                in
                Game.resumableGame (Just runningGame) (Just storedGame)
                    |> Expect.equal (Just runningGame)
        , test "falls back to the stored game when the live game is not resumable" <|
            \_ ->
                Game.resumableGame (Just waitOnStartGame) (Just runningGame)
                    |> Expect.equal (Just runningGame)
        ]
