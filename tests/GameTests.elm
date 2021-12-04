module GameTests exposing (fuzzSanitizeGameDefinition, enforceEdgeCaseSanitize, interactionTests)

import Expect
import Fuzz exposing (int, tuple3)
import Game exposing (adjustGameDefinition)
import Application exposing(FinishStatus(..), GameModel, GameStatus(..), GameUpdateMsg(..), PlayGroundDefinition)
import Test exposing (Test, describe, fuzzWith, test)

fuzzSanitizeGameDefinition: Test
fuzzSanitizeGameDefinition = fuzzWith {runs = 30000} (tuple3 (int, int, int)) "any playground definition should be sanitized" <|
            \(width, height, mines) -> createGameDefinition width height mines
              -- |> Debug.log "Fuzz generated game definition\t"
              |> adjustGameDefinition
              -- |> Debug.log "Sanitized game definition\t"
              |> Expect.all
                  [ \gameDefinition -> Expect.atLeast 4 gameDefinition.dimensionX
                  , \gameDefinition -> Expect.atLeast 4 gameDefinition.dimensionY
                  , \gameDefinition -> Expect.atLeast 1 gameDefinition.amountOfMines
                  , \gameDefinition -> Expect.atMost (gameDefinition.dimensionX * gameDefinition.dimensionY - 9) gameDefinition.amountOfMines
                  ]

enforceEdgeCaseSanitize: Test
enforceEdgeCaseSanitize = describe "Test min board definition and min / max mines"
                          [ test "At least 4x4 with 1 mine" <|
                              \_  -> createGameDefinition 0 0 0
                                      |> adjustGameDefinition
                                      |> Expect.equal (createGameDefinition 4 4 1)
                          , test "At least 4x4 but max 7 mine" <|
                              \_  -> createGameDefinition 0 0 3333
                                      |> adjustGameDefinition
                                      |> Expect.equal (createGameDefinition 4 4 7)

                          ]

createGameDefinition: Int -> Int -> Int -> PlayGroundDefinition
createGameDefinition dimensionX dimensionY amountOfMines = {dimensionX = dimensionX, dimensionY = dimensionY, amountOfMines = amountOfMines}

interactionTests: Test
interactionTests = describe "Testing the logical interaction with the game"
                    [ test "Toogle Pause on a uninitialized game should have no effect" <|
                        \_ -> Game.interactWithGame TogglePause NoGame
                              |> Expect.equal NoGame
                    , test "Toggle pause on game not yet started should have no effect" <|
                        \_ -> Game.interactWithGame TogglePause (InitGame (createGameDefinition 4 4 1))
                              |> Expect.equal (InitGame (createGameDefinition 4 4 1))
                    , test "Toggle pause on a finished game should have no effect" <|
                        \_ -> let
                                gameStatusWon = FinishedGame (Won 2) createGameFieldForInteractionTesting
                                gameStatusLost = FinishedGame (Lost 1) createGameFieldForInteractionTesting
                              in (Game.interactWithGame TogglePause gameStatusLost, Game.interactWithGame TogglePause gameStatusWon)
                                |> Expect.equal (gameStatusLost, gameStatusWon)
                    , test "Toggle a running game should pause the game" <|
                      \_ -> let
                              gamefield = createGameFieldForInteractionTesting
                            in RunningGame gamefield
                              |> Game.interactWithGame TogglePause
                              |> Debug.log "Toogle"
                              |> Expect.equal (PausedGame gamefield)
                    ]

createGameFieldForInteractionTesting: GameModel
createGameFieldForInteractionTesting = {}
