module GameTests exposing (all)

import Expect
import Fuzz exposing (int, tuple3)
import Game exposing (PlayGroundDefinition, adjustGameDefinition)
import Test exposing (Test, describe, fuzzWith, test)

all: Test
all = describe "Test suite for all game logic tests"
      [ fuzzSanitizeGameDefinition
      , enforceEdgeCaseSanitize
      ]

fuzzSanitizeGameDefinition: Test
fuzzSanitizeGameDefinition = fuzzWith {runs = 300} (tuple3 (int, int, int)) "any playground definition should be sanitized" <|
            \(width, height, mines) -> createGameDefinition width height mines
              |> Debug.log "Fuzz generated game definition\t"
              |> adjustGameDefinition
              |> Debug.log "Sanitized game definition\t"
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
