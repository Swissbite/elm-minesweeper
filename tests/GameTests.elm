module GameTests exposing (gameLogic)

import Expect
import Fuzz exposing (int, tuple3)
import Game exposing (adjustGameDefinition)
import Test exposing (Test, describe, fuzzWith)

gameLogic: Test
gameLogic = fuzzWith {runs = 300} (tuple3 (int, int, int)) "any playground definition should be sanitized" <|
            \(width, height, mines) -> {dimensionX = width, dimensionY = height, amountOfMines = mines}
              |> Debug.log "Fuzz generated game definition\t"
              |> adjustGameDefinition
              |> Debug.log "Sanitized game definition\t"
              |> Expect.all
                  [ \gameDefinition -> Expect.atLeast 4 gameDefinition.dimensionX
                  , \gameDefinition -> Expect.atLeast 4 gameDefinition.dimensionY
                  , \gameDefinition -> Expect.atLeast 1 gameDefinition.amountOfMines
                  , \gameDefinition -> Expect.atMost (gameDefinition.dimensionX * gameDefinition.dimensionY - 9) gameDefinition.amountOfMines
                  ]

