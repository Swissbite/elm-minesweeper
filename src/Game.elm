module Game exposing (PlayGroundDefinition, GameStatus(..))
{-
  This module is for the game view itself. While the game is running, this module should handle all the state update. This may simplify the code.
  It is "just" responsible for the game view and the right sidebar.
-}

adjustGameDefinition: PlayGroundDefinition -> PlayGroundDefinition
adjustGameDefinition initGameDefinition =
  let
    dimensionX = if (initGameDefinition.dimensionX < 4) then 4 else initGameDefinition.dimensionX
    dimensionY = if (initGameDefinition.dimensionY < 4) then 4 else initGameDefinition.dimensionY
    fields = dimensionX * dimensionY
    maxMines = fields - 9
    minMines = 1
    amountOfMines =
      if (initGameDefinition.amountOfMines < minMines)
      then minMines
      else if (initGameDefinition.amountOfMines > maxMines)
      then maxMines
      else initGameDefinition.amountOfMines
  in {dimensionX = dimensionX, dimensionY = dimensionY, amountOfMines = amountOfMines}

{-
  The initial play ground definition, can be set on start a game. The function adjustGameDefinition adjusts the input values to a valid setup.
  A game field should be at least 4 x 4, have ad least 1 mine and not more than the size of the fields - 9
-}

type alias PlayGroundDefinition =
  { dimensionX: Int
  , dimensionY: Int
  , amountOfMines: Int
  }

type GameStatus = NoGame | InitGame PlayGroundDefinition | RunningGame GameField | FinishedGame FinishStatus GameField | PausedGame GameField

type alias GameDurationInSeconds = Int

type FinishStatus = Won GameDurationInSeconds | Lost GameDurationInSeconds

type alias GameField = {}

type GameCellType = Mine | EmptyCell | MinesAround Int

type GameCellStatus = Closed | Marked | Open
