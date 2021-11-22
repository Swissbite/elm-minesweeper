module Game exposing (PlayGroundDefinition, GameStatus(..), adjustGameDefinition, interactWithGame, GameUpdateMsg(..), FinishStatus(..), GameField)
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

interactWithGame: GameUpdateMsg -> GameStatus -> GameStatus
interactWithGame updateMsg gameStatus =
  case updateMsg of
    TogglePause -> togglePause gameStatus
    CreateGame definition -> adjustGameDefinition definition |> InitGame
    _ -> Debug.todo "Handle ClickCell"

togglePause: GameStatus -> GameStatus
togglePause gameStatus =
  case gameStatus of
    RunningGame field -> PausedGame field
    PausedGame field -> RunningGame field
    others -> others

type GameUpdateMsg = CreateGame PlayGroundDefinition | ClickCell CellCoordinates | TogglePause

type alias PlayGroundDefinition =
  { dimensionX: Int
  , dimensionY: Int
  , amountOfMines: Int
  }

type GameStatus = NoGame | InitGame PlayGroundDefinition | RunningGame GameField | FinishedGame FinishStatus GameField | PausedGame GameField

type alias GameDurationInSeconds = Int

type FinishStatus = Won GameDurationInSeconds | Lost GameDurationInSeconds

type alias GameField = {}

type alias CellCoordinates =
  { rowIndex: Int
  , columnIndex: Int
  }

type GameCellType = Mine | EmptyCell | MinesAround Int

type GameCellStatus = Closed | Marked | Open
