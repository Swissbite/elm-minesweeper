module Game exposing (PlayGroundDefinition, GameStatus(..), adjustGameDefinition, interactWithGame, GameUpdateMsg(..), FinishStatus(..), GameModel)

{-
  This module is for the game view itself. While the game is running, this module should handle all the state update. This may simplify the code.
  It is "just" responsible for the game view and the right sidebar.
-}

import Array exposing (Array)
import Time


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
    ClickCell coordinates -> clickOnCell gameStatus coordinates
    Tick time -> updateTime gameStatus time

togglePause: GameStatus -> GameStatus
togglePause gameStatus =
  case gameStatus of
    RunningGame gameModel -> PausedGame gameModel
    PausedGame gameModel -> RunningGame gameModel
    others -> others

clickOnCell: GameStatus -> CellCoordinates -> GameStatus
clickOnCell gameStatus cellCoordinates =
  case gameStatus of
    RunningGame gameModel -> Debug.todo "Implement cell handling"
    InitGame playgroundDefinition -> Debug.todo "Start game"
    noOp -> noOp

updateTime: GameStatus -> Time.Posix -> GameStatus
updateTime gameStatus time =
    case gameStatus of
        RunningGame gameModel -> Debug.todo "Update tick"
        other -> other

type GameUpdateMsg = CreateGame PlayGroundDefinition | ClickCell CellCoordinates | TogglePause | Tick Time.Posix

type alias PlayGroundDefinition =
  { dimensionX: Int
  , dimensionY: Int
  , amountOfMines: Int
  }

type GameStatus = NoGame | InitGame PlayGroundDefinition | RunningGame GameModel | FinishedGame FinishStatus GameModel | PausedGame GameModel

type alias GameDurationInSeconds = Int

type FinishStatus = Won GameDurationInSeconds | Lost GameDurationInSeconds

type alias GameModel =
    { board: Array (Array Cell)
    , elapsedTime: Int
    }

type alias CellCoordinates =
  { rowIndex: Int
  , columnIndex: Int
  }

type alias Cell =
    { cellType: GameCellType
    , status: GameCellStatus
    , coordinate: CellCoordinates
    }

type GameCellType = Mine | EmptyCell | MinesAround Int

type GameCellStatus = Closed | Marked | Open
