module Application exposing (..)
import Time
type Msg = GameMsg GameUpdateMsg

type GameUpdateMsg = CreateGame PlayGroundDefinition | ClickCell CellCoordinates | TogglePause | Tick Time.Posix


type alias PlayGroundDefinition =
  { dimensionX: Int
  , dimensionY: Int
  , amountOfMines: Int
  }
type alias CellCoordinates =
  { rowIndex: Int
  , columnIndex: Int
  }