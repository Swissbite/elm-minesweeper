module Application exposing (..)

import Time


type Msg
    = GameMsg GameUpdateMsg
    | ChangeScreen ActiveScreen


type GameUpdateMsg
    = CreateGame PlayGroundDefinition
    | CustomGameDefinition
    | ClickCell CellCoordinates
    | TogglePause
    | Tick Time.Posix


type ActiveScreen
    = GameScreen
    | GameHistroyScreen
    | AboutScreen


type alias PlayGroundDefinition =
    { dimensionX : Int
    , dimensionY : Int
    , amountOfMines : Int
    }


type alias CellCoordinates =
    { rowIndex : Int
    , columnIndex : Int
    }
