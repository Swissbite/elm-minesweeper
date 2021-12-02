module Application exposing (..)

{-| Root definition for all shared types. Solves circular dependencies between Main.elm and Game.elm


## Used mostly in Main.elm

@docs Msg, Model, ActiveScreen


## Used mostly in Game.elm

@docs GameUpdateMsg, GameStatus, PlayGroundDefinition

-}

import Grid exposing (Grid)
import Random exposing (Seed)
import Time


icons =
    { markerFlag = "⚑"
    , untouchedBomb = "💣"
    , exploded = "💥"
    , stopWatch = "⏱️"
    }


type Msg
    = GameMsg GameUpdateMsg
    | ChangeScreen ActiveScreen


type alias Model =
    { gameStatus : GameStatus
    , activeSceen : ActiveScreen
    , initialSeed : Seed
    }


type GameStatus
    = NoGame
    | ViewCustomGameDefinitionSetup
    | InitGame PlayGroundDefinition
    | RunningGame GameModel
    | FinishedGame FinishStatus GameModel
    | PausedGame GameModel


type GameUpdateMsg
    = CreateGame PlayGroundDefinition
    | CustomGameDefinition
    | ClickCell CellCoordinates
    | TogglePause
    | Tick Time.Posix


type ActiveScreen
    = GameScreen
    | GameHistoryScreen
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

{-| Represents if the game has ben won or lost and how long it took to win or loose the game.
-}
type FinishStatus
    = Won GameDurationInSeconds
    | Lost GameDurationInSeconds


type alias GameModel =
    { board : Grid Cell
    , elapsedTime : Int
    }


type alias Cell =
    { cellType : GameCellType
    , status : GameCellStatus
    , coordinate : CellCoordinates
    }


type GameCellType
    = Mine
    | EmptyCell
    | MinesAround Int


type GameCellStatus
    = Closed
    | Marked
    | Open


type alias GameDurationInSeconds =
    Int
