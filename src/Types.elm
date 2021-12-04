module Types exposing (..)

import Grid exposing (Grid)
import Set exposing (Set)


{-| Export of all Types and Type definitions in this file helps to group Typings and asdf
-}
type Msg
    = Msg
    | ClickedOnInitGameCell PlayGroundDefinition Coordinates
    | StartGame PlayGameGrid


type alias Model =
    { gameBoardStatus : GameBoardStatus
    }


type alias PlayGroundDefinition =
    { rows : Int
    , cols : Int
    , mines : Int
    }


type alias Coordinates =
    { x : Int
    , y : Int
    }


type GameBoardStatus
    = WaitOnStart InitGameGrid
    | RunningGame PlayGameGrid
    | FinishedGame PlayGameGrid GameResult


type alias InitGameGrid =
    { grid : Grid InitGameCell
    , mines : Int
    }


type alias Mines =
    Int


type alias PlayGameGrid =
    Grid GameCell


type InitGameCell
    = InitGameCell


type CellType
    = EmptyCell
    | MineCell
    | MineNeighbourCell AmountOfNeighbours


type CellStatus
    = Opened
    | Untouched
    | Flagged


type AmountOfNeighbours
    = OneMine
    | TwoMines
    | ThreeMines
    | FourMines
    | FiveMines
    | SixMines
    | SevenMines
    | EightMines


type GameCell
    = CellType CellStatus


type CellClickMode
    = Reveal
    | Flag


type GameResult
    = Won
    | Lost
