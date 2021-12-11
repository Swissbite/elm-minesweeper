module Types exposing (..)

import Grid exposing (Grid)
import Time


{-| Export of all Types and Type definitions in this file helps to group Typings
-}
type Msg
    = GameView GameMsg

type GameMsg
    = ClickedOnInitGameCell InitGameGrid Coordinates
    | StartGame PlayGameGrid
    | ClickOnGameCell Coordinates
    | ToogleGameCellInteractionMode
    | CreateNewGame PlayGroundDefinition
    | GoToStartPage
    | ClockTick Time.Posix
    | ToogleGamePause

type alias Model =
    { gameBoardStatus : GameBoardStatus
    , gameInteractionMode : CellClickMode
    , gameRunningTimes : List ( Time.Posix, Time.Posix )
    , gamePauseResumeState : PauseResumeState
    , playedGameHistory : List FinishedGameHistoryEntry
    , currentView: View
    }

type View
    = Game
type PauseResumeState
    = Paused
    | Resumed Int


type alias PlayGroundDefinition =
    { rows : Int
    , cols : Int
    , mines : Int
    }


type alias Coordinates =
    { x : Int
    , y : Int
    }


type FinishedGameHistoryEntry
    = FinishedGameHistoryEntry PlayGameGrid GameResult Int


type GameBoardStatus
    = WaitOnStart InitGameGrid
    | RunningGame PlayGameGrid
    | FinishedGame PlayGameGrid GameResult Int
    | NoGame NoGameMode


type NoGameMode
    = PreSelect
    | Custom


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
    | MineNeighbourCell Int


type CellStatus
    = Opened
    | Untouched
    | Flagged


type GameCell
    = GameCell CellType CellStatus


type CellClickMode
    = Reveal
    | Flag


type GameResult
    = Won
    | Lost
