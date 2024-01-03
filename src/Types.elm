module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Element exposing (Device)
import Grid exposing (Grid)
import Time


{-| Export of all Types and Type definitions in this file helps to group Typings
-}
type Msg
    = GameView GameMsg
    | GameHistory GameHistoryMsg
    | Navigation UrlRequest
    | SetScreenSize Int Int


type alias Flags =
    { height : Int
    , width : Int
    , history : String
    , initPath : String
    }


{-| Type definition of all Messages for playing minesweeper itself. Moved from Msg to own type to simplify the update function in Main.elm
Prefixed in Msg by GameView, so that the update function in Main.elm can make an simple case match and call the update function in Game.elm
-}
type GameMsg
    = ClickedOnInitGameCell InitGameData Coordinates
    | StartGame PlayGameGrid
    | ClickOnGameCell Coordinates
    | ToogleGameCellInteractionMode
    | CreateNewGame PlayGroundDefinition
    | GoToStartPage
    | ClockTick Time.Posix
    | ToogleGamePause
    | NavigationEvent View
    | NoUpdate


type GameHistoryMsg
    = DeleteAll
    | DeleteLost
    | SetDisplayMode GameHistoryDisplayMode
    | SetOrderBy GameHistoryOrderBy OrderDirection


type GameHistoryDisplayMode
    = DisplayAll
    | DisplayLost
    | DisplayWon


type GameHistoryOrderBy
    = ByDuration
    | ByFieldSize
    | ByPosix
    | ByResult
    | ByMines


type OrderDirection
    = Ascending
    | Descending


type alias Model =
    { game : GameModel
    , playedGameHistory : List FinishedGameHistoryEntry
    , currentView : View
    , device : Device
    , key : Key
    , containsGithubPrefixInPath : Bool
    }


type alias GameModel =
    { gameBoardStatus : GameBoardStatus
    , gameInteractionMode : CellClickMode
    , gameRunningTimes : List ( Time.Posix, Time.Posix )
    , gamePauseResumeState : PauseResumeState
    , lastClockTick : Time.Posix
    }


type View
    = Game
    | History GameHistoryDisplayMode GameHistoryOrderBy OrderDirection
    | Error404


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


type alias FinishedGameHistory =
    { entries : List FinishedGameHistoryEntry
    , version : Int
    }


type alias FinishedGameHistoryEntry =
    { grid : PlayGameGrid
    , result : GameResult
    , duration : Int
    , playFinish : Time.Posix
    }


type GameBoardStatus
    = WaitOnStart InitGameData
    | RunningGame PlayGameGrid
    | FinishedGame PlayGameGrid GameResult Int
    | NoGame NoGameMode


type NoGameMode
    = PreSelect
    | Custom


type alias InitGameData =
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
