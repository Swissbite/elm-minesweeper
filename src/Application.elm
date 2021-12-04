module Application exposing (..)

{-| Root definition for all shared types. Solves circular dependencies between Main.elm and Game.elm


## Used mostly in Main.elm

@docs Msg, Model, ActiveScreen


## Used mostly in Game.elm

@docs GameUpdateMsg, GameStatus, PlayGroundDefinition

-}

import Element as Element
import Element.Background as Background
import Element.Border as Border
import Framework.Color as Color
import Grid exposing (Grid)
import Random exposing (Seed)
import Time


icons =
    { markerFlag = "⚑"
    , untouchedBomb = "💣"
    , exploded = "💥"
    , stopWatch = "⏱️"
    }


basicMineCellStyle : List (Element.Attribute msg)
basicMineCellStyle =
    [ Element.width <| Element.px 40
    , Element.height <| Element.px 40
    , Background.color Color.grey_lighter
    , Border.color Color.grey
    , Border.solid
    , Border.width 1
    ]


emptyCellStyle : List (Element.Attribute msg)
emptyCellStyle =
    basicMineCellStyle ++ [ Background.color Color.white ]


{-| Nothing to say
-}
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


{-| All messages for anything within the game board. Represents messages for cell clicks, creation of games, running a game etc.
-}
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
