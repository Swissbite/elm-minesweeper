module Game exposing (PlayGroundDefinition, GameStatus(..), adjustGameDefinition, interactWithGame, FinishStatus(..), GameModel, gameBoardView)

{-|
  This module is for the game view itself. While the game is running, this module should handle all the state update. This may simplify the code.
  It is "just" responsible for the game view and the right sidebar.
  @see gameBoardView
-}

import Array exposing (Array)
import Element exposing (Color, Element, centerX, centerY, column, el, fill, height, padding, row, spacing, text, width)
import Element.Background as Background
import Application exposing (..)
import Element.Input exposing (button)
import Framework.Color exposing (grey_lighter, white_ter)
import Time

{-|Exposed function to render the main game view depending on the current GameStatus
-}
gameBoardView: GameStatus -> Element Msg
gameBoardView gameStatus = case gameStatus of
    NoGame -> gameBoardViewNoStatus
    _ -> el [centerX, centerY] <| text "Game Area from game"

gameBoardViewNoStatus: Element Msg
gameBoardViewNoStatus =
    let
        columnStyled = column [width fill, height fill, spacing 10]
        buttonStyled = button [height fill, width fill , centerY, centerX, Background.color grey_lighter]
    in row [width fill, height fill, padding 10, spacing 10, Background.color white_ter]
        [ columnStyled
            [ buttonStyled {onPress = Just (GameMsg <| CreateGame {amountOfMines = 10, dimensionX = 10, dimensionY = 10}), label = text "Small Game"}
            , buttonStyled {onPress = Nothing, label = text "Large Game"}
            ]
        , columnStyled
            [ buttonStyled {onPress = Nothing, label = text "Medium Game"}
            , buttonStyled {onPress = Nothing, label = text "Custom Game"}
            ]
        ]

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
    CustomGameDefinition -> ViewCustomGameDefinitionSetup

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


type alias PlayGroundDefinition =
  { dimensionX: Int
  , dimensionY: Int
  , amountOfMines: Int
  }

type GameStatus = NoGame | ViewCustomGameDefinitionSetup | InitGame PlayGroundDefinition | RunningGame GameModel | FinishedGame FinishStatus GameModel | PausedGame GameModel

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
