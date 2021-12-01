module Game exposing
    ( gameBoardView
    , interactWithGame
    , FinishStatus(..), GameModel, GameStatus(..), PlayGroundDefinition
    )

{-| The Game module is responsible for handling the game view and exports the view function and the update function to
handle the all states for a game.

#View

@docs gameBoardView

#Update

@docs interactWithGame

#Type definitions

@docs FinishStatus, GameModel, GameStatus, PlayGroundDefinition

-}

import Application exposing (..)
import Array exposing (Array)
import Element exposing (Color, Element, centerX, centerY, column, el, fill, height, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (button)
import Framework.Color exposing (grey_lighter, white_ter)
import Time


{-| Exposed function to render the main game view depending on the current GameStatus
-}
gameBoardView : GameStatus -> Element Msg
gameBoardView gameStatus =
    case gameStatus of
        NoGame ->
            gameBoardViewNoStatus

        ViewCustomGameDefinitionSetup ->
            Debug.todo "Implement view for custom game definition"

        InitGame definition ->
            row [ Element.width fill, Element.height fill ]
                [ rightStatusBarView Nothing Nothing definition.amountOfMines ]

        _ ->
            el [ centerX, centerY ] <| text "Game Area from game"


gameBoardViewNoStatus : Element Msg
gameBoardViewNoStatus =
    let
        columnStyled : List (Element msg) -> Element msg
        columnStyled =
            column [ width fill, height fill, spacing 10 ]

        buttonStyled :
            { onPress : Maybe msg
            , label : Element msg
            }
            -> Element msg
        buttonStyled =
            button [ height fill, width fill, centerY, centerX, Background.color grey_lighter ]

        createLabel : Maybe PlayGroundDefinition -> Element msg
        createLabel maybeDefinition =
            let
                styledLabelColumn : List (Element msg) -> Element msg
                styledLabelColumn =
                    Element.column [ width fill, height fill ]

                styledHeadlineText : String -> Element msg
                styledHeadlineText =
                    \t -> Element.el [ Font.bold, centerY, centerX ] <| text t

                styledLowerlineText : String -> Element msg
                styledLowerlineText =
                    \t -> Element.el [ centerY, centerX ] <| text t
            in
            case maybeDefinition of
                Just definition ->
                    styledLabelColumn
                        [ styledHeadlineText <| String.concat [ String.fromInt definition.dimensionX, " x ", String.fromInt definition.dimensionY ]
                        , styledLowerlineText <| String.concat [ String.fromInt definition.amountOfMines, "Mines" ]
                        ]

                Nothing ->
                    styledLabelColumn
                        [ styledHeadlineText "?"
                        , styledLowerlineText "User defined"
                        ]
    in
    row [ width fill, height fill, padding 10, spacing 10, Background.color white_ter ]
        [ columnStyled
            [ buttonStyled { onPress = Just (GameMsg <| CreateGame defaultGameDefinitions.smallGame), label = createLabel <| Just defaultGameDefinitions.smallGame }
            , buttonStyled { onPress = Just (GameMsg <| CreateGame defaultGameDefinitions.largeGame), label = createLabel <| Just defaultGameDefinitions.largeGame }
            ]
        , columnStyled
            [ buttonStyled { onPress = Just (GameMsg <| CreateGame defaultGameDefinitions.mediumGame), label = createLabel <| Just defaultGameDefinitions.mediumGame }
            , buttonStyled { onPress = Just <| GameMsg CustomGameDefinition, label = createLabel Nothing }
            ]
        ]


defaultGameDefinitions : { smallGame : PlayGroundDefinition, mediumGame : PlayGroundDefinition, largeGame : PlayGroundDefinition }
defaultGameDefinitions =
    { smallGame = { amountOfMines = 10, dimensionX = 8, dimensionY = 8 }
    , mediumGame = { amountOfMines = 40, dimensionX = 16, dimensionY = 16 }
    , largeGame = { amountOfMines = 99, dimensionX = 30, dimensionY = 16 }
    }


rightStatusBarView : Maybe Int -> Maybe Int -> Int -> Element msg
rightStatusBarView flags elapsedTime amountOfMines =
    let
        timeText : String
        timeText =
            case elapsedTime of
                Nothing ->
                    "--:--"

                Just seconds ->
                    String.concat [ String.fromInt <| seconds // 60, ":", String.fromInt <| modBy 60 seconds ]

        missingFlags : String
        missingFlags =
            case flags of
                Nothing ->
                    String.concat <| List.append [ "-" ] <| List.repeat calculatePrefixZeroOfFlags "-"

                Just f ->
                    String.concat
                        [ String.concat <| List.repeat calculatePrefixZeroOfFlags "0"
                        , String.fromInt <| amountOfMines - f
                        ]

        calculatePrefixZeroOfFlags : Int
        calculatePrefixZeroOfFlags =
            let
                getBase10Truncated : Int -> Int
                getBase10Truncated =
                    \n -> truncate <| logBase 10 <| toFloat n

                amountOfMinesLogBase10 : Int
                amountOfMinesLogBase10 =
                    getBase10Truncated amountOfMines

                amountOfFlagsLogBase10 : Int
                amountOfFlagsLogBase10 =
                    case flags of
                        Nothing ->
                            1

                        Just 0 ->
                            1

                        Just f ->
                            getBase10Truncated f
            in
            amountOfMinesLogBase10 - amountOfFlagsLogBase10
    in
    Element.column [ width (Element.maximum 150 fill), Element.padding 10, Element.alignRight, Element.alignTop ]
        [ Element.el [ Font.bold, Font.size 30, Element.centerX ] <| text Application.icons.stopWatch
        , Element.el [ Element.centerX, Element.paddingXY 0 10 ] <| text timeText
        , Element.el [ Font.bold, Element.paddingEach { bottom = 0, top = 30, left = 0, right = 0 }, Font.size 30, Element.centerX ] <| text Application.icons.markerFlag
        , Element.el [ Element.centerX, Element.paddingXY 0 10 ] <| text missingFlags
        ]


{-| The initial play ground definition, can be set on start a game. The function adjustGameDefinition adjusts the input values to a valid setup.
A game field should be at least 4 x 4, have ad least 1 mine and not more than the size of the fields - 9

    adjustGameDefinition { dimensionX = 1, dimensionY = 1, amountOfMines = 99 } == { dimensionX = 4, dimensionY = 4, amountOfMines = 7 }

    adjustGameDefinition { dimensionX = 1, dimensionY = 1, amountOfMines = -99 } == { dimensionX = 4, dimensionY = 4, amountOfMines = 1 }

-}
adjustGameDefinition : PlayGroundDefinition -> PlayGroundDefinition
adjustGameDefinition initGameDefinition =
    let
        dimensionX =
            if initGameDefinition.dimensionX < 4 then
                4

            else
                initGameDefinition.dimensionX

        dimensionY =
            if initGameDefinition.dimensionY < 4 then
                4

            else
                initGameDefinition.dimensionY

        fields =
            dimensionX * dimensionY

        maxMines =
            fields - 9

        minMines =
            1

        amountOfMines =
            if initGameDefinition.amountOfMines < minMines then
                minMines

            else if initGameDefinition.amountOfMines > maxMines then
                maxMines

            else
                initGameDefinition.amountOfMines
    in
    { dimensionX = dimensionX, dimensionY = dimensionY, amountOfMines = amountOfMines }


interactWithGame : GameUpdateMsg -> GameStatus -> GameStatus
interactWithGame updateMsg gameStatus =
    case updateMsg of
        TogglePause ->
            togglePause gameStatus

        CreateGame definition ->
            adjustGameDefinition definition |> InitGame

        ClickCell coordinates ->
            clickOnCell gameStatus coordinates

        Tick time ->
            updateTime gameStatus time

        CustomGameDefinition ->
            ViewCustomGameDefinitionSetup


togglePause : GameStatus -> GameStatus
togglePause gameStatus =
    case gameStatus of
        RunningGame gameModel ->
            PausedGame gameModel

        PausedGame gameModel ->
            RunningGame gameModel

        others ->
            others


clickOnCell : GameStatus -> CellCoordinates -> GameStatus
clickOnCell gameStatus cellCoordinates =
    case gameStatus of
        RunningGame gameModel ->
            Debug.todo "Implement cell handling"

        InitGame playgroundDefinition ->
            Debug.todo "Start game"

        noOp ->
            noOp


updateTime : GameStatus -> Time.Posix -> GameStatus
updateTime gameStatus time =
    case gameStatus of
        RunningGame gameModel ->
            Debug.todo "Update tick"

        other ->
            other


type alias PlayGroundDefinition =
    { dimensionX : Int
    , dimensionY : Int
    , amountOfMines : Int
    }


type GameStatus
    = NoGame
    | ViewCustomGameDefinitionSetup
    | InitGame PlayGroundDefinition
    | RunningGame GameModel
    | FinishedGame FinishStatus GameModel
    | PausedGame GameModel


type alias GameDurationInSeconds =
    Int


{-| Represents if the game has ben won or lost and how long it took to win or loose the game.
-}
type FinishStatus
    = Won GameDurationInSeconds
    | Lost GameDurationInSeconds


type alias GameModel =
    { board : Array (Array Cell)
    , elapsedTime : Int
    }


type alias CellCoordinates =
    { rowIndex : Int
    , columnIndex : Int
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
