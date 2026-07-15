{-
   This file is part of Elm Minesweeper.

   Elm Minesweeper is free software: you can redistribute it and/or modify it under
   the terms of the GNU Affero General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later version.

   Elm Minesweeper is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
   PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License along with
   Elm Minesweeper. If not, see <https://www.gnu.org/licenses/>.

-}


module Help exposing (view)

{-| Help page that explains how to play Minesweeper. Renders visual cell
examples using the same Styles functions used in the real game so that the
illustrations match what the player will actually see.
-}

import Colors
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Routing exposing (githubPagePathPrefix)
import Styles
import Theme exposing (Theme)
import Types exposing (Model)


exampleCellSize : Int
exampleCellSize =
    40


view : Model -> Element msg
view model =
    let
        theme =
            model.theme

        homeUrl =
            if model.containsGithubPrefixInPath then
                "/" ++ githubPagePathPrefix ++ "/"

            else
                "/"
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.scrollbarY
        ]
        [ Element.column
            [ Element.width (Element.maximum 720 Element.fill)
            , Element.centerX
            , Element.padding 24
            , Element.spacing 40
            ]
            [ -- Page title
              Element.column [ Element.spacing 8, Element.width Element.fill ]
                [ Element.el [ Font.bold, Font.size 32 ] <| Element.text "How to Play Minesweeper"
                , Element.paragraph [ Font.color (Colors.textDim theme) ]
                    [ Element.text "A complete guide for new and returning players." ]
                ]

            -- Section: Objective
            , section theme
                "🎯 Objective"
                [ Element.paragraph []
                    [ Element.text "The goal is to reveal every cell on the board that does "
                    , Element.el [ Font.bold ] <| Element.text "not"
                    , Element.text " contain a mine — without clicking on a mine. Use the numbers revealed on cells to deduce where the mines are hiding."
                    ]
                ]

            -- Section: The board
            , section theme
                "🗺️ The Board"
                [ Element.paragraph []
                    [ Element.text "The board is a grid of hidden cells. Each cell is one of five kinds:" ]
                , cellTypesTable theme
                ]

            -- Section: Controls
            , section theme
                "🖱️ Controls"
                [ controlsTable theme ]

            -- Section: How to win
            , section theme
                "🏆 How to Win"
                [ Element.paragraph []
                    [ Element.text "Reveal every safe cell. You do not need to flag all mines — simply uncover all non-mine cells and the game is won." ]
                , Element.paragraph [ Font.color (Colors.textDim theme) ]
                    [ Element.text "Tip: your very first click is always safe. The board is generated after your first click so that you can never lose immediately." ]
                ]

            -- Section: Strategy tips
            , section theme
                "💡 Strategy Tips"
                [ Element.column [ Element.spacing 12, Element.width Element.fill ]
                    (List.map
                        (\tip ->
                            Element.row [ Element.width Element.fill, Element.spacing 12 ]
                                [ Element.el [ Element.alignTop, Font.color (Colors.primary theme), Font.bold ] <| Element.text "•"
                                , Element.paragraph [ Element.width Element.fill ] [ Element.text tip ]
                                ]
                        )
                        [ "Start with corners and edges — they have fewer neighbours, making deduction easier."
                        , "When a numbered cell has exactly as many flags around it as its number, click it to automatically reveal all remaining hidden neighbours."
                        , "A \"1\" touching only one hidden cell? That cell is a mine — flag it and move on."
                        , "On hard boards a guess is sometimes unavoidable — pick the cell least likely to be a mine."
                        ]
                    )
                ]

            -- Start playing button
            , Element.el [ Element.centerX, Element.paddingXY 0 8 ] <|
                Element.link
                    [ Element.padding 16
                    , Background.color (Colors.primary theme)
                    , Border.rounded 12
                    , Font.color Colors.white
                    , Font.bold
                    , Font.size 18
                    ]
                    { url = homeUrl
                    , label = Element.text "▶ Start Playing"
                    }
            ]
        ]


{-| Renders a titled card section.
-}
section : Theme -> String -> List (Element msg) -> Element msg
section theme title content =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 16
        , Element.padding 20
        , Background.color (Colors.surface theme)
        , Border.rounded 12
        , Border.color (Colors.cellBorderColor theme)
        , Border.width 1
        ]
        (Element.el [ Font.bold, Font.size 20 ] (Element.text title)
            :: content
        )


{-| Table that shows each cell type with a visual example and a description.
-}
cellTypesTable : Theme -> Element msg
cellTypesTable theme =
    let
        cellSize =
            exampleCellSize

        row : Element msg -> String -> String -> Element msg
        row cellEl label description =
            Element.row [ Element.spacing 16, Element.width Element.fill ]
                [ Element.el [ Element.alignTop ] cellEl
                , Element.column [ Element.spacing 4, Element.width Element.fill, Element.alignTop ]
                    [ Element.el [ Font.bold ] <| Element.text label
                    , Element.paragraph [ Element.width Element.fill, Font.alignLeft, Font.color (Colors.textDim theme), Font.size 14 ] [ Element.text description ]
                    ]
                ]
    in
    Element.column [ Element.spacing 16, Element.width Element.fill ]
        [ row
            (untouchedCell theme cellSize)
            "Untouched"
            "A cell you have not yet interacted with. Click (or tap) it to reveal what is underneath."
        , row
            (openedEmptyCell theme cellSize)
            "Empty (safe)"
            "A revealed cell with no mines nearby. Clicking an empty cell automatically reveals all its safe neighbours."
        , row
            (numberCell theme cellSize 1)
            "Number (1 – 8)"
            "A revealed cell showing how many of its 8 neighbours contain a mine. Use these numbers to locate mines."
        , row
            (flaggedCell theme cellSize)
            "Flagged"
            "A cell you have marked as a suspected mine. Flagged cells cannot be accidentally revealed."
        , row
            (mineCell theme cellSize)
            "Mine 💥"
            "A cell containing a mine. Revealing this ends the game immediately — avoid it!"
        ]


{-| Explains controls using a mode-based description instead of a misleading
left/right-click table. The game uses a single click/tap whose effect is
determined by the active mode.
-}
controlsTable : Theme -> Element msg
controlsTable theme =
    Element.column [ Element.width Element.fill, Element.spacing 12 ]
        [ Element.paragraph [ Element.width Element.fill, Font.alignLeft ]
            [ Element.text "Use a normal click or tap on a cell. The active "
            , Element.el [ Font.bold ] <| Element.text "mode"
            , Element.text " decides what that click does:"
            ]
        , Element.column [ Element.width Element.fill, Element.spacing 8 ]
            [ Element.paragraph [ Element.width Element.fill, Font.alignLeft ]
                [ Element.el [ Font.bold ] <| Element.text "Reveal mode:"
                , Element.text " click or tap uncovers the cell."
                ]
            , Element.paragraph [ Element.width Element.fill, Font.alignLeft ]
                [ Element.el [ Font.bold ] <| Element.text "Flag mode:"
                , Element.text " click or tap places or removes a flag."
                ]
            ]
        , Element.paragraph [ Element.width Element.fill, Font.alignLeft, Font.color (Colors.textDim theme), Font.size 14 ]
            [ Element.text "Switch modes with the in-game toggle button or press "
            , Element.el [ Font.bold, Font.color (Colors.primary theme) ] <| Element.text "T"
            , Element.text "."
            ]
        ]



-- ---- Small cell helpers ----


untouchedCell : Theme -> Int -> Element msg
untouchedCell theme size =
    Element.el (Styles.untouchedCellStyle theme size) Element.none


openedEmptyCell : Theme -> Int -> Element msg
openedEmptyCell theme size =
    Element.el (Styles.openedCellStyle theme size) Element.none


numberCell : Theme -> Int -> Int -> Element msg
numberCell theme size n =
    Element.el
        (Styles.openedMineNeighbourCellStyle theme size n
            ++ [ Element.centerX, Font.center ]
        )
    <|
        Element.el [ Element.centerX, Element.centerY ] <|
            Element.text (String.fromInt n)


flaggedCell : Theme -> Int -> Element msg
flaggedCell theme size =
    Element.el
        (Styles.untouchedCellStyle theme size
            ++ [ Font.center ]
        )
    <|
        Element.el [ Element.centerX, Element.centerY ] <|
            Element.text (String.fromChar Styles.icons.markerFlag)


mineCell : Theme -> Int -> Element msg
mineCell theme size =
    Element.el
        (Styles.openedCellStyle theme size
            ++ [ Font.center, Background.color (Colors.danger theme) ]
        )
    <|
        Element.el [ Element.centerX, Element.centerY ] <|
            Element.text (String.fromChar Styles.icons.untouchedBomb)
