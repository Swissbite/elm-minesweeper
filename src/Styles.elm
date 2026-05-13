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


module Styles exposing (..)

import Colors
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as HA


icons : { markerFlag : Char, untouchedBomb : Char, exploded : Char, stopWatch : String, victory : Char, downSign : Char, upSign : Char, world : Char, calendar : String, pause : String, resume : String }
icons =
    { markerFlag = '⚑'
    , untouchedBomb = '💣'
    , world = '🌐'
    , exploded = '💥'
    , stopWatch = "⏱️"
    , victory = '🎉'
    , downSign = '🔻'
    , upSign = '🔺'
    , calendar = "🗓️"
    , pause = "⏸️"
    , resume = "▶️"
    }


cellPixelSize : Device -> { cols : Int, rows : Int } -> Int
cellPixelSize device { cols, rows } =
    let
        longestSide =
            max cols rows
    in
    case device.class of
        Phone ->
            if longestSide <= 8 then
                40

            else if longestSide <= 16 then
                34

            else
                32

        Tablet ->
            if longestSide <= 8 then
                42

            else if longestSide <= 16 then
                36

            else
                32

        Desktop ->
            if longestSide <= 8 then
                44

            else if longestSide <= 16 then
                32

            else
                28

        BigDesktop ->
            if longestSide <= 8 then
                46

            else if longestSide <= 16 then
                34

            else
                30


cellWidth : Int -> Element.Length
cellWidth size =
    Element.px size


basicCellStyle : Int -> List (Element.Attribute msg)
basicCellStyle size =
    [ Element.width (cellWidth size)
    , Element.height (cellWidth size)
    , Border.color Colors.cellBorderColor
    , Border.width 1
    , Element.pointer
    , Font.size (max 16 (size // 2))
    , htmlAttribute <| HA.style "touch-action" "manipulation"
    , htmlAttribute <| HA.style "user-select" "none"
    , htmlAttribute <| HA.style "-webkit-tap-highlight-color" "transparent"
    ]


untouchedCellStyle : Int -> List (Element.Attribute msg)
untouchedCellStyle size =
    basicCellStyle size
        ++ [ Background.color Colors.untouchedCellGray
           ]


openedCellStyle : Int -> List (Element.Attribute msg)
openedCellStyle size =
    basicCellStyle size
        ++ [ Background.color Colors.openedCellGray
           ]


openedMineNeighbourCellStyle : Int -> Int -> List (Element.Attribute msg)
openedMineNeighbourCellStyle size number =
    let
        color =
            case number of
                1 ->
                    Colors.saffron

                2 ->
                    Colors.fieryRose

                3 ->
                    Colors.cerise

                4 ->
                    Colors.smitten

                5 ->
                    Colors.eggplant

                6 ->
                    Colors.caputMortuum

                7 ->
                    Colors.asparagus

                8 ->
                    Colors.babyBlue

                _ ->
                    Colors.black
    in
    openedCellStyle size
        ++ [ Font.color color
           , Font.family [ Font.monospace ]
           , Font.extraBold
           , Font.glow color 0.2
           ]


styledGameSelectionButton : { onPress : Maybe msg, title : String, subtitle : String, isPhone : Bool } -> Element msg
styledGameSelectionButton { onPress, title, subtitle, isPhone } =
    Input.button
        [ Element.width
            (if isPhone then
                fill

             else
                px 240
            )
        , Element.padding 16
        , Background.color Colors.lightGrey
        , Border.rounded 16
        , Border.color Colors.cellBorderColor
        , Border.width 1
        , Element.centerX
        ]
        { onPress = onPress
        , label =
            column [ Element.width fill, Element.spacing 8 ]
                [ el [ Font.bold, Font.size 24 ] <| text title
                , el [ Font.color Colors.caputMortuum ] <| text subtitle
                ]
        }


{-| Credits to <https://ellie-app.com/85HbWTjCGWha1>
-}
toggleCheckboxWidget : { offColor : Color, onColor : Color, sliderColor : Color, toggleWidth : Int, toggleHeight : Int, offSymbol : Maybe Char, onSymbol : Maybe Char, tooltip : Maybe String } -> Bool -> Element msg
toggleCheckboxWidget { offColor, onColor, sliderColor, toggleWidth, toggleHeight, offSymbol, onSymbol, tooltip } checked =
    let
        pad =
            3

        sliderSize =
            toggleHeight - 2 * pad

        translation =
            (toggleWidth - sliderSize - pad)
                |> String.fromInt
    in
    Element.el
        ([ Background.color <|
            if checked then
                onColor

            else
                offColor
         , width <| px <| toggleWidth
         , height <| px <| toggleHeight
         , Border.rounded <| toggleHeight // 2
         , inFront <|
            el [ height fill ] <|
                el
                    [ Background.color sliderColor
                    , Border.rounded <| sliderSize // 2
                    , width <| px <| sliderSize
                    , height <| px <| sliderSize
                    , centerY
                    , moveRight pad
                    , htmlAttribute <|
                        HA.style "transition" ".3s"
                    , htmlAttribute <|
                        if checked then
                            HA.style "transform" <| "translateX(" ++ translation ++ "px)"

                        else
                            HA.class ""
                    ]
                <|
                    el [ centerX, centerY, Font.size <| toggleHeight // 2, Font.color <| rgb255 150 150 150 ] <|
                        text <|
                            if checked then
                                Maybe.withDefault "" <| Maybe.map String.fromChar offSymbol

                            else
                                Maybe.withDefault "" <| Maybe.map String.fromChar onSymbol
         ]
            ++ (case tooltip of
                    Just text ->
                        [ htmlAttribute <| HA.title text ]

                    Nothing ->
                        []
               )
        )
    <|
        Element.none
