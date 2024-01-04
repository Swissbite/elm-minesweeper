{-
   This file is part of Elm Minesweeper.

   Elm Minesweeper is free software: you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later version.

   Elm Minesweeper is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
   PARTICULAR PURPOSE. See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along with
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


icons : { markerFlag : Char, untouchedBomb : Char, exploded : Char, stopWatch : String, victory : Char, downSign : Char, upSign : Char, world : Char, calendar : String }
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
    }


cellWidth : Element.Length
cellWidth =
    Element.px 50


basicCellStyle : List (Element.Attribute msg)
basicCellStyle =
    [ Element.width cellWidth
    , Element.height cellWidth
    , Border.color Colors.cellBorderColor
    , Border.width 1
    , Element.pointer
    ]


untouchedCellStyle : List (Element.Attribute msg)
untouchedCellStyle =
    basicCellStyle
        ++ [ Background.color Colors.untouchedCellGray
           ]


openedCellStyle : List (Element.Attribute msg)
openedCellStyle =
    basicCellStyle
        ++ [ Background.color Colors.openedCellGray
           ]


openedMineNeighbourCellStyle : Int -> List (Element.Attribute msg)
openedMineNeighbourCellStyle number =
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
    openedCellStyle
        ++ [ Font.color color
           , Font.family [ Font.monospace ]
           , Font.extraBold
           , Font.glow color 0.2
           ]


styledGameSelectionButton : { onPress : Maybe msg, label : Element msg } -> Element msg
styledGameSelectionButton =
    Input.button
        [ Element.width
            (fill
                |> Element.maximum 400
                |> Element.minimum 300
            )
        , Element.height
            (fill
                |> Element.maximum 400
                |> Element.minimum 300
            )
        , Background.color Colors.lightGrey
        , Element.centerX
        , Element.centerY
        ]


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
