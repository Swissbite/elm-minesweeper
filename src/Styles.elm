module Styles exposing (..)

import Element as Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes as HA


icons : { markerFlag : Char, untouchedBomb : Char, exploded : Char, stopWatch : Char }
icons =
    { markerFlag = '⚑'
    , untouchedBomb = '💣'
    , exploded = '💥'
    , stopWatch = '⏱'
    }


saffron : Element.Color
saffron =
    Element.rgba255 227 181 5 1


fieryRose : Element.Color
fieryRose =
    Element.rgba255 245 100 118 1


cerise : Element.Color
cerise =
    Element.rgba255 228 63 111 1


smitten : Element.Color
smitten =
    Element.rgba255 190 62 130 1


eggplant : Element.Color
eggplant =
    Element.rgba255 94 67 82 1


caputMortuum : Element.Color
caputMortuum =
    Element.rgba255 80 36 25 1


asparagus : Element.Color
asparagus =
    Element.rgba255 126 161 114 1


babyBlue : Element.Color
babyBlue =
    Element.rgba255 108 212 255 1


untouchedCellGray : Element.Color
untouchedCellGray =
    Element.rgba255 190 190 190 0.8


openedCellGray : Element.Color
openedCellGray =
    Element.rgba255 190 190 190 0.2


cellBorderColor : Element.Color
cellBorderColor =
    Element.rgba 100 100 100 1


lightGrey : Color
lightGrey =
    rgb255 187 187 187


green : Color
green =
    rgb255 39 203 139


white : Color
white =
    rgb255 255 255 255


cellWidth : Element.Length
cellWidth =
    Element.px 50


basicCellStyle : List (Element.Attribute msg)
basicCellStyle =
    [ Element.width cellWidth
    , Element.height cellWidth
    , Border.color cellBorderColor
    , Border.width 1
    , Element.pointer
    ]


untouchedCellStyle : List (Element.Attribute msg)
untouchedCellStyle =
    basicCellStyle
        ++ [ Background.color untouchedCellGray
           ]


openedCellStyle : List (Element.Attribute msg)
openedCellStyle =
    basicCellStyle
        ++ [ Background.color openedCellGray
           ]


openedMineNeighbourCellStyle : Int -> List (Element.Attribute msg)
openedMineNeighbourCellStyle number =
    let
        color =
            case number of
                1 ->
                    saffron

                2 ->
                    fieryRose

                3 ->
                    cerise

                4 ->
                    smitten

                5 ->
                    eggplant

                6 ->
                    caputMortuum

                7 ->
                    asparagus

                8 ->
                    babyBlue

                _ ->
                    Element.rgba 0 0 0 1
    in
    openedCellStyle
        ++ [ Font.color color
           , Font.family [ Font.monospace ]
           , Font.extraBold
           , Font.glow color 0.2
           ]


{-| Credits to <https://ellie-app.com/85HbWTjCGWha1>
-}
toggleCheckboxWidget : { offColor : Color, onColor : Color, sliderColor : Color, toggleWidth : Int, toggleHeight : Int, offSymbol : Maybe Char, onSymbol : Maybe Char } -> Bool -> Element msg
toggleCheckboxWidget { offColor, onColor, sliderColor, toggleWidth, toggleHeight, offSymbol, onSymbol } checked =
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
        [ Background.color <|
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
    <|
        Element.none
