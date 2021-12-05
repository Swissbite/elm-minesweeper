module Styles exposing (..)

import Element as Element
import Element.Background as Background
import Element.Border
import Element.Font as Font


icons : { markerFlag : String, untouchedBomb : String, exploded : String, stopWatch : String }
icons =
    { markerFlag = "⚑"
    , untouchedBomb = "💣"
    , exploded = "💥"
    , stopWatch = "⏱️"
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


cellWidth : Element.Length
cellWidth =
    Element.px 50


basicCellStyle : List (Element.Attribute msg)
basicCellStyle =
    [ Element.width cellWidth
    , Element.height cellWidth
    , Element.Border.color cellBorderColor
    , Element.Border.width 1
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
           ]
