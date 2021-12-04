module Styles exposing (..)

import Element as Element
import Element.Background as Background
import Element.Border


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
    basicCellStyle ++
    [ Background.color untouchedCellGray
    ]
openedCellStyle : List (Element.Attribute msg)
openedCellStyle =
    basicCellStyle ++
    [ Background.color openedCellGray
    ]