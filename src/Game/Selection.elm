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


module Game.Selection exposing (view)

import Colors
import Element exposing (Element)
import Element.Font as Font
import Styles
import Types exposing (..)


smallPlayground : PlayGroundDefinition
smallPlayground =
    { cols = 8
    , rows = 8
    , mines = 10
    }


mediumPlayground : PlayGroundDefinition
mediumPlayground =
    { cols = 16
    , rows = 16
    , mines = 40
    }


advancePlayground : PlayGroundDefinition
advancePlayground =
    { cols = 30
    , rows = 16
    , mines = 99
    }


xxlPlayground : PlayGroundDefinition
xxlPlayground =
    { cols = 30
    , rows = 30
    , mines = 200
    }


view : Model -> Element GameMsg
view model =
    let
        isPhone =
            model.device.class == Element.Phone

        optionView : ( String, PlayGroundDefinition ) -> Element GameMsg
        optionView ( title, definition ) =
            Styles.styledGameSelectionButton model.theme
                { onPress = Just (CreateNewGame definition)
                , title = title
                , subtitle =
                    String.fromInt definition.cols
                        ++ " x "
                        ++ String.fromInt definition.rows
                        ++ " • "
                        ++ String.fromInt definition.mines
                        ++ " mines"
                , isPhone = isPhone
                }

        options =
            [ ( "Small", smallPlayground )
            , ( "Medium", mediumPlayground )
            , ( "Advanced", advancePlayground )
            , ( "XXL", xxlPlayground )
            ]
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.padding 16
        , Element.spacing 32
        ]
        [ Element.column [ Element.width Element.fill, Element.spacing 12, Font.center ]
            [ Element.el [ Font.bold, Font.size 32, Element.centerX ] <| Element.text "Choose a board"
            , Element.paragraph [ Font.color (Colors.textDim model.theme), Element.centerX, Element.width (Element.maximum 500 Element.fill) ]
                [ Element.text "Mobile keeps touch-friendly cells and lets larger boards scroll when needed." ]
            ]
        , Element.wrappedRow
            [ Element.centerX
            , Element.spacing 16
            , Element.width (Element.maximum 576 Element.fill)
            ]
            (List.map optionView options)
        ]
