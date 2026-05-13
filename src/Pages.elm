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


module Pages exposing (Page, findPublishedBySlug, navigationEntries, view)

import Content.Pages.Registry as Registry
import Element exposing (Element, fill)
import Element.Font as Font
import Markdown.Block
import Markdown.Renderer
import Markdown.Renderer.ElmUi
import Styles
import Time


type alias Page =
    { title : String
    , slug : String
    , order : Int
    , published : Bool
    , description : String
    , lang : String
    , updatedAt : Time.Posix
    , body : List Markdown.Block.Block
    }


findPublishedBySlug : String -> Maybe Page
findPublishedBySlug slug =
    publishedPages
        |> List.filter (\page -> page.slug == slug)
        |> List.head


navigationEntries : List { title : String, slug : String }
navigationEntries =
    publishedPages
        |> List.map (\page -> { title = page.title, slug = page.slug })


view : Page -> Element msg
view page =
    Element.column
        [ Element.width fill
        , Element.padding 24
        , Element.spacing 24
        ]
        [ Element.column
            [ Element.width fill
            , Element.spacing 12
            ]
            ([ Element.el [ Font.size 32, Font.semiBold ] (Element.text page.title)
             , Element.paragraph [ Font.size 18 ] [ Element.text page.description ]
             , metaView page
             ]
                ++ bodyView page.body
            )
        ]


publishedPages : List Page
publishedPages =
    Registry.all
        |> List.filter .published
        |> List.sortBy .order


metaView : Page -> Element msg
metaView page =
    Element.wrappedRow
        [ Element.width fill
        , Element.spacing 16
        , Font.size 14
        ]
        [ Element.el [] (Element.text (Styles.icons.calendar ++ " Updated " ++ formatDate page.updatedAt))
        , Element.el [] (Element.text (Styles.icons.world ++ " " ++ String.toUpper page.lang))
        ]


bodyView : List Markdown.Block.Block -> List (Element msg)
bodyView blocks =
    case Markdown.Renderer.render Markdown.Renderer.ElmUi.renderer blocks of
        Ok rendered ->
            [ Element.column [ Element.width fill, Element.spacing 24 ] rendered ]

        Err _ ->
            [ Element.paragraph [] [ Element.text "This page could not be rendered right now." ] ]


formatDate : Time.Posix -> String
formatDate posix =
    String.fromInt (Time.toYear Time.utc posix)
        ++ "-"
        ++ monthToString (Time.toMonth Time.utc posix)
        ++ "-"
        ++ padDay (Time.toDay Time.utc posix)


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "01"

        Time.Feb ->
            "02"

        Time.Mar ->
            "03"

        Time.Apr ->
            "04"

        Time.May ->
            "05"

        Time.Jun ->
            "06"

        Time.Jul ->
            "07"

        Time.Aug ->
            "08"

        Time.Sep ->
            "09"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"


padDay : Int -> String
padDay day =
    if day < 10 then
        "0" ++ String.fromInt day

    else
        String.fromInt day
