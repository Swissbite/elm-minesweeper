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


module Content.Pages.About exposing (Content, content)

import Markdown.Block exposing (Block(..), HeadingLevel(..), Inline(..), ListItem(..), ListSpacing(..), Task(..))
import Time


type alias Content =
    { title : String
    , slug : String
    , order : Int
    , published : Bool
    , description : String
    , lang : String
    , updatedAt : Time.Posix
    , body : List Block
    }


content : Content
content =
    { title = "Über das Projekt"
    , slug = "about"
    , order = 1
    , published = True
    , description = "Warum dieses Projekt existiert und wie Build-Time-Content im Elm-Projekt landet."
    , lang = "de"
    , updatedAt = Time.millisToPosix 1778630400000
    , body =
        [ Paragraph [ Text "Elm Minesweeper ist ein Spielprojekt, das neben dem eigentlichen Game auch Platz für statische Inhalte bekommen soll." ]
        , Heading H2 [ Text "Was diese Seite zeigt" ]
        , UnorderedList Tight
            [ ListItem NoTask [ Paragraph [ Text "Frontmatter mit validierten Metadaten" ] ]
            , ListItem NoTask [ Paragraph [ Text "Build-Time generierte Elm-Module" ] ]
            , ListItem NoTask [ Paragraph [ Text "Routing innerhalb derselben Elm-SPA" ] ]
            , ListItem NoTask [ Paragraph [ Text "Graceful Fallback, falls Rendering einmal fehlschlägt" ] ]
            ]
        , Heading H2 [ Text "Warum Build-Time?" ]
        , Paragraph [ Text "Der Browser muss keine Markdown-Dateien entdecken oder parsen. Stattdessen werden die Inhalte vor dem Build in Elm-Code übersetzt." ]
        ]
    }
