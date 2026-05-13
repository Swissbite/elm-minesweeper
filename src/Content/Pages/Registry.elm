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


module Content.Pages.Registry exposing (all)

import Content.Pages.About as Page1
import Markdown.Block
import Time


all :
    List
        { title : String
        , slug : String
        , order : Int
        , published : Bool
        , description : String
        , lang : String
        , updatedAt : Time.Posix
        , body : List Markdown.Block.Block
        }
all =
    [ { title = Page1.content.title
      , slug = Page1.content.slug
      , order = Page1.content.order
      , published = Page1.content.published
      , description = Page1.content.description
      , lang = Page1.content.lang
      , updatedAt = Page1.content.updatedAt
      , body = Page1.content.body
      }
    ]
