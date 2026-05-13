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


module Content exposing (decoder)

import Char
import Content.Decode as Decode
import Content.Decode.Markdown as MarkdownDecode
import Content.Decode.Syntax as Syntax
import Content.Type as Type
import Json.Decode as Json


decoder : Type.Path -> Decode.QueryResult
decoder typePath =
    case typePath of
        Type.Single [ "Content", "Pages", _ ] ->
            Decode.frontmatter MarkdownDecode.decode
                [ Decode.attribute "title" Decode.string
                , Decode.attribute "slug" slugDecoder
                , Decode.attribute "order" Decode.int
                , Decode.attribute "published" boolDecoder
                , Decode.attribute "description" Decode.string
                , Decode.attribute "lang" langDecoder
                , Decode.attribute "updatedAt" Decode.datetime
                ]

        _ ->
            Decode.throw


boolDecoder : Decode.Decoder Bool
boolDecoder =
    Decode.fromSyntax Syntax.bool
        (always [])
        (\_ -> Json.bool)


slugDecoder : Decode.Decoder String
slugDecoder =
    validatedStringDecoder isValidSlug "Slug must be lowercase kebab-case."


langDecoder : Decode.Decoder String
langDecoder =
    validatedStringDecoder isValidLanguage "Language must contain lowercase letters and dashes only."


validatedStringDecoder : (String -> Bool) -> String -> Decode.Decoder String
validatedStringDecoder isValid errorMessage =
    Decode.fromSyntax Syntax.string
        (always [])
        (\_ ->
            Json.string
                |> Json.andThen
                    (\value ->
                        if isValid value then
                            Json.succeed value

                        else
                            Json.fail errorMessage
                    )
        )


isValidSlug : String -> Bool
isValidSlug slug =
    (not <| String.isEmpty slug)
        && (not <| String.startsWith "-" slug)
        && (not <| String.endsWith "-" slug)
        && (not <| String.contains "--" slug)
        && String.all
            (\char ->
                Char.isLower char || Char.isDigit char || char == '-'
            )
            slug


isValidLanguage : String -> Bool
isValidLanguage lang =
    (not <| String.isEmpty lang)
        && String.all
            (\char ->
                Char.isLower char || char == '-'
            )
            lang
