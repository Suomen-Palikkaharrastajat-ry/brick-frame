module Studio.PartIndex exposing
    ( Part
    , PartIndex
    , categories
    , decoder
    , fetch
    , search
    )

{-| The catalogue the part palette searches.

`scripts/build-part-index.mjs` writes it next to the synced LDraw library as
`parts-index.json`, one `[file, description, category, keywords?]` entry per part.
It is fetched once, when the palette is first opened, and searched in memory.

-}

import Dict
import Http
import Json.Decode as Decode exposing (Decoder)


{-| One part of the library. `haystack` is everything searchable about it, in
lower case, so a search is a substring test.
-}
type alias Part =
    { file : String
    , description : String
    , category : String
    , haystack : String
    }


{-| Every part, in file order, and the categories they fall into.
-}
type alias PartIndex =
    { parts : List Part
    , categories : List ( String, Int )
    }


{-| Read `{ "version": 1, "parts": [[file, description, category, keywords?], …] }`.
-}
decoder : Decoder PartIndex
decoder =
    Decode.field "version" Decode.int
        |> Decode.andThen
            (\version ->
                if version == 1 then
                    Decode.field "parts" (Decode.list partDecoder)
                        |> Decode.map (\parts -> { parts = parts, categories = categories parts })

                else
                    Decode.fail ("Unsupported part index version " ++ String.fromInt version)
            )


partDecoder : Decoder Part
partDecoder =
    Decode.map4
        (\file description category keywords ->
            { file = file
            , description = description
            , category = category
            , haystack = String.toLower (String.join " " [ file, description, keywords ])
            }
        )
        (Decode.index 0 Decode.string)
        (Decode.index 1 Decode.string)
        (Decode.index 2 Decode.string)
        (Decode.oneOf [ Decode.index 3 Decode.string, Decode.succeed "" ])


{-| Fetch the index that sits in the LDraw library directory `ldrawBase`.
-}
fetch : String -> (Result Http.Error PartIndex -> msg) -> Cmd msg
fetch ldrawBase toMsg =
    Http.get
        { url = withoutTrailingSlash ldrawBase ++ "/parts-index.json"
        , expect = Http.expectJson toMsg decoder
        }


withoutTrailingSlash : String -> String
withoutTrailingSlash base =
    if String.endsWith "/" base then
        String.dropRight 1 base

    else
        base


{-| Categories with their part counts, most parts first.
-}
categories : List Part -> List ( String, Int )
categories parts =
    parts
        |> List.foldl
            (\part counts -> Dict.update part.category (\n -> Just (Maybe.withDefault 0 n + 1)) counts)
            Dict.empty
        |> Dict.toList
        |> List.sortBy (\( name, count ) -> ( -count, name ))


{-| Parts matching every word of `query` (in the file name, description or
keywords) and, if given, a category — best matches first, at most `limit`.

A part number typed exactly comes first, then parts whose number starts with
the query, then those whose description does, then the rest by name.

-}
search : Int -> String -> Maybe String -> PartIndex -> List Part
search limit query category index =
    let
        words =
            query |> String.toLower |> String.words

        wanted part =
            (category == Nothing || category == Just part.category)
                && List.all (\word -> String.contains word part.haystack) words

        whole =
            String.toLower (String.trim query)

        rank part =
            if part.file == whole || part.file == whole ++ ".dat" then
                0

            else if String.startsWith whole part.file then
                1

            else if String.startsWith whole (String.toLower part.description) then
                2

            else
                3
    in
    index.parts
        |> List.filter wanted
        |> List.sortBy (\part -> ( rank part, String.length part.description, part.file ))
        |> List.take limit
