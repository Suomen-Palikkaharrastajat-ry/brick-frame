module Studio.PartSearch exposing
    ( classicPosition
    , matching
    , normaliseQuery
    , tierOrder
    )

{-| Searching the part catalogue (`Studio.PartIndex`) by text, shared by the
studio's part palette and Brick Play's dock.

A query is normalised (lower case, sizes spelled "2 x 4"), split into words with
each size kept whole, and every word must occur in a part's searchable text.
Matches are ranked: exact part number, part-number prefix, description prefix,
description match, then keyword-only matches. Within each rank, curated classics
come first, then everyday building parts before Technic and the rest, and
footprint matches ahead of 3D extended sizes.

The caller narrows the candidates first (a build group, a colour, a shelf); this
module only matches and ranks what it is given.

-}

import Studio.BuildGroups as BuildGroups exposing (Tier(..))
import Studio.PartIndex exposing (Part)


{-| The parts that match a non-blank query, best first, at most `limit` of them.
A blank query matches nothing.
-}
matching : Int -> String -> List Part -> List Part
matching limit query parts =
    let
        whole =
            normaliseQuery query

        words =
            searchTerms whole

        matches part =
            List.all (\word -> termIn word part.haystack) words

        rank part =
            if part.file == whole || part.file == whole ++ ".dat" then
                0

            else if String.startsWith whole part.file then
                1

            else if String.startsWith whole part.descriptionLower then
                2

            else if List.all (\word -> termIn word part.descriptionLower) words then
                3

            else
                -- Matched only through keywords.
                4

        -- The key is computed once per part, then sorted on: `sortBy` would
        -- recompute it on every comparison.
        key part =
            ( rank part
            , classicFirst part
            , ( footprintOrder words part, tierOrder part.shelving.tier, ( part.shelving.classicOrder, ( String.length part.description, part.file ) ) )
            )
    in
    if whole == "" then
        []

    else
        parts
            |> List.filter matches
            |> List.map (\part -> ( key part, part ))
            |> List.sortBy Tuple.first
            |> List.map Tuple.second
            |> List.take limit


{-| Lower-case and trim a search, and spell sizes the way LDraw does: "2x4",
"2 x4" and "2x 4" all become "2 x 4".
-}
normaliseQuery : String -> String
normaliseQuery query =
    query
        |> String.toLower
        |> String.words
        |> List.map spellSize
        |> String.join " "


{-| The words of a normalised search, with each size kept whole: "brick 1 x 2"
looks for "brick" and "1 x 2", not for "1", "x" and "2" anywhere.
-}
searchTerms : String -> List String
searchTerms whole =
    let
        isNumber w =
            String.toFloat w /= Nothing

        go ws =
            case ws of
                a :: "x" :: b :: rest ->
                    if isNumber a && isNumber b then
                        sizeRun (a ++ " x " ++ b) rest

                    else
                        a :: go ("x" :: b :: rest)

                w :: rest ->
                    w :: go rest

                [] ->
                    []

        sizeRun acc ws =
            case ws of
                "x" :: c :: rest ->
                    if isNumber c then
                        sizeRun (acc ++ " x " ++ c) rest

                    else
                        acc :: go ws

                _ ->
                    acc :: go ws
    in
    go (String.words whole)


spellSize : String -> String
spellSize word =
    let
        pieces =
            String.split "x" word

        isNumber piece =
            String.toFloat piece /= Nothing
    in
    -- "2x4" and also the halves of "2 x4" / "2x 4": "x4" and "2x".
    if List.length pieces >= 2 && List.any isNumber pieces && List.all (\piece -> piece == "" || isNumber piece) pieces then
        String.join " x " pieces

    else
        word


{-| Footprint matches rank ahead of 3D extended sizes when a 2D footprint is queried.
-}
footprintOrder : List String -> Part -> Int
footprintOrder words part =
    let
        twoPartSizes =
            List.filter isTwoPartSize words
    in
    if List.isEmpty twoPartSizes then
        0

    else if List.all (\size -> hasExactFootprint size part.descriptionLower) twoPartSizes then
        0

    else
        1


{-| Whether a word represents a 2D footprint size like "1 x 2" or "2 x 4".
-}
isTwoPartSize : String -> Bool
isTwoPartSize word =
    case String.split " x " word of
        [ a, b ] ->
            String.toFloat a /= Nothing && String.toFloat b /= Nothing

        _ ->
            False


{-| Whether an occurrence of a 2D size in a description is an exact footprint match,
rather than being preceded or followed by another dimension (e.g. "1 x 2 x 3").
-}
hasExactFootprint : String -> String -> Bool
hasExactFootprint size text =
    String.indexes size text
        |> List.any
            (\i ->
                let
                    before =
                        String.slice 0 i text

                    after =
                        String.dropLeft (i + String.length size) text

                    beforeValid =
                        not (isDigitAt (i - 1) text) && not (endsInDimension before)

                    afterValid =
                        not (isDigitAt 0 after) && not (startsInDimension after)
                in
                beforeValid && afterValid
            )


{-| Whether a string ends with a dimension marker like " 2 x ".
-}
endsInDimension : String -> Bool
endsInDimension s =
    if String.endsWith " x " s then
        let
            before =
                String.dropRight 3 s
        in
        case String.uncons (String.right 1 before) of
            Just ( c, _ ) ->
                Char.isDigit c || c == '.'

            Nothing ->
                False

    else
        False


{-| Whether a string starts with an extended dimension like " x 3" or " x 0.667".
-}
startsInDimension : String -> Bool
startsInDimension s =
    if String.startsWith " x " s then
        let
            remainder =
                String.dropLeft 3 s
        in
        case String.uncons remainder of
            Just ( c, _ ) ->
                Char.isDigit c || c == '.'

            Nothing ->
                False

    else
        False


{-| A part's place among a group's classics, after every classic when it is
not one.
-}
classicPosition : String -> Part -> Int
classicPosition group part =
    part.shelving.classics
        |> List.filter (\( id, _ ) -> id == group)
        |> List.map Tuple.second
        |> List.minimum
        |> Maybe.withDefault BuildGroups.notClassic


{-| Curated classics lead a search whatever their tier, so "axle" or "gear"
starts with Technic Axle 4 and Gear 8 Tooth rather than every plate with an
axle hole.
-}
classicFirst : Part -> Int
classicFirst part =
    if List.isEmpty part.shelving.classics then
        1

    else
        0


{-| Whether a search term occurs in a text. A size ("2 x 4") must not run into
neighbouring digits, so it does not match "12 x 4" or "2 x 48".
-}
termIn : String -> String -> Bool
termIn term text =
    if String.contains " x " term then
        String.indexes term text
            |> List.any
                (\i ->
                    not (isDigitAt (i - 1) text || isDigitAt (i + String.length term) text)
                )

    else
        String.contains term text


isDigitAt : Int -> String -> Bool
isDigitAt i text =
    if i < 0 then
        False

    else
        case String.uncons (String.slice i (i + 1) text) of
            Just ( c, _ ) ->
                Char.isDigit c || c == '.'

            Nothing ->
                False


{-| Sort order of build tiers: basic parts first, clutter last.
-}
tierOrder : Tier -> Int
tierOrder tier =
    case tier of
        Basic ->
            0

        Other ->
            1

        Clutter ->
            2
