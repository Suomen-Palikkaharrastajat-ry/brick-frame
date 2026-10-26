module Link exposing
    ( freeHash
    , freeShareHash
    , puzzleHash
    )

{-| The URL hash of a Brick Play build, in its two forms.

  - Free building: `free=1&model=…`, with every part on the board and the glued
    ones marked (`0 !PALIKKA GLUED`). `free=1` alone is an empty board.
  - Puzzle: `model=…&parts=…`. The `model=` holds the board (glued parts
    marked), and `parts=` the kit: the dock's counts, always written, even when
    empty, so an empty kit is not mistaken for no kit.

A puzzle's kit counts every piece of its file and colour the dock may offer,
including the unglued pieces the model already places (see `Main.remainingForItem`),
so reopening the link shows the same remaining counts.

A free share (`freeShareHash`) keeps only the glued parts in the model and turns
every other part into a kit count, so the link opens as a puzzle with the glued
parts locked and the rest in the dock.

-}

import LDraw.Document as Document exposing (Document)
import LDraw.Writer as Writer
import Studio.UrlPayload as UrlPayload exposing (PartKitItem)
import Url


{-| The hash of free building: the whole board, glue included.
-}
freeHash : Document -> String
freeHash doc =
    if List.isEmpty (Document.placements doc) then
        "free=1"

    else
        "free=1&model=" ++ Url.percentEncode (Writer.writeDocument doc)


{-| The hash of a puzzle: the board (glued and placed parts alike) and the kit.
-}
puzzleHash : Document -> List PartKitItem -> String
puzzleHash doc kit =
    let
        modelPart =
            if List.isEmpty (Document.placements doc) then
                ""

            else
                "model=" ++ Url.percentEncode (Writer.writeDocument doc) ++ "&"
    in
    modelPart ++ "parts=" ++ Url.percentEncode (UrlPayload.encodeParts kit)


{-| The puzzle a free build shares: its glued parts stay on the board, and every
other part becomes a kit count, grouped by file and colour.
-}
freeShareHash : Document -> String
freeShareHash doc =
    let
        unglued =
            Document.placements doc
                |> List.filter (\placement -> not (Document.isGlued placement.id doc))

        gluedOnly =
            List.foldl (\placement acc -> Document.removePlacement placement.id acc) doc unglued
    in
    puzzleHash gluedOnly (UrlPayload.kitOfPlacements unglued)
