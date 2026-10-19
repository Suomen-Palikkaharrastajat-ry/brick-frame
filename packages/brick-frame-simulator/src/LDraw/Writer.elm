module LDraw.Writer exposing (writeDocument, writeLines)

{-| The reverse of `LDraw.Parser`: serialise documents and parsed lines back to
`.ldr` / `.mpd` text.

Lines kept verbatim in a `Document` come out exactly as read; placements are
re-formatted with stable six-decimal floats.

-}

import LDraw.Document as Document exposing (Document, Entry(..))
import LDraw.Format as Format
import LDraw.Types exposing (LDrawLine)


{-| Write a document. A single unnamed submodel is a plain `.ldr`; otherwise
every submodel gets a `0 FILE` header and the result is an `.mpd`.
-}
writeDocument : Document -> String
writeDocument doc =
    let
        entryText entry =
            case entry of
                PlacementEntry p ->
                    Format.placementToText p

                Raw text ->
                    text

        submodelLines submodel =
            if Document.isMultiFile doc then
                ("0 FILE " ++ submodel.name) :: List.map entryText submodel.entries

            else
                List.map entryText submodel.entries
    in
    doc.submodels
        |> List.concatMap submodelLines
        |> String.join "\n"


{-| Write parsed lines as a plain `.ldr` body.
-}
writeLines : List LDrawLine -> String
writeLines lines =
    lines
        |> List.map Format.lineToText
        |> String.join "\n"
