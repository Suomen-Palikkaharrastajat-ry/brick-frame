module LDraw.Steps exposing
    ( CalloutItem
    , Step
    , StepBounds
    , autoPartition
    , autoStepDocument
    , calloutForPlacements
    , colorName
    , documentAtStep
    , exportHtml
    , fromDocument
    , fromEntries
    , fromSubmodel
    , hasStepMarkers
    , isStepMarker
    , stepAtIndex
    , stepBounds
    , stepCount
    , stepDeltaCallout
    )

{-| Step breakdown and building instructions for LDraw documents.

Placements in an LDraw document can be partitioned into sequential steps
by `0 STEP` metadata markers. This module provides step parsing,
delta part callout calculation, step indexing, automated camera framing bounds,
and printable HTML export.

-}

import Dict exposing (Dict)
import LDraw.Colors as Colors
import LDraw.Document as Document exposing (Document, Entry(..), Placement, Submodel)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3)


{-| A single building instruction step.

  - `index`: 1-based step number (1, 2, ...)
  - `placements`: parts introduced in this specific step
  - `accumulatedPlacements`: all parts up to and including this step
  - `callout`: parts callout showing quantities and colours for this step's new parts

-}
type alias Step =
    { index : Int
    , placements : List Placement
    , accumulatedPlacements : List Placement
    , callout : List CalloutItem
    }


{-| An item in the parts callout box for a step.
Represents a unique part and colour combination introduced in that step, with quantity.
-}
type alias CalloutItem =
    { part : String
    , color : Int
    , count : Int
    }


{-| Bounding center and radius for a step's introduced parts.
-}
type alias StepBounds =
    { center : Vec3
    , radius : Float
    }



-- ── Step Parsing & Grouping ───────────────────────────────────────────────────


{-| True when a raw comment line is an LDraw `0 STEP` or `0 !STEP` directive.
-}
isStepMarker : String -> Bool
isStepMarker raw =
    case String.words (String.trim raw) of
        "0" :: word :: _ ->
            let
                upper =
                    String.toUpper word
            in
            upper == "STEP" || upper == "!STEP"

        _ ->
            False


{-| True when the document's main submodel contains at least one `0 STEP` directive.
-}
hasStepMarkers : Document -> Bool
hasStepMarkers doc =
    (Document.mainSubmodel doc).entries
        |> List.any
            (\entry ->
                case entry of
                    Raw s ->
                        isStepMarker s

                    PlacementEntry _ ->
                        False
            )


{-| Break a document's main submodel into steps based on `0 STEP` lines.

If the document contains no `0 STEP` lines, all placements are placed into
a single step 1.

-}
fromDocument : Document -> List Step
fromDocument doc =
    fromEntries (Document.mainSubmodel doc).entries


{-| Break a submodel's entries into steps based on `0 STEP` lines.
-}
fromSubmodel : Submodel -> List Step
fromSubmodel submodel =
    fromEntries submodel.entries


{-| Group an arbitrary list of entries into steps by `0 STEP` directives.
Leading, trailing, and consecutive `0 STEP` lines do not create empty steps.
-}
fromEntries : List Entry -> List Step
fromEntries entries =
    let
        step entry state =
            case entry of
                Raw line ->
                    if isStepMarker line then
                        if List.isEmpty state.current then
                            state

                        else
                            let
                                newStep =
                                    { index = state.index
                                    , placements = List.reverse state.current
                                    , accumulatedPlacements = List.reverse state.accumulated
                                    , callout = calloutForPlacements (List.reverse state.current)
                                    }
                            in
                            { current = []
                            , accumulated = state.accumulated
                            , steps = newStep :: state.steps
                            , index = state.index + 1
                            }

                    else
                        state

                PlacementEntry p ->
                    { current = p :: state.current
                    , accumulated = p :: state.accumulated
                    , steps = state.steps
                    , index = state.index
                    }

        finalState =
            List.foldl step { current = [], accumulated = [], steps = [], index = 1 } entries
    in
    if List.isEmpty finalState.current then
        List.reverse finalState.steps

    else
        let
            lastStep =
                { index = finalState.index
                , placements = List.reverse finalState.current
                , accumulatedPlacements = List.reverse finalState.accumulated
                , callout = calloutForPlacements (List.reverse finalState.current)
                }
        in
        List.reverse (lastStep :: finalState.steps)


{-| Automatically partition a list of placements into steps of `partsPerStep` parts.
-}
autoPartition : Int -> List Placement -> List Step
autoPartition partsPerStep placements =
    let
        stepSize =
            max 1 partsPerStep

        chunks list =
            if List.isEmpty list then
                []

            else
                List.take stepSize list :: chunks (List.drop stepSize list)

        stepChunks =
            chunks placements

        buildStep ( index, chunk ) ( stepsAcc, accPlacements ) =
            let
                allSoFar =
                    accPlacements ++ chunk

                newStep =
                    { index = index
                    , placements = chunk
                    , accumulatedPlacements = allSoFar
                    , callout = calloutForPlacements chunk
                    }
            in
            ( newStep :: stepsAcc, allSoFar )

        indexedChunks =
            List.indexedMap (\i c -> ( i + 1, c )) stepChunks

        ( result, _ ) =
            List.foldl buildStep ( [], [] ) indexedChunks
    in
    List.reverse result


{-| Insert `0 STEP` directives after every `partsPerStep` placements in the
document's main submodel.
-}
autoStepDocument : Int -> Document -> Document
autoStepDocument partsPerStep doc =
    let
        stepSize =
            max 1 partsPerStep

        main =
            Document.mainSubmodel doc

        step entry ( count, acc ) =
            case entry of
                PlacementEntry p ->
                    let
                        nextCount =
                            count + 1
                    in
                    if nextCount >= stepSize then
                        ( 0, Raw "0 STEP" :: PlacementEntry p :: acc )

                    else
                        ( nextCount, PlacementEntry p :: acc )

                Raw s ->
                    if isStepMarker s then
                        -- Drop existing step markers when auto-stepping
                        ( count, acc )

                    else
                        ( count, Raw s :: acc )

        ( _, reversedEntries ) =
            List.foldl step ( 0, [] ) main.entries

        newMain =
            { main | entries = List.reverse reversedEntries }

        newSubmodels =
            case doc.submodels of
                _ :: rest ->
                    newMain :: rest

                [] ->
                    [ newMain ]
    in
    { doc | submodels = newSubmodels }



-- ── Callout Calculation ───────────────────────────────────────────────────────


{-| Compute the parts callout for a list of placements.
Groups identical part files and colours, summing their counts.
-}
calloutForPlacements : List Placement -> List CalloutItem
calloutForPlacements placements =
    let
        add p dict =
            let
                key =
                    ( p.part, p.color )

                count =
                    Dict.get key dict |> Maybe.withDefault 0
            in
            Dict.insert key (count + 1) dict

        counts =
            List.foldl add Dict.empty placements
    in
    Dict.toList counts
        |> List.map (\( ( part, color ), count ) -> { part = part, color = color, count = count })
        |> List.sortWith
            (\a b ->
                case compare a.part b.part of
                    EQ ->
                        compare a.color b.color

                    ord ->
                        ord
            )


{-| Return the callout items representing the delta parts introduced in this step.
-}
stepDeltaCallout : Step -> List CalloutItem
stepDeltaCallout step =
    step.callout



-- ── Step Queries & Sub-Documents ──────────────────────────────────────────────


{-| Total number of steps.
-}
stepCount : List Step -> Int
stepCount =
    List.length


{-| Look up a step by its 1-based index.
-}
stepAtIndex : Int -> List Step -> Maybe Step
stepAtIndex index steps =
    steps
        |> List.filter (\s -> s.index == index)
        |> List.head


{-| Produce a Document containing only the placements up to the given step index.
Non-main submodels are preserved intact so submodel references continue to resolve.
-}
documentAtStep : Document -> Int -> Document
documentAtStep doc targetStepIndex =
    let
        main =
            Document.mainSubmodel doc

        step entry ( currentStep, acc ) =
            case entry of
                Raw s ->
                    if isStepMarker s then
                        let
                            nextStep =
                                currentStep + 1
                        in
                        if currentStep < targetStepIndex then
                            ( nextStep, entry :: acc )

                        else
                            ( nextStep, acc )

                    else if currentStep <= targetStepIndex then
                        ( currentStep, entry :: acc )

                    else
                        ( currentStep, acc )

                PlacementEntry _ ->
                    if currentStep <= targetStepIndex then
                        ( currentStep, entry :: acc )

                    else
                        ( currentStep, acc )

        ( _, reversedEntries ) =
            List.foldl step ( 1, [] ) main.entries

        newMain =
            { main | entries = List.reverse reversedEntries }

        newSubmodels =
            case doc.submodels of
                _ :: rest ->
                    newMain :: rest

                [] ->
                    [ newMain ]
    in
    { doc | submodels = newSubmodels }



-- ── Camera Framing Calculation ────────────────────────────────────────────────


{-| Compute the bounding center and radius for a list of placements.
Uses the placement translation matrix and adds a 25 LDU boundary margin.
-}
stepBounds : List Placement -> Maybe StepBounds
stepBounds placements =
    case placements of
        [] ->
            Nothing

        p0 :: rest ->
            let
                pos p =
                    let
                        m =
                            Mat4.toRecord p.matrix
                    in
                    ( m.m14, m.m24, m.m34 )

                margin =
                    25.0

                update ( x, y, z ) acc =
                    { minX = min acc.minX (x - margin)
                    , maxX = max acc.maxX (x + margin)
                    , minY = min acc.minY (y - margin)
                    , maxY = max acc.maxY (y + margin)
                    , minZ = min acc.minZ (z - margin)
                    , maxZ = max acc.maxZ (z + margin)
                    }

                ( x0, y0, z0 ) =
                    pos p0

                initialAcc =
                    { minX = x0 - margin
                    , maxX = x0 + margin
                    , minY = y0 - margin
                    , maxY = y0 + margin
                    , minZ = z0 - margin
                    , maxZ = z0 + margin
                    }

                bounds =
                    List.foldl (pos >> update) initialAcc rest

                center =
                    Vec3.vec3
                        ((bounds.minX + bounds.maxX) / 2)
                        ((bounds.minY + bounds.maxY) / 2)
                        ((bounds.minZ + bounds.maxZ) / 2)

                dx =
                    bounds.maxX - bounds.minX

                dy =
                    bounds.maxY - bounds.minY

                dz =
                    bounds.maxZ - bounds.minZ

                radius =
                    max 30.0 (sqrt (dx * dx + dy * dy + dz * dz) / 2)
            in
            Just { center = center, radius = radius }



-- ── Export & Printable Format ─────────────────────────────────────────────────


{-| Human-readable name for common LDraw colour codes.
-}
colorName : Int -> String
colorName code =
    case code of
        0 ->
            "Black"

        1 ->
            "Blue"

        2 ->
            "Green"

        3 ->
            "Dark Turquoise"

        4 ->
            "Red"

        5 ->
            "Dark Pink"

        6 ->
            "Brown"

        7 ->
            "Light Gray"

        8 ->
            "Dark Gray"

        9 ->
            "Light Blue"

        10 ->
            "Bright Green"

        11 ->
            "Light Turquoise"

        12 ->
            "Light Red"

        13 ->
            "Pink"

        14 ->
            "Yellow"

        15 ->
            "White"

        16 ->
            "Main Color"

        19 ->
            "Tan"

        24 ->
            "Edge"

        28 ->
            "Dark Tan"

        71 ->
            "Light Bluish Gray"

        72 ->
            "Dark Bluish Gray"

        320 ->
            "Dark Red"

        321 ->
            "Dark Blue"

        323 ->
            "Light Aqua"

        378 ->
            "Sand Green"

        379 ->
            "Sand Blue"

        _ ->
            "Color " ++ String.fromInt code


colorCssRgb : Int -> String
colorCssRgb code =
    let
        c =
            Colors.resolveColor 16 code

        channel v =
            String.fromInt (clamp 0 255 (round (v * 255)))
    in
    "rgb(" ++ channel c.r ++ ", " ++ channel c.g ++ ", " ++ channel c.b ++ ")"


{-| Generate a standalone, printable HTML document containing the complete
step-by-step building instructions with parts callouts and print stylesheets.
-}
exportHtml : String -> List Step -> String
exportHtml modelTitle steps =
    let
        totalParts =
            steps
                |> List.concatMap .placements
                |> List.length

        totalSteps =
            List.length steps

        renderCalloutItem item =
            let
                swatchColor =
                    colorCssRgb item.color

                name =
                    colorName item.color
            in
            """
            <div class="part-chip">
              <span class="swatch" style="background-color: """ ++ swatchColor ++ """;"></span>
              <span class="count">""" ++ String.fromInt item.count ++ """&times;</span>
              <span class="part-id">""" ++ item.part ++ """</span>
              <span class="color-name">(""" ++ name ++ """)</span>
            </div>
            """

        renderStep step =
            let
                calloutHtml =
                    if List.isEmpty step.callout then
                        "<div class=\"empty-callout\">No new parts</div>"

                    else
                        String.join "\n" (List.map renderCalloutItem step.callout)

                stepPlacementCount =
                    List.length step.placements

                accPlacementCount =
                    List.length step.accumulatedPlacements
            in
            """
            <section class="step-card">
              <header class="step-header">
                <h2>Step """ ++ String.fromInt step.index ++ """ <span class="step-of">of """ ++ String.fromInt totalSteps ++ """</span></h2>
                <span class="part-badge">""" ++ String.fromInt stepPlacementCount ++ """ new / """ ++ String.fromInt accPlacementCount ++ """ total parts</span>
              </header>
              <div class="callout-box">
                <div class="callout-title">Parts needed for this step:</div>
                <div class="callout-grid">
                  """ ++ calloutHtml ++ """
                </div>
              </div>
            </section>
            """

        stepsHtml =
            String.join "\n" (List.map renderStep steps)
    in
    """<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>Building Instructions — """ ++ modelTitle ++ """</title>
  <style>
    :root {
      --bg-page: #f8fafc;
      --bg-card: #ffffff;
      --text-main: #0f172a;
      --text-muted: #64748b;
      --border-color: #e2e8f0;
      --accent-color: #2563eb;
    }
    body {
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
      margin: 0;
      padding: 2rem;
      background: var(--bg-page);
      color: var(--text-main);
      line-height: 1.5;
    }
    .container {
      max-width: 800px;
      margin: 0 auto;
    }
    .doc-header {
      border-bottom: 2px solid var(--border-color);
      padding-bottom: 1.5rem;
      margin-bottom: 2rem;
    }
    .doc-header h1 {
      margin: 0 0 0.5rem 0;
      font-size: 2rem;
    }
    .doc-summary {
      color: var(--text-muted);
      font-size: 1rem;
    }
    .step-card {
      background: var(--bg-card);
      border: 1px solid var(--border-color);
      border-radius: 8px;
      padding: 1.5rem;
      margin-bottom: 2rem;
      box-shadow: 0 1px 3px rgba(0,0,0,0.05);
      page-break-inside: avoid;
      break-inside: avoid;
    }
    .step-header {
      display: flex;
      justify-content: space-between;
      align-items: baseline;
      border-bottom: 1px solid var(--border-color);
      padding-bottom: 0.75rem;
      margin-bottom: 1rem;
    }
    .step-header h2 {
      margin: 0;
      font-size: 1.5rem;
    }
    .step-of {
      font-size: 1rem;
      color: var(--text-muted);
      font-weight: normal;
    }
    .part-badge {
      font-size: 0.875rem;
      color: var(--text-muted);
      background: var(--bg-page);
      padding: 0.25rem 0.5rem;
      border-radius: 4px;
    }
    .callout-box {
      background: var(--bg-page);
      border: 1px dashed var(--border-color);
      border-radius: 6px;
      padding: 1rem;
    }
    .callout-title {
      font-weight: 600;
      font-size: 0.875rem;
      color: var(--text-muted);
      margin-bottom: 0.75rem;
      text-transform: uppercase;
      letter-spacing: 0.05em;
    }
    .callout-grid {
      display: flex;
      flex-wrap: wrap;
      gap: 0.75rem;
    }
    .part-chip {
      display: inline-flex;
      align-items: center;
      gap: 0.5rem;
      background: var(--bg-card);
      border: 1px solid var(--border-color);
      border-radius: 4px;
      padding: 0.35rem 0.65rem;
      font-size: 0.9rem;
    }
    .swatch {
      display: inline-block;
      width: 14px;
      height: 14px;
      border-radius: 2px;
      border: 1px solid rgba(0,0,0,0.2);
    }
    .count {
      font-weight: 700;
      color: var(--accent-color);
    }
    .part-id {
      font-family: monospace;
      font-weight: 600;
    }
    .color-name {
      color: var(--text-muted);
      font-size: 0.8rem;
    }
    .empty-callout {
      color: var(--text-muted);
      font-style: italic;
      font-size: 0.875rem;
    }
    @media print {
      body {
        padding: 0;
        background: #ffffff;
      }
      .step-card {
        border: none;
        box-shadow: none;
        page-break-after: always;
        break-after: page;
        margin-bottom: 0;
        padding: 1.5cm 1cm;
      }
    }
  </style>
</head>
<body>
  <div class="container">
    <header class="doc-header">
      <h1>""" ++ modelTitle ++ """</h1>
      <div class="doc-summary">Building Instructions &bull; """ ++ String.fromInt totalSteps ++ """ steps &bull; """ ++ String.fromInt totalParts ++ """ total parts</div>
    </header>
    <main>
      """ ++ stepsHtml ++ """
    </main>
  </div>
</body>
</html>"""
