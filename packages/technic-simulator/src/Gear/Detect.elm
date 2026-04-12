module Gear.Detect exposing (buildGearGraph, extractGears)

{-| Gear detection: identify known Technic gear parts within a loaded LDraw
model and build a connectivity graph from their world positions.

## Detection algorithm

1. `extractGears` — recursive walk of the part tree (same structure as
   `LDraw.Geometry.flatten`). Every `SubFileRef` whose normalised filename
   matches a provided gear-spec entry becomes a `GearInstance`. The world matrix is
   accumulated through the call stack exactly as geometry flattening does it.

2. `buildGearGraph` — O(n²) pair comparison. Two gears mesh when the Euclidean
   distance between their axle centres ≈ the sum of their pitch radii, within
   a ±4 LDU tolerance (empirically derived from standard LEGO geometry).

-}

import Array
import Dict exposing (Dict)
import Gear.Types exposing (GearGraph, GearId, GearInstance, GearSpec)
import LDraw.Resolve exposing (PartCache, PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)


-- ── Public API ────────────────────────────────────────────────────────────────


{-| Walk the part tree and collect all gear instances.

    extractGears gearSpecs topLevelLines partCache

Returns instances in the order they are encountered (depth-first). IDs are
assigned sequentially starting at 0.

-}
extractGears : List GearSpec -> List LDrawLine -> PartCache -> List GearInstance
extractGears gearSpecs lines cache =
    walkLines gearSpecs lines cache Mat4.identity []
        |> List.reverse
        |> List.indexedMap (\i inst -> { inst | id = i })


{-| Build the gear adjacency graph from a list of gear instances.

Two gears are considered meshing when:

    |distance(g1.worldPosition, g2.worldPosition) - (g1.pitchRadius + g2.pitchRadius)| <= meshTolerance

-}
buildGearGraph : List GearInstance -> GearGraph
buildGearGraph instances =
    let
        arr =
            Array.fromList instances

        n =
            Array.length arr

        connections =
            List.foldl
                (\i acc ->
                    List.foldl
                        (\j acc2 ->
                            case ( Array.get i arr, Array.get j arr ) of
                                ( Just g1, Just g2 ) ->
                                    if meshing g1 g2 then
                                        -- Worm drives are self-locking: only add the
                                        -- worm→wheel direction so that BFS cannot
                                        -- back-drive a worm from the wheel side.
                                        if isWorm g1.spec then
                                            acc2 |> addConnection i j

                                        else if isWorm g2.spec then
                                            acc2 |> addConnection j i

                                        else
                                            acc2
                                                |> addConnection i j
                                                |> addConnection j i

                                    else
                                        acc2

                                _ ->
                                    acc2
                        )
                        acc
                        (List.range (i + 1) (n - 1))
                )
                Dict.empty
                (List.range 0 (n - 1))
    in
    { instances = arr
    , connections = connections
    }


-- ── Internal ──────────────────────────────────────────────────────────────────


{-| Radial tolerance in LDU for gear mesh detection.

Scaled from expected centre distance with conservative clamps to avoid
false-positive links in dense models.
-}
meshToleranceFor : Float -> Float
meshToleranceFor expectedDistance =
    clamp 0.8 2.0 (expectedDistance * 0.035)


meshing : GearInstance -> GearInstance -> Bool
meshing g1 g2 =
    let
        dist =
            Vec3.distance g1.worldPosition g2.worldPosition

        expected =
            g1.spec.pitchRadius + g2.spec.pitchRadius

        axis1 =
            gearAxis g1

        axis2 =
            gearAxis g2

        absDot =
            abs (Vec3.dot axis1 axis2)

        centerDelta =
            Vec3.sub g2.worldPosition g1.worldPosition

        axialOffset =
            abs (Vec3.dot centerDelta axis1)

        axisOk =
            if isCrownLike g1.spec || isCrownLike g2.spec then
                -- Crown gears mesh with perpendicular mating gears.
                absDot <= 0.35

            else if isBevelLike g1.spec || isBevelLike g2.spec then
                -- Bevel/crown meshes are approximately perpendicular
                absDot <= 0.35

            else
                -- Spur meshes share a common axle direction
                absDot >= 0.9 && axialOffset <= 2.0

        radialTolerance =
            meshToleranceFor expected
    in
    axisOk && abs (dist - expected) <= radialTolerance


gearAxis : GearInstance -> Vec3
gearAxis inst =
    let
        origin =
            Mat4.transform inst.worldMatrix (Vec3.vec3 0 0 0)

        alongLocalZ =
            Mat4.transform inst.worldMatrix (Vec3.vec3 0 0 1)

        raw =
            Vec3.sub alongLocalZ origin

        len =
            Vec3.length raw
    in
    if len < 1.0e-6 then
        Vec3.vec3 0 0 1

    else
        Vec3.scale (1 / len) raw


isWorm : GearSpec -> Bool
isWorm spec =
    spec.teeth == 1


isBevelLike : GearSpec -> Bool
isBevelLike spec =
    List.member spec.partFile [ "32198.dat", "32269.dat" ]


isCrownLike : GearSpec -> Bool
isCrownLike spec =
    spec.partFile == "3650b.dat"


addConnection : GearId -> GearId -> Dict GearId (List GearId) -> Dict GearId (List GearId)
addConnection from to dict =
    Dict.update from
        (\existing ->
            case existing of
                Nothing ->
                    Just [ to ]

                Just xs ->
                    Just (to :: xs)
        )
        dict


{-| Recursive part-tree walk. Accumulates gear instances (without final IDs —
those are assigned after the walk in `extractGears`).
-}
walkLines : List GearSpec -> List LDrawLine -> PartCache -> Mat4 -> List GearInstance -> List GearInstance
walkLines gearSpecs lines cache worldMat acc =
    List.foldl (walkLine gearSpecs cache worldMat) acc lines


walkLine : List GearSpec -> PartCache -> Mat4 -> LDrawLine -> List GearInstance -> List GearInstance
walkLine gearSpecs cache worldMat line acc =
    case line of
        SubFileRef { file, transform } ->
            let
                combinedMat =
                    Mat4.mul worldMat transform
            in
            case matchGear gearSpecs file of
                Just spec ->
                    -- This sub-file is a known gear — record it
                    let
                        worldPos =
                            Mat4.transform combinedMat (Vec3.vec3 0 0 0)

                        inst =
                            { id = 0 -- placeholder; set after walk
                            , spec = spec
                            , worldPosition = worldPos
                            , worldMatrix = combinedMat
                            }
                    in
                    inst :: acc

                Nothing ->
                    -- Not a gear — recurse into its sub-parts if cached
                    case Dict.get file cache of
                        Just (Loaded subLines) ->
                            walkLines gearSpecs subLines cache combinedMat acc

                        _ ->
                            acc

        _ ->
            acc


matchGear : List GearSpec -> String -> Maybe GearSpec
matchGear gearSpecs file =
    gearSpecs
        |> List.filter (\spec -> spec.partFile == file)
        |> List.head
