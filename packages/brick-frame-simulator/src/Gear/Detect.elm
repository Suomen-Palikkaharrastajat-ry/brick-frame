module Gear.Detect exposing (buildGearGraph, buildGearGraphReference, extractGears)

{-| Gear detection: identify known gear parts within a loaded LDraw
model and build a connectivity graph from their world positions.


## Detection algorithm

1.  `extractGears` — recursive walk of the part tree (same structure as
    `LDraw.Geometry.flatten`). Every `SubFileRef` whose normalised filename
    matches a provided gear-spec entry becomes a `GearInstance`. The world matrix is
    accumulated through the call stack exactly as geometry flattening does it.

2.  `buildGearGraph` — O(n²) pair comparison. Two gears mesh when the Euclidean
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
import Set exposing (Set)



-- ── Public API ────────────────────────────────────────────────────────────────


{-| Walk the part tree and collect all gear instances.

    extractGears gearSpecs topLevelLines partCache

Returns instances in the order they are encountered (depth-first). IDs are
assigned sequentially starting at 0.

-}
extractGears : List GearSpec -> List LDrawLine -> PartCache -> List GearInstance
extractGears gearSpecs lines cache =
    walkLines gearSpecs lines cache 15 Mat4.identity []
        |> List.reverse
        |> List.indexedMap (\i inst -> { inst | id = i })


{-| Build the gear adjacency graph from a list of gear instances.

Two gears are considered meshing when:

    |distance(g1.worldPosition, g2.worldPosition) - (g1.pitchRadius + g2.pitchRadius)| <= meshTolerance

Rather than comparing every pair (O(n²)), candidate pairs are enumerated from
two spatial indexes and then classified with the exact `meshing` / `coAxial`
tests. The output is identical to `buildGearGraphReference` — including the
order of each adjacency list — because the candidate pairs are folded in
ascending `(i, j)` order (via `Set (Int, Int)`), matching the reference's nested
`i < j` iteration.

Two indexes are needed because the two relations have different distance
bounds:

  - **Meshing** is distance-bounded (`dist ≈ r1 + r2`), so a 3D position hash
    with a 27-cell neighbour sweep captures every meshing pair.
  - **Co-axial** is _not_ distance-bounded — two gears on the same axle are
    co-axial at any axial separation. Hashing by position would silently drop
    long-axle rigid links. Instead gears are hashed by their _axle line_
    (canonical axis direction + closest point to the origin), so gears sharing
    an axle land in the same bucket however far apart they sit along it.

-}
buildGearGraph : List GearInstance -> GearGraph
buildGearGraph instances =
    let
        arr =
            Array.fromList instances

        indexed =
            Array.toIndexedList arr

        maxPitchRadius =
            List.foldl (\g m -> max m g.spec.pitchRadius) 0.0 instances

        -- A meshing pair is at most `2 * maxPitchRadius + tolerance` apart
        -- (tolerance ≤ 2.0), so a cell this size guarantees both gears fall in
        -- the same or an adjacent cell.
        meshCell =
            2.0 * maxPitchRadius + 2.0

        meshKey p =
            ( floor (Vec3.getX p / meshCell)
            , floor (Vec3.getY p / meshCell)
            , floor (Vec3.getZ p / meshCell)
            )

        meshBuckets =
            List.foldl
                (\( idx, g ) d -> Dict.update (meshKey g.worldPosition) (prependIndex idx) d)
                Dict.empty
                indexed

        axleBuckets =
            List.foldl
                (\( idx, g ) d -> Dict.update (axleKey g) (prependIndex idx) d)
                Dict.empty
                indexed

        candidatePairs =
            List.foldl
                (\( idx, g ) set ->
                    let
                        neighbours =
                            meshNeighbourIndices (meshKey g.worldPosition) meshBuckets
                                ++ axleNeighbourIndices (axleKey g) axleBuckets
                    in
                    List.foldl
                        (\j s ->
                            if j /= idx then
                                Set.insert (orderPair idx j) s

                            else
                                s
                        )
                        set
                        neighbours
                )
                Set.empty
                indexed

        ( connections, rigidAxles ) =
            Set.foldl
                (\( i, j ) acc ->
                    case ( Array.get i arr, Array.get j arr ) of
                        ( Just g1, Just g2 ) ->
                            classifyPair i j g1 g2 acc

                        _ ->
                            acc
                )
                ( Dict.empty, Dict.empty )
                candidatePairs
    in
    { instances = arr
    , connections = connections
    , rigidAxles = rigidAxles
    }


{-| Reference O(n²) implementation of `buildGearGraph`, retained as the
correctness oracle for the spatial-hash version (see `Gear.DetectTest`).
-}
buildGearGraphReference : List GearInstance -> GearGraph
buildGearGraphReference instances =
    let
        arr =
            Array.fromList instances

        n =
            Array.length arr

        ( connections, rigidAxles ) =
            List.foldl
                (\i accOuter ->
                    List.foldl
                        (\j acc ->
                            case ( Array.get i arr, Array.get j arr ) of
                                ( Just g1, Just g2 ) ->
                                    classifyPair i j g1 g2 acc

                                _ ->
                                    acc
                        )
                        accOuter
                        (List.range (i + 1) (n - 1))
                )
                ( Dict.empty, Dict.empty )
                (List.range 0 (n - 1))
    in
    { instances = arr
    , connections = connections
    , rigidAxles = rigidAxles
    }


{-| Classify a single ordered pair (`i < j`) and fold its edges into the
`( connections, rigidAxles )` accumulator. Shared verbatim by both graph
builders so their per-pair behaviour — and adjacency-list order — cannot drift.
-}
classifyPair :
    GearId
    -> GearId
    -> GearInstance
    -> GearInstance
    -> ( Dict GearId (List GearId), Dict GearId (List GearId) )
    -> ( Dict GearId (List GearId), Dict GearId (List GearId) )
classifyPair i j g1 g2 ( accC, accA ) =
    if meshing g1 g2 then
        -- Worm drives are self-locking: only add the worm→wheel direction so
        -- that BFS cannot back-drive a worm from the wheel side.
        if isWorm g1.spec then
            ( accC |> addConnection i j, accA )

        else if isWorm g2.spec then
            ( accC |> addConnection j i, accA )

        else
            ( accC
                |> addConnection i j
                |> addConnection j i
            , accA
            )

    else if coAxial g1 g2 then
        ( accC
        , accA
            |> addConnection i j
            |> addConnection j i
        )

    else
        ( accC, accA )



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

            else if isWorm g1.spec || isWorm g2.spec then
                -- Worm-spur meshes have perpendicular axes
                absDot <= 0.35

            else
                -- Spur meshes share a common axle direction
                absDot >= 0.9 && axialOffset <= 2.0

        radialTolerance =
            meshToleranceFor expected
    in
    axisOk && abs (dist - expected) <= radialTolerance


{-| True when two gears share a physical axle: parallel axes with negligible
radial offset between their centres. Such gears rotate together at 1:1.
-}
coAxial : GearInstance -> GearInstance -> Bool
coAxial g1 g2 =
    let
        axis1 =
            gearAxis g1

        centerDelta =
            Vec3.sub g2.worldPosition g1.worldPosition

        axialProjection =
            Vec3.dot centerDelta axis1

        radialComponent =
            Vec3.sub centerDelta (Vec3.scale axialProjection axis1)

        radialOffset =
            Vec3.length radialComponent

        absDot =
            abs (Vec3.dot axis1 (gearAxis g2))
    in
    absDot >= 0.99 && radialOffset <= 2.0


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



-- ── Spatial indexing (pair enumeration only) ──────────────────────────────────


prependIndex : Int -> Maybe (List Int) -> Maybe (List Int)
prependIndex idx existing =
    Just (idx :: Maybe.withDefault [] existing)


orderPair : Int -> Int -> ( Int, Int )
orderPair a b =
    if a < b then
        ( a, b )

    else
        ( b, a )


{-| The 27 neighbour offsets (including the zero offset) of a 3D grid cell.
-}
neighbourOffsets : List ( Int, Int, Int )
neighbourOffsets =
    let
        steps =
            [ -1, 0, 1 ]
    in
    List.concatMap
        (\dx -> List.concatMap (\dy -> List.map (\dz -> ( dx, dy, dz )) steps) steps)
        steps


{-| Gather gear indices from the 27 cells surrounding a meshing-hash cell.
-}
meshNeighbourIndices : ( Int, Int, Int ) -> Dict ( Int, Int, Int ) (List Int) -> List Int
meshNeighbourIndices ( cx, cy, cz ) buckets =
    List.concatMap
        (\( dx, dy, dz ) ->
            Dict.get ( cx + dx, cy + dy, cz + dz ) buckets
                |> Maybe.withDefault []
        )
        neighbourOffsets


{-| Cell size for the canonical-axis-direction dimension of the axle hash. A
co-axial pair may differ in axis direction by up to `acos 0.99 ≈ 8°`
(per-component difference ≤ 0.14); this cell plus the ±1 sweep captures that.
-}
axleDirCell : Float
axleDirCell =
    0.2


{-| Cell size for the closest-point dimension of the axle hash. `coAxial`
requires a radial offset ≤ 2.0, so co-axial gears' closest points differ by at
most that (plus small axis-tilt wobble); this cell plus the ±1 sweep captures it.
-}
axleQCell : Float
axleQCell =
    3.0


{-| The axle-line bucket key for a gear: its canonical axis direction and the
closest point on that axis to the origin, each quantized. Two gears sharing an
axle map to the same (or an adjacent) bucket regardless of axial separation.
-}
axleKey : GearInstance -> ( ( Int, Int, Int ), ( Int, Int, Int ) )
axleKey g =
    let
        a =
            canonicalAxis (gearAxis g)

        p =
            g.worldPosition

        closest =
            Vec3.sub p (Vec3.scale (Vec3.dot p a) a)
    in
    ( ( floor (Vec3.getX a / axleDirCell)
      , floor (Vec3.getY a / axleDirCell)
      , floor (Vec3.getZ a / axleDirCell)
      )
    , ( floor (Vec3.getX closest / axleQCell)
      , floor (Vec3.getY closest / axleQCell)
      , floor (Vec3.getZ closest / axleQCell)
      )
    )


{-| Gather gear indices from the buckets neighbouring an axle-hash bucket,
sweeping ±1 in each of the six quantized dimensions.
-}
axleNeighbourIndices :
    ( ( Int, Int, Int ), ( Int, Int, Int ) )
    -> Dict ( ( Int, Int, Int ), ( Int, Int, Int ) ) (List Int)
    -> List Int
axleNeighbourIndices ( ( ax, ay, az ), ( qx, qy, qz ) ) buckets =
    List.concatMap
        (\( dax, day, daz ) ->
            List.concatMap
                (\( dqx, dqy, dqz ) ->
                    Dict.get
                        ( ( ax + dax, ay + day, az + daz )
                        , ( qx + dqx, qy + dqy, qz + dqz )
                        )
                        buckets
                        |> Maybe.withDefault []
                )
                neighbourOffsets
        )
        neighbourOffsets


{-| Canonicalize an axis direction so that `a` and `-a` map to the same value:
flip the sign so the first non-negligible component is positive. `coAxial`
already treats the two directions as equivalent (via `absDot`), so this collapses
them into one bucket.
-}
canonicalAxis : Vec3 -> Vec3
canonicalAxis a =
    let
        x =
            Vec3.getX a

        y =
            Vec3.getY a

        z =
            Vec3.getZ a

        eps =
            1.0e-6
    in
    if x > eps then
        a

    else if x < -eps then
        Vec3.scale -1 a

    else if y > eps then
        a

    else if y < -eps then
        Vec3.scale -1 a

    else if z >= 0 then
        a

    else
        Vec3.scale -1 a


{-| Recursive part-tree walk. Accumulates gear instances (without final IDs —
those are assigned after the walk in `extractGears`).
-}
walkLines : List GearSpec -> List LDrawLine -> PartCache -> Int -> Mat4 -> List GearInstance -> List GearInstance
walkLines gearSpecs lines cache parentColor worldMat acc =
    List.foldl (walkLine gearSpecs cache parentColor worldMat) acc lines


walkLine : List GearSpec -> PartCache -> Int -> Mat4 -> LDrawLine -> List GearInstance -> List GearInstance
walkLine gearSpecs cache parentColor worldMat line acc =
    case line of
        SubFileRef { file, transform, color } ->
            let
                combinedMat =
                    Mat4.mul worldMat transform

                childColor =
                    if color == 16 || color == -1 then
                        parentColor

                    else
                        color
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
                            , color = childColor
                            , worldPosition = worldPos
                            , worldMatrix = combinedMat
                            }
                    in
                    inst :: acc

                Nothing ->
                    -- Not a gear — recurse into its sub-parts if cached
                    case Dict.get file cache of
                        Just (Loaded subLines) ->
                            walkLines gearSpecs subLines cache childColor combinedMat acc

                        _ ->
                            acc

        _ ->
            acc


matchGear : List GearSpec -> String -> Maybe GearSpec
matchGear gearSpecs file =
    gearSpecs
        |> List.filter (\spec -> spec.partFile == file)
        |> List.head
