module Studio.DragPlane exposing
    ( Box
    , Mode(..)
    , Target
    , WorkingPlane
    , boxesOverlap
    , choose
    , footprintOverlaps
    , freeSpot
    , freeSpotPlacement
    , intersect
    , snapTo
    , steepElevation
    , supportTop
    , targetPosition
    , toLDraw
    , unionBox
    , worldBox
    )

{-| View-relative body drag: the pure maths behind picking a part up by its body
and sliding it with the cursor.

On press a **working plane** is fixed through the grab point `G` (render space,
Y up), chosen once from the camera (`choose`):

  - `Horizontal` (normal +Y) when the camera looks down or up steeply,
    `abs elevation >= steepElevation`. Screen left/right is camera-left/right on
    the ground, screen up/down is away/toward the camera.
  - `Facing`, a vertical plane facing the camera, when it is near the horizon.
    Screen left/right is camera-left/right, screen up/down is up/down. A
    horizontal plane there would run off to the horizon.

Each move intersects the screen ray with that plane (`intersect`) and moves the
lead part by `P - G`, so the grabbed point stays under the cursor (before grid
and stud snapping). `targetPosition` then applies the grid and the height rule:
in `Horizontal` mode the dragged parts rest on whatever their footprint overlaps
(`supportTop`), in `Facing` mode the height follows the plane.

X and Z snap to the grid **shifted by the part's stud offset** (`snapTo`): a
studded part's origin lands on the stud lattice (studs at 10 mod 20 LDU), so a
1×1 sits at ±10 and a 2×4 at 0 mod 20. The offset comes from
`LDraw.Connectivity.studOffset` for the placement's rotation, and is `{ x = 0,
z = 0 }` for Technic parts. Y is not shifted.

`freeSpot` reuses the same boxes to find room for a new part: the nearest grid
position on that same shifted grid, searched ring by ring outward from a start
point, where the part's box overlaps nothing already placed.

Render space is LDraw scaled by `(1, -1, -1)`; `Box` values and positions given
to `targetPosition` are in LDraw coordinates, where up is **negative** Y and the
ground top is Y = 0.

-}

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Render.Camera exposing (Camera)


{-| An axis-aligned box, in LDraw coordinates unless stated otherwise.
-}
type alias Box =
    { min : Vec3, max : Vec3 }


{-| Which way the working plane faces.
-}
type Mode
    = Horizontal
    | Facing


{-| The plane a body drag slides on: through `point` with unit `normal`, both in
render space.
-}
type alias WorkingPlane =
    { point : Vec3
    , normal : Vec3
    , mode : Mode
    }


{-| Camera elevation (either sign) from which the drag plane is horizontal: 20°.
-}
steepElevation : Float
steepElevation =
    degrees 20


{-| The working plane for a drag grabbed at `grab` (render space).
-}
choose : Camera -> Vec3 -> WorkingPlane
choose camera grab =
    if abs camera.elevation >= steepElevation then
        { point = grab, normal = vec3 0 1 0, mode = Horizontal }

    else
        { point = grab
        , normal = vec3 (sin camera.azimuth) 0 (cos camera.azimuth)
        , mode = Facing
        }


{-| Where a ray `( origin, direction )` (render space) meets the plane, or
`Nothing` when it runs parallel to it or the plane is behind the eye.
-}
intersect : WorkingPlane -> ( Vec3, Vec3 ) -> Maybe Vec3
intersect plane ( origin, direction ) =
    let
        denom =
            Vec3.dot direction plane.normal
    in
    if abs denom < 1.0e-6 then
        Nothing

    else
        let
            t =
                Vec3.dot (Vec3.sub plane.point origin) plane.normal / denom
        in
        if t <= 0 || isNaN t || isInfinite t then
            Nothing

        else
            Just (Vec3.add origin (Vec3.scale t direction))


{-| Render space (Y up) to LDraw (Y down) and back: the conversion is its own
inverse, and works for points and deltas alike.
-}
toLDraw : Vec3 -> Vec3
toLDraw v =
    vec3 (Vec3.getX v) -(Vec3.getY v) -(Vec3.getZ v)


{-| The axis-aligned bounds of a local box under a placement matrix.
-}
worldBox : Mat4 -> Box -> Box
worldBox matrix local =
    let
        xs =
            [ Vec3.getX local.min, Vec3.getX local.max ]

        ys =
            [ Vec3.getY local.min, Vec3.getY local.max ]

        zs =
            [ Vec3.getZ local.min, Vec3.getZ local.max ]

        corners =
            List.concatMap (\x -> List.concatMap (\y -> List.map (\z -> Mat4.transform matrix (vec3 x y z)) zs) ys) xs
    in
    unionBox corners
        |> Maybe.withDefault { min = Mat4.transform matrix local.min, max = Mat4.transform matrix local.min }


{-| The smallest box around a list of points, `Nothing` for none. Pass box
corners (`[ b.min, b.max ]`) to merge boxes.
-}
unionBox : List Vec3 -> Maybe Box
unionBox points =
    case points of
        first :: rest ->
            Just
                (List.foldl
                    (\p acc ->
                        { min = vec3 (min (Vec3.getX p) (Vec3.getX acc.min)) (min (Vec3.getY p) (Vec3.getY acc.min)) (min (Vec3.getZ p) (Vec3.getZ acc.min))
                        , max = vec3 (max (Vec3.getX p) (Vec3.getX acc.max)) (max (Vec3.getY p) (Vec3.getY acc.max)) (max (Vec3.getZ p) (Vec3.getZ acc.max))
                        }
                    )
                    { min = first, max = first }
                    rest
                )

        [] ->
            Nothing


{-| Whether two boxes overlap in plan view (X and Z). Boxes that only touch, or
overlap by less than half an LDU, do not: side-by-side bricks are not supports.
-}
footprintOverlaps : Box -> Box -> Bool
footprintOverlaps a b =
    let
        margin =
            0.5

        overlapOn get =
            min (get a.max) (get b.max) - max (get a.min) (get b.min) > margin
    in
    overlapOn Vec3.getX && overlapOn Vec3.getZ


{-| Whether two boxes overlap in all three axes. Boxes that only touch, or
overlap by less than half an LDU, do not: side-by-side bricks are not a clash.
-}
boxesOverlap : Box -> Box -> Bool
boxesOverlap a b =
    footprintOverlaps a b
        && (min (Vec3.getY a.max) (Vec3.getY b.max) - max (Vec3.getY a.min) (Vec3.getY b.min) > 0.5)


{-| The value nearest to `value` on the grid `offset + k * step` (LDU), `k` an
integer. A value exactly halfway between two grid points goes to the higher one.

With `offset` 0 this is the plain grid; a studded part passes its stud offset
(`LDraw.Connectivity.studOffset`) so its origin lands on the stud lattice.

-}
snapTo : Float -> Float -> Float -> Float
snapTo offset step value =
    offset + toFloat (round ((value - offset) / step)) * step


{-| The nearest free grid position (X, Z in LDraw) for a new part.

  - `box` — the new part's world box (LDraw) with its origin at X = Z = 0 and
    already at its final height and orientation.
  - `offset` — the part's stud offset (X and Z, see `snapTo`): the grid is shifted
    by it.
  - `start` — where to look first (X, Z); snapped to that grid.
  - `step` — the grid step, in LDU.
  - `others` — world boxes of the parts already placed.

Candidates are the grid positions on square rings around `start` (ring 0 is
`start` itself, ring `r` the positions `r` steps away), nearest first within a
ring; the first whose shifted `box` overlaps none of `others` (`boxesOverlap`)
wins. Gives up after 64 rings and returns the position one ring further along +X.

-}
freeSpot : { box : Box, offset : { x : Float, z : Float }, start : ( Float, Float ), step : Float, others : List Box } -> ( Float, Float )
freeSpot { box, offset, start, step, others } =
    let
        gridStep =
            max 1 step

        ( sx, sz ) =
            ( snapTo offset.x gridStep (Tuple.first start)
            , snapTo offset.z gridStep (Tuple.second start)
            )

        maxRings =
            64

        shifted ( x, z ) =
            { min = Vec3.add box.min (vec3 x 0 z), max = Vec3.add box.max (vec3 x 0 z) }

        isFree spot =
            let
                candidate =
                    shifted spot
            in
            not (List.any (boxesOverlap candidate) others)

        ring r =
            if r == 0 then
                [ ( 0, 0 ) ]

            else
                List.range -r r
                    |> List.concatMap (\i -> List.range -r r |> List.map (\j -> ( i, j )))
                    |> List.filter (\( i, j ) -> max (abs i) (abs j) == r)
                    |> List.sortBy (\( i, j ) -> ( i * i + j * j, -i, j ))

        search r =
            if r > maxRings then
                ( sx + toFloat (maxRings + 1) * gridStep, sz )

            else
                case
                    ring r
                        |> List.map (\( i, j ) -> ( sx + toFloat i * gridStep, sz + toFloat j * gridStep ))
                        |> List.filter isFree
                of
                    spot :: _ ->
                        spot

                    [] ->
                        search (r + 1)
    in
    search 0


{-| `freeSpot` for a whole placement: a new part given by its matrix, with its
height and orientation kept and only its X and Z moved to the nearest free grid
position. Pure and model-free, so any host can place a part without a
`Studio.Core.Model`.

  - `box` — the part's local box (LDraw) in the frame of `matrix`, as
    `worldBox` takes it.
  - `matrix` — the placement the part would have; its translation's X and Z
    are the only things replaced.
  - `offset` — the stud offset of the part in that orientation (see `freeSpot`).
  - `start` — where to search from (X, Z), e.g. the selection's centre or the
    camera target's ground position.
  - `gridStep` — the grid step, in LDU (at least 1).
  - `others` — world boxes (LDraw) of the parts already placed.

Returns the placement matrix to add the part with (`Edit`/`addPartAt`-style).

    freeSpotPlacement
        { box = localBox
        , matrix = Mat4.makeTranslate3 0 -bottom 0
        , start = ( 0, 0 )
        , gridStep = 20
        , others = []
        }

-}
freeSpotPlacement :
    { box : Box
    , matrix : Mat4
    , offset : { x : Float, z : Float }
    , start : ( Float, Float )
    , gridStep : Float
    , others : List Box
    }
    -> Mat4
freeSpotPlacement { box, matrix, offset, start, gridStep, others } =
    let
        origin =
            Mat4.transform matrix (vec3 0 0 0)

        atOrigin =
            Mat4.mul (Mat4.makeTranslate3 -(Vec3.getX origin) 0 -(Vec3.getZ origin)) matrix

        ( x, z ) =
            freeSpot
                { box = worldBox atOrigin box
                , offset = offset
                , start = start
                , step = gridStep
                , others = others
                }
    in
    Mat4.mul (Mat4.makeTranslate3 x 0 z) atOrigin


{-| The Y (LDraw, up is negative) a footprint would rest on: the highest top
(smallest `min` Y) among `others` whose plan overlaps it, or the ground (0)
when none does or every overlapping box is below ground.
-}
supportTop : Box -> List Box -> Float
supportTop footprint others =
    others
        |> List.filter (footprintOverlaps footprint)
        |> List.map (\b -> Vec3.getY b.min)
        |> List.foldl min 0


{-| Everything `targetPosition` needs.

  - `plane`, `cursor` — the working plane and where the cursor ray meets it
    (render space); `cursor` minus `plane.point` is the drag delta.
  - `original` — the lead part's position when the drag started (LDraw).
  - `gridStep` — snap X/Z to this grid, in LDU; X and Z are shifted by `offset`
    (see `snapTo`).
  - `verticalStep` — snap Y to this grid in `Facing` mode, in LDU (a height
    step need not match the X/Z grid).
  - `offset` — the lead part's stud offset (`LDraw.Connectivity.studOffset`), so a
    studded part lands on the stud lattice; `{ x = 0, z = 0 }` for Technic parts.
  - `elevationOffset` — the wheel lift (LDraw; negative is up), added last.
  - `footprint` — the world box (LDraw) of everything dragged, at `original`.
  - `others` — world boxes (LDraw) of the placements not being dragged.
  - `keepHeight` — hold the original height (Studio's Shift elevation lock): no
    support search, no plane height and no ground clamp below the start.

-}
type alias Target =
    { plane : WorkingPlane
    , cursor : Vec3
    , original : Vec3
    , gridStep : Float
    , verticalStep : Float
    , offset : { x : Float, z : Float }
    , elevationOffset : Float
    , footprint : Box
    , others : List Box
    , keepHeight : Bool
    }


{-| The lead part's new position (LDraw). Apply the same translation (and any
in-flight rotation) to the rest of the selection to move it rigidly.

In `Horizontal` mode the dragged parts rest exactly on their support at the
target: the bottom of `footprint` sits on the highest top it overlaps
(`supportTop`), or on the ground. Nothing about the height at press time is
remembered, so a part slid off a brick drops to the ground and one that was
floating (a tile mounted sideways, say) settles onto what is below it. Parts are
created resting on the ground (`Studio.Core.addPart`), so a fresh part does not
jump on the first move. In `Facing` mode the height follows the plane in grid
steps. The wheel `elevationOffset` is added last in both modes.

The dragged parts never go below the ground — except with `keepHeight`, which
holds the press-time height exactly, so a part already sunk stays where it is.

-}
targetPosition : Target -> Vec3
targetPosition t =
    let
        delta =
            toLDraw (Vec3.sub t.cursor t.plane.point)

        step =
            max 1.0e-3 t.gridStep

        vertical =
            max 1.0e-3 t.verticalStep

        snapY v =
            toFloat (round (v / vertical)) * vertical

        ox =
            Vec3.getX t.original

        oy =
            Vec3.getY t.original

        oz =
            Vec3.getZ t.original

        x =
            snapTo t.offset.x step (ox + Vec3.getX delta)

        z =
            snapTo t.offset.z step (oz + Vec3.getZ delta)

        moved =
            { min = Vec3.add t.footprint.min (vec3 (x - ox) 0 (z - oz))
            , max = Vec3.add t.footprint.max (vec3 (x - ox) 0 (z - oz))
            }

        -- Origin height at which the footprint's bottom sits at `top`.
        restingOn top =
            oy + (top - Vec3.getY t.footprint.max)

        baseY =
            if t.keepHeight then
                oy

            else
                case t.plane.mode of
                    Horizontal ->
                        restingOn (supportTop moved t.others)

                    Facing ->
                        oy + snapY (Vec3.getY delta)

        lowest =
            if t.keepHeight then
                max (restingOn 0) oy

            else
                restingOn 0
    in
    vec3 x (min lowest (baseY + t.elevationOffset)) z
