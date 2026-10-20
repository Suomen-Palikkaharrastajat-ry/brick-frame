module Viewport.Framing exposing
    ( ModelBounds
    , ViewPreset(..)
    , applyZoomRange
    , cameraForBounds
    , cameraForStep
    , groundLevel
    , modelBoundsFromInstanced
    , modelBoundsFromLines
    , viewPresetAngles
    )

{-| Where the camera goes: model bounds, auto-fit, zoom range and canonical
view presets.

Everything here is a function of the model's bounding sphere, never of fixed
distances, so city-scale and single-brick models both frame sensibly.

-}

import LDraw.Geometry as Geometry
import LDraw.Resolve exposing (PartCache)
import LDraw.Steps as Steps exposing (Step)
import LDraw.Types exposing (LDrawLine)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3
import Render.Camera as Camera exposing (Camera)
import Render.Instanced exposing (InstancedScene)


type ViewPreset
    = FrontView
    | BackView
    | LeftView
    | RightView
    | TopView
    | IsometricView


{-| Azimuth and elevation for a preset, in radians.

`TopView` stops just short of straight down, matching the camera's own elevation
clamp.

-}
viewPresetAngles : ViewPreset -> ( Float, Float )
viewPresetAngles preset =
    case preset of
        FrontView ->
            ( 0, 0 )

        BackView ->
            ( pi, 0 )

        LeftView ->
            ( -pi / 2, 0 )

        RightView ->
            ( pi / 2, 0 )

        TopView ->
            ( 0, pi / 2 - 0.01 )

        IsometricView ->
            ( 0.75, 0.6 )


{-| A model's extents, kept so the view can be re-framed on demand without
re-flattening the geometry.

`minCorner`/`maxCorner` are the axis-aligned box; `groundLevel` reads the floor
off it for street-level positioning. `center`/`radius` are the bounding sphere
used for framing and for the far plane.

-}
type alias ModelBounds =
    { minCorner : Vec3.Vec3
    , maxCorner : Vec3.Vec3
    , center : Vec3.Vec3
    , radius : Float
    }


{-| The model's lowest point — where the ground is, in render space.
-}
groundLevel : ModelBounds -> Float
groundLevel bounds =
    Vec3.getY bounds.minCorner


{-| Bounding sphere of the baked geometry path.
-}
modelBoundsFromLines : List LDrawLine -> PartCache -> Maybe ModelBounds
modelBoundsFromLines lines cache =
    let
        geom =
            Geometry.flatten lines cache 15 Mat4.identity

        points =
            (geom.triangles
                |> List.concatMap (\( a, b, c ) -> [ a.position, b.position, c.position ])
            )
                ++ (geom.lines |> List.concatMap (\( p1, p2 ) -> [ p1, p2 ]))
                ++ (geom.conditionalLines |> List.concatMap (\e -> [ e.p1, e.p2, e.c1, e.c2 ]))
    in
    case points of
        [] ->
            Nothing

        p0 :: rest ->
            let
                bounds =
                    List.foldl
                        (\p acc ->
                            { minX = min acc.minX (Vec3.getX p)
                            , maxX = max acc.maxX (Vec3.getX p)
                            , minY = min acc.minY (Vec3.getY p)
                            , maxY = max acc.maxY (Vec3.getY p)
                            , minZ = min acc.minZ (Vec3.getZ p)
                            , maxZ = max acc.maxZ (Vec3.getZ p)
                            }
                        )
                        { minX = Vec3.getX p0
                        , maxX = Vec3.getX p0
                        , minY = Vec3.getY p0
                        , maxY = Vec3.getY p0
                        , minZ = Vec3.getZ p0
                        , maxZ = Vec3.getZ p0
                        }
                        rest

                center =
                    Vec3.vec3
                        ((bounds.minX + bounds.maxX) / 2)
                        ((bounds.minY + bounds.maxY) / 2)
                        ((bounds.minZ + bounds.maxZ) / 2)

                extentX =
                    bounds.maxX - bounds.minX

                extentY =
                    bounds.maxY - bounds.minY

                extentZ =
                    bounds.maxZ - bounds.minZ

                radius =
                    max 1 (sqrt (extentX * extentX + extentY * extentY + extentZ * extentZ) / 2)
            in
            Just
                { minCorner = Vec3.vec3 bounds.minX bounds.minY bounds.minZ
                , maxCorner = Vec3.vec3 bounds.maxX bounds.maxY bounds.maxZ
                , center = center
                , radius = radius
                }


{-| Bounding sphere of the instanced path, from the placement spheres that
`Instanced.build` has already measured.
-}
modelBoundsFromInstanced : InstancedScene -> ModelBounds
modelBoundsFromInstanced instanced =
    { minCorner = instanced.boundsMin
    , maxCorner = instanced.boundsMax
    , center = Vec3.scale 0.5 (Vec3.add instanced.boundsMin instanced.boundsMax)
    , radius = max 1 (0.5 * Vec3.length (Vec3.sub instanced.boundsMax instanced.boundsMin))
    }


{-| Frame a bounding sphere: orbit far enough back that it fits the narrower of
the two field-of-view angles, from the default three-quarter viewpoint.

Split out of `autoFitCamera` so the instanced path can reuse it with bounds
derived from placement spheres, instead of flattening the whole model just to
measure it.

-}
cameraForBounds : Int -> Int -> Vec3.Vec3 -> Float -> Camera -> Camera
cameraForBounds width height center radius currentCamera =
    { currentCamera
        | target = center
        , distance = framedDistance width height radius
        , azimuth = 0.75
        , elevation = 0.6
    }
        |> applyZoomRange width height radius


{-| Frame the camera to focus on the parts introduced in a building instruction step.
Falls back to accumulated placements if the step itself has no placements.
-}
cameraForStep : Int -> Int -> Step -> Camera -> Camera
cameraForStep width height step currentCamera =
    let
        targetPlacements =
            if List.isEmpty step.placements then
                step.accumulatedPlacements

            else
                step.placements
    in
    case Steps.stepBounds targetPlacements of
        Just bounds ->
            cameraForBounds width height bounds.center bounds.radius currentCamera

        Nothing ->
            currentCamera


{-| Orbit distance at which a bounding sphere of `radius` fills the viewport.
-}
framedDistance : Int -> Int -> Float -> Float
framedDistance width height radius =
    let
        aspect =
            toFloat width / max 1 (toFloat height)

        fovY =
            degrees Camera.fovYDegrees

        fovX =
            2 * atan (tan (fovY / 2) * aspect)

        limitingHalfFov =
            max 0.15 (min (fovX / 2) (fovY / 2))
    in
    clamp 8 maxAutoFitDistance (radius / sin limitingHalfFov * 1.25)


{-| Size the interactive zoom range to the model, and record the canvas height
and model radius.

Applied whether or not the camera is being re-framed, because all three are
properties of the model rather than of the current view: a shared `#d=` link
restores a manual camera, and it still needs a range that reaches it. A fixed
ceiling used to snap a city-scale model from its framed distance down to the
limit on the very first wheel tick, with no way back out.

The scene radius is what keeps the far plane honest once the camera is _inside_
the model — see `Render.Camera.farPlane`. This is the only place it is set, so
walk mode inherits it for free.

-}
applyZoomRange : Int -> Int -> Float -> Camera -> Camera
applyZoomRange width height radius currentCamera =
    let
        framed =
            framedDistance width height radius
    in
    currentCamera
        |> Camera.setViewportHeight (toFloat height)
        |> Camera.setSceneRadius radius
        |> Camera.setZoomRange 0.5 (max 2000 (framed * zoomOutHeadroom))


{-| Upper bound on the auto-fit orbit distance, in LDU.

A city-scale model needs to be viewed from several thousand LDU out; the clip
planes now scale with orbit distance (`Render.Camera.farPlane`), so there is no
longer a fixed far plane for this to stay under.

-}
maxAutoFitDistance : Float
maxAutoFitDistance =
    200000


{-| How far past its framed distance a model can be zoomed out, as a multiple.

Enough to pull well back from a city-scale layout and see it in context, while
still stopping short of the point where the model is a speck.

-}
zoomOutHeadroom : Float
zoomOutHeadroom =
    8
