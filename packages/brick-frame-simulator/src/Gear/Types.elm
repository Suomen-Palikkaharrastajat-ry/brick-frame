module Gear.Types exposing
    ( GearGraph
    , GearId
    , GearInstance
    , GearSpec
    )

{-| Core types for gear detection and simulation.

## Coordinate system

All world positions and matrices use the Y-up coordinate system output by
`LDraw.Geometry.flatten` (LDraw Y-down is negated during flattening).

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)


-- ── GearSpec ──────────────────────────────────────────────────────────────────


{-| Static properties of a known gear part.

  - `partFile`    — normalised LDraw filename (lowercase, forward slashes)
  - `teeth`       — tooth count (determines gear ratio)
  - `pitchRadius` — distance from axle centre to the pitch circle in LDraw
    units (LDU). Two gears mesh when their world-space axle separation ≈
    pitchRadius1 + pitchRadius2.

-}
type alias GearSpec =
    { partFile : String
    , teeth : Int
    , pitchRadius : Float
    }


-- ── GearId ────────────────────────────────────────────────────────────────────


{-| Unique identifier for a gear instance within a loaded model.
Simply the zero-based index into the `GearGraph.instances` array.
-}
type alias GearId =
    Int


-- ── GearInstance ──────────────────────────────────────────────────────────────


{-| A concrete occurrence of a known gear part within the loaded model.

  - `id`           — unique index within the model's gear list
  - `spec`         — which gear part this is
  - `worldPosition` — axle centre in Y-up world space
  - `worldMatrix`   — full 4×4 world transform (includes rotation, scale)

-}
type alias GearInstance =
    { id : GearId
    , spec : GearSpec
    , color : Int
    , worldPosition : Vec3
    , worldMatrix : Mat4
    }


-- ── GearGraph ─────────────────────────────────────────────────────────────────


{-| The complete gear connectivity for a loaded model.

  - `instances`   — all detected gear instances, indexed by `GearId`
  - `connections` — adjacency list; each `GearId` maps to the list of
    `GearId`s whose pitch circles touch it
  - `rigidAxles`  — adjacency list for gears that share a physical axle
    (co-axial, zero radial offset). These rotate together 1:1 in the same
    direction, regardless of tooth count.

-}
type alias GearGraph =
    { instances : Array GearInstance
    , connections : Dict GearId (List GearId)
    , rigidAxles : Dict GearId (List GearId)
    }
