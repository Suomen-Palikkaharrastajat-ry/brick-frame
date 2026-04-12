module Gear.Animate exposing (MotorState, defaultMotor, gearAngles)

{-| Motor and animation state for the gear simulation.

The motor drives one gear at a fixed angular speed. All other gear angles are
derived deterministically via `Gear.Physics.propagate` from the motor gear's
cumulative angle (`speed × time`).

-}

import Dict exposing (Dict)
import Gear.Physics as Physics
import Gear.Types exposing (GearGraph, GearId)


-- ── Types ─────────────────────────────────────────────────────────────────────


{-| State of the driving motor.

  - `drivingGearId` — which gear the motor is attached to (`Nothing` = no motor)
  - `speedRadPerSec` — angular speed at the motor gear (rad/s, always positive;
    direction is encoded in the physics sign conventions)
  - `running` — whether the animation loop is active

-}
type alias MotorState =
    { drivingGearId : Maybe GearId
    , speedRadPerSec : Float
    , running : Bool
    }


-- ── Public API ────────────────────────────────────────────────────────────────


{-| Sensible default: 1 rad/s, first gear selected, paused until user starts.
-}
defaultMotor : MotorState
defaultMotor =
    { drivingGearId = Nothing
    , speedRadPerSec = 1.0
    , running = False
    }


{-| Compute the current angle (radians) for every reachable gear.

    gearAngles motor graph totalTime

  - `totalTime` — accumulated simulation time in seconds (monotonically
    increasing while `motor.running = True`)
  - Returns `Dict GearId Float` suitable for passing to the renderer.

Returns an empty dict when no motor gear is selected.

-}
gearAngles : MotorState -> GearGraph -> Float -> Dict GearId Float
gearAngles motor graph totalTime =
    case motor.drivingGearId of
        Nothing ->
            Dict.empty

        Just motorId ->
            let
                motorAngle =
                    motor.speedRadPerSec * totalTime
            in
            Physics.propagate graph motorId motorAngle
