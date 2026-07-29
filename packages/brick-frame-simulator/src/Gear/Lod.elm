module Gear.Lod exposing (Lod(..), chooseLod, detailDistance, simplifyDistance)

{-| Distance-based level-of-detail selection for gear meshes.

A gear can be drawn at `Full` detail or with a `Simplified` (decimated) mesh.
The choice is a pure function of the gear's distance from the camera and its
pitch radius, with **hysteresis**: the switch-to-simplified distance sits beyond
the switch-back-to-full distance, so a gear hovering near the boundary does not
flicker between the two meshes frame to frame.

Thresholds scale with pitch radius so a large gear stays detailed further out
than a small one. The constants are in LDU and tuned by eye — adjust
`detailDistance` / `simplifyDistance` if the switch is too eager or too lazy.

@docs Lod, chooseLod, detailDistance, simplifyDistance

-}


{-| The two levels of detail a gear can be rendered at.
-}
type Lod
    = Full
    | Simplified


{-| Distance (LDU) at or within which a gear is rendered at full detail. A gear
currently `Simplified` switches back to `Full` once it is closer than this.
-}
detailDistance : Float -> Float
detailDistance pitchRadius =
    25.0 * pitchRadius


{-| Distance (LDU) beyond which a gear is rendered simplified. A gear currently
`Full` switches to `Simplified` once it is farther than this. Always greater
than `detailDistance`, giving the hysteresis band.
-}
simplifyDistance : Float -> Float
simplifyDistance pitchRadius =
    40.0 * pitchRadius


{-| Choose the level of detail for a gear given its current level, its distance
from the camera, and its pitch radius.

    chooseLod currentLod distanceLdu pitchRadiusLdu

-}
chooseLod : Lod -> Float -> Float -> Lod
chooseLod current distance pitchRadius =
    case current of
        Full ->
            if distance > simplifyDistance pitchRadius then
                Simplified

            else
                Full

        Simplified ->
            if distance < detailDistance pitchRadius then
                Full

            else
                Simplified
