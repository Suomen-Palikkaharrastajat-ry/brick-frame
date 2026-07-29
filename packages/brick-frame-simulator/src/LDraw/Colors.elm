module LDraw.Colors exposing (resolveColor, toVec4)

{-| LDraw color resolution.

LDraw has two special color codes, each with a BrickLink Studio equivalent:

  - **16** (Studio **-1**) — "current color", inherit the parent's color.
  - **24** (Studio **-2**) — "edge color", used for edge lines; rendered black.

All other codes are looked up in `LDraw.ColorData`, generated from the official
`LDConfig.ldr`. Unknown codes fall back to magenta so they are visually obvious
in the rendered scene.

Reference: <https://www.ldraw.org/article/547.html>

-}

import Dict
import LDraw.ColorData
import Math.Vector4 as Vec4 exposing (Vec4)



-- ── Public API ────────────────────────────────────────────────────────────────


{-| Resolve a LDraw color code to an RGBA value.

    resolveColor parentColor thisColor

  - `parentColor` is the color inherited from the enclosing sub-file reference.
    Used when `thisColor == 16`.
  - `thisColor` is the color declared on this line.

-}
resolveColor :
    Int
    -> Int
    -> { r : Float, g : Float, b : Float, alpha : Float }
resolveColor parentColor thisColor =
    if thisColor == 16 || thisColor == -1 then
        lookupColor parentColor

    else if thisColor == 24 || thisColor == -2 then
        { r = 0.0, g = 0.0, b = 0.0, alpha = 1.0 }

    else
        lookupColor (normalizeColorCode thisColor)


{-| Convert a color record to a `Vec4` for WebGL shaders.
-}
toVec4 : { r : Float, g : Float, b : Float, alpha : Float } -> Vec4
toVec4 c =
    Vec4.vec4 c.r c.g c.b c.alpha



-- ── Internal ──────────────────────────────────────────────────────────────────


{-| BrickLink Studio encodes certain colors as 100000 + ldrawCode in its
exported .ldr files. Strip the offset so lookups hit the standard table.
-}
normalizeColorCode : Int -> Int
normalizeColorCode code =
    if code >= 100000 then
        code - 100000

    else
        code


lookupColor : Int -> { r : Float, g : Float, b : Float, alpha : Float }
lookupColor code =
    Dict.get code LDraw.ColorData.colorTable
        |> Maybe.withDefault { r = 1.0, g = 0.0, b = 1.0, alpha = 1.0 }
