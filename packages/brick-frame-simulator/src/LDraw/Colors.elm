module LDraw.Colors exposing (resolveColor, toVec4)

{-| LDraw color resolution.

LDraw has two special color codes:

  - **16** ("current color") — inherit the parent's color.
  - **24** ("edge color") — used for edge lines; rendered black.

All other codes are looked up in the embedded color table. Unknown codes fall
back to magenta so they are visually obvious in the rendered scene.

Reference: <https://www.ldraw.org/article/547.html>

-}

import Dict exposing (Dict)
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

    else if thisColor == 24 then
        { r = 0.0, g = 0.0, b = 0.0, alpha = 1.0 }

    else
        lookupColor thisColor


{-| Convert a color record to a `Vec4` for WebGL shaders.
-}
toVec4 : { r : Float, g : Float, b : Float, alpha : Float } -> Vec4
toVec4 c =
    Vec4.vec4 c.r c.g c.b c.alpha



-- ── Internal ──────────────────────────────────────────────────────────────────


lookupColor : Int -> { r : Float, g : Float, b : Float, alpha : Float }
lookupColor code =
    Dict.get code colorTable
        |> Maybe.withDefault { r = 1.0, g = 0.0, b = 1.0, alpha = 1.0 }


{-| Standard LDraw color table embedded in the library.
Values are linear RGB in [0.0, 1.0].
-}
colorTable : Dict Int { r : Float, g : Float, b : Float, alpha : Float }
colorTable =
    Dict.fromList
        [ ( 0, { r = 0.067, g = 0.067, b = 0.067, alpha = 1.0 } ) -- Black
        , ( 1, { r = 0.0, g = 0.333, b = 0.749, alpha = 1.0 } ) -- Blue
        , ( 2, { r = 0.145, g = 0.478, b = 0.243, alpha = 1.0 } ) -- Green
        , ( 3, { r = 0.0, g = 0.514, b = 0.561, alpha = 1.0 } ) -- Dark Turquoise
        , ( 4, { r = 0.788, g = 0.102, b = 0.035, alpha = 1.0 } ) -- Red
        , ( 5, { r = 0.784, g = 0.184, b = 0.439, alpha = 1.0 } ) -- Dark Pink
        , ( 6, { r = 0.357, g = 0.212, b = 0.067, alpha = 1.0 } ) -- Brown
        , ( 7, { r = 0.608, g = 0.631, b = 0.608, alpha = 1.0 } ) -- Light Grey
        , ( 8, { r = 0.392, g = 0.373, b = 0.353, alpha = 1.0 } ) -- Dark Grey
        , ( 9, { r = 0.682, g = 0.831, b = 0.933, alpha = 1.0 } ) -- Light Blue
        , ( 10, { r = 0.294, g = 0.78, b = 0.231, alpha = 1.0 } ) -- Bright Green
        , ( 11, { r = 0.0, g = 0.659, b = 0.682, alpha = 1.0 } ) -- Light Turquoise
        , ( 12, { r = 0.988, g = 0.565, b = 0.478, alpha = 1.0 } ) -- Salmon
        , ( 13, { r = 0.988, g = 0.671, b = 0.749, alpha = 1.0 } ) -- Pink
        , ( 14, { r = 0.980, g = 0.784, b = 0.039, alpha = 1.0 } ) -- Yellow
        , ( 15, { r = 1.0, g = 1.0, b = 1.0, alpha = 1.0 } ) -- White
        , ( 17, { r = 0.71, g = 0.902, b = 0.71, alpha = 1.0 } ) -- Light Green
        , ( 18, { r = 0.988, g = 0.929, b = 0.624, alpha = 1.0 } ) -- Light Yellow
        , ( 19, { r = 0.902, g = 0.835, b = 0.612, alpha = 1.0 } ) -- Tan
        , ( 20, { r = 0.812, g = 0.729, b = 0.878, alpha = 1.0 } ) -- Light Violet
        , ( 22, { r = 0.373, g = 0.082, b = 0.49, alpha = 1.0 } ) -- Purple
        , ( 23, { r = 0.122, g = 0.141, b = 0.62, alpha = 1.0 } ) -- Dark Blue Violet
        , ( 25, { r = 0.988, g = 0.502, b = 0.122, alpha = 1.0 } ) -- Orange
        , ( 26, { r = 0.627, g = 0.0, b = 0.502, alpha = 1.0 } ) -- Magenta
        , ( 27, { r = 0.749, g = 0.878, b = 0.118, alpha = 1.0 } ) -- Lime
        , ( 28, { r = 0.639, g = 0.537, b = 0.337, alpha = 1.0 } ) -- Dark Tan
        , ( 29, { r = 0.988, g = 0.671, b = 0.78, alpha = 1.0 } ) -- Bright Pink
        , ( 30, { r = 0.667, g = 0.525, b = 0.718, alpha = 1.0 } ) -- Medium Lavender
        , ( 31, { r = 0.792, g = 0.714, b = 0.847, alpha = 1.0 } ) -- Lavender
        , ( 36, { r = 0.988, g = 0.769, b = 0.518, alpha = 1.0 } ) -- Very Light Orange
        , ( 38, { r = 0.651, g = 0.251, b = 0.047, alpha = 1.0 } ) -- Dark Orange
        , ( 40, { r = 0.243, g = 0.243, b = 0.243, alpha = 0.5 } ) -- Trans Black
        , ( 41, { r = 0.902, g = 0.071, b = 0.008, alpha = 0.5 } ) -- Trans Red
        , ( 42, { r = 0.773, g = 0.988, b = 0.0, alpha = 0.5 } ) -- Trans Neon Green
        , ( 43, { r = 0.537, g = 0.851, b = 0.988, alpha = 0.5 } ) -- Trans Light Blue
        , ( 44, { r = 0.682, g = 0.525, b = 0.741, alpha = 0.5 } ) -- Trans Light Purple
        , ( 45, { r = 0.878, g = 0.4, b = 0.573, alpha = 0.5 } ) -- Trans Dark Pink
        , ( 46, { r = 0.988, g = 0.855, b = 0.165, alpha = 0.5 } ) -- Trans Yellow
        , ( 47, { r = 0.878, g = 0.878, b = 0.878, alpha = 0.5 } ) -- Trans White
        , ( 57, { r = 0.988, g = 0.502, b = 0.122, alpha = 0.5 } ) -- Trans Orange
        , ( 68, { r = 0.988, g = 0.769, b = 0.518, alpha = 1.0 } ) -- Very Light Orange
        , ( 69, { r = 0.584, g = 0.106, b = 0.624, alpha = 1.0 } ) -- Bright Purple
        , ( 70, { r = 0.408, g = 0.188, b = 0.067, alpha = 1.0 } ) -- Reddish Brown
        , ( 71, { r = 0.694, g = 0.722, b = 0.749, alpha = 1.0 } ) -- Light Bluish Grey
        , ( 72, { r = 0.4, g = 0.427, b = 0.451, alpha = 1.0 } ) -- Dark Bluish Grey
        , ( 73, { r = 0.486, g = 0.631, b = 0.808, alpha = 1.0 } ) -- Medium Blue
        , ( 74, { r = 0.467, g = 0.733, b = 0.396, alpha = 1.0 } ) -- Medium Green
        , ( 77, { r = 0.988, g = 0.671, b = 0.749, alpha = 1.0 } ) -- Pink
        , ( 78, { r = 0.988, g = 0.847, b = 0.663, alpha = 1.0 } ) -- Light Flesh
        , ( 84, { r = 0.71, g = 0.396, b = 0.184, alpha = 1.0 } ) -- Medium Dark Flesh
        , ( 85, { r = 0.243, g = 0.09, b = 0.294, alpha = 1.0 } ) -- Dark Purple
        , ( 86, { r = 0.58, g = 0.286, b = 0.114, alpha = 1.0 } ) -- Dark Flesh
        , ( 89, { r = 0.251, g = 0.341, b = 0.659, alpha = 1.0 } ) -- Blue Violet
        , ( 92, { r = 0.851, g = 0.549, b = 0.302, alpha = 1.0 } ) -- Flesh
        , ( 100, { r = 0.988, g = 0.729, b = 0.639, alpha = 1.0 } ) -- Light Salmon
        , ( 110, { r = 0.247, g = 0.247, b = 0.6, alpha = 1.0 } ) -- Violet
        , ( 112, { r = 0.412, g = 0.412, b = 0.682, alpha = 1.0 } ) -- Medium Violet
        , ( 115, { r = 0.624, g = 0.765, b = 0.176, alpha = 1.0 } ) -- Medium Lime
        , ( 118, { r = 0.667, g = 0.902, b = 0.843, alpha = 1.0 } ) -- Aqua
        , ( 120, { r = 0.812, g = 0.902, b = 0.494, alpha = 1.0 } ) -- Light Lime
        , ( 125, { r = 0.988, g = 0.671, b = 0.369, alpha = 1.0 } ) -- Light Orange
        , ( 128, { r = 0.651, g = 0.251, b = 0.047, alpha = 1.0 } ) -- Dark Orange
        , ( 151, { r = 0.859, g = 0.875, b = 0.878, alpha = 1.0 } ) -- Very Light Bluish Grey
        , ( 191, { r = 0.988, g = 0.671, b = 0.047, alpha = 1.0 } ) -- Bright Light Orange
        , ( 212, { r = 0.624, g = 0.812, b = 0.988, alpha = 1.0 } ) -- Bright Light Blue
        , ( 216, { r = 0.671, g = 0.165, b = 0.082, alpha = 1.0 } ) -- Rust
        , ( 226, { r = 0.988, g = 0.929, b = 0.447, alpha = 1.0 } ) -- Bright Light Yellow
        , ( 232, { r = 0.533, g = 0.753, b = 0.878, alpha = 1.0 } ) -- Sky Blue
        , ( 256, { r = 0.067, g = 0.067, b = 0.067, alpha = 1.0 } ) -- Rubber Black
        , ( 272, { r = 0.0, g = 0.173, b = 0.475, alpha = 1.0 } ) -- Dark Blue
        , ( 288, { r = 0.055, g = 0.267, b = 0.09, alpha = 1.0 } ) -- Dark Green
        , ( 308, { r = 0.188, g = 0.114, b = 0.055, alpha = 1.0 } ) -- Dark Brown
        , ( 320, { r = 0.486, g = 0.0, b = 0.027, alpha = 1.0 } ) -- Dark Red
        , ( 321, { r = 0.0, g = 0.549, b = 0.773, alpha = 1.0 } ) -- Dark Azure
        , ( 322, { r = 0.341, g = 0.71, b = 0.867, alpha = 1.0 } ) -- Medium Azure
        , ( 323, { r = 0.788, g = 0.945, b = 0.886, alpha = 1.0 } ) -- Light Aqua
        , ( 326, { r = 0.835, g = 0.929, b = 0.576, alpha = 1.0 } ) -- Yellowish Green
        , ( 329, { r = 1.0, g = 1.0, b = 0.902, alpha = 1.0 } ) -- White Glow
        , ( 334, { r = 0.769, g = 0.608, b = 0.224, alpha = 1.0 } ) -- Pearl Gold
        , ( 335, { r = 0.694, g = 0.49, b = 0.459, alpha = 1.0 } ) -- Sand Red
        , ( 366, { r = 0.671, g = 0.376, b = 0.071, alpha = 1.0 } ) -- Earth Orange
        , ( 373, { r = 0.58, g = 0.486, b = 0.592, alpha = 1.0 } ) -- Sand Purple
        , ( 378, { r = 0.482, g = 0.608, b = 0.518, alpha = 1.0 } ) -- Sand Green
        , ( 379, { r = 0.427, g = 0.518, b = 0.616, alpha = 1.0 } ) -- Sand Blue
        , ( 383, { r = 0.878, g = 0.878, b = 0.878, alpha = 1.0 } ) -- Chrome Silver
        , ( 462, { r = 0.988, g = 0.671, b = 0.369, alpha = 1.0 } ) -- Light Orange
        , ( 484, { r = 0.651, g = 0.251, b = 0.047, alpha = 1.0 } ) -- Dark Orange
        , ( 503, { r = 0.878, g = 0.878, b = 0.867, alpha = 1.0 } ) -- Very Light Grey
        ]
