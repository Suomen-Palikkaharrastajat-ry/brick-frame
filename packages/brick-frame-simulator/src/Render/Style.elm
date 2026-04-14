module Render.Style exposing
    ( Style
    , clampStyle
    , clampVibrance
    , defaultStyle
    , fromLight
    )

{-| Rendering style controls for lightweight LEGO-like material tuning.

This keeps the pipeline cheap (single forward pass), while exposing a small
set of parameters for scene lighting, subtle plastic highlights, and edge tint.

-}

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Render.Lighting exposing (LightUniforms)


{-| Configurable material + edge look parameters used by render shaders.

Fields:

  - **`lightDirection`** — World-space vector pointing _toward_ the light
    source. Only the direction matters; magnitude is ignored (normalised
    automatically by `clampStyle`). Affects diffuse shading and specular
    highlights. Default: slightly above-right-front `(0.25, 1.0, 0.35)`.

  - **`ambientStrength`** — Minimum brightness applied to every face
    regardless of its orientation (`0`–`1`). Acts as the base floor of the
    light multiplier: `ambientStrength + lightStrength × diffuse`. `1.0`
    makes all faces equally bright (flat rendering); `0.0` lets faces
    pointing away from the light go completely black.

  - **`lightStrength`** — Intensity of the directional light contribution
    (`0`–`1`). Added on top of ambient: the final light multiplier is
    `ambientStrength + lightStrength × diffuse`. `0.0` disables directional
    light entirely (pure flat rendering when `ambientStrength = 1.0`); `0.3`
    adds gentle shading that reads depth without darkening shadows too much.

  - **`specularStrength`** — Intensity of the Blinn-Phong specular
    highlight (`0`–`1`). `0.0` disables specular entirely. Keep low (≤ 0.2)
    for a matte plastic look; raise toward `0.5` for a shiny surface.

  - **`specularPower`** — Sharpness of the specular highlight (`1`–`64`).
    Low values (e.g. `4`) give a broad, soft gloss; high values (e.g. `32`)
    give a tight, hard reflection. Only relevant when `specularStrength > 0`.

  - **`rimStrength`** — Intensity of the rim-light halo applied to silhouette
    edges (`0`–`1`). Rim light brightens faces whose normal is nearly
    perpendicular to the view direction, adding a subtle glow around the
    model outline. `0.0` disables rim lighting.

  - **`rimPower`** — Fall-off sharpness of the rim light (`0.1`–`8`). Lower
    values spread the rim effect over a wider band; higher values restrict it
    to a thin edge highlight. Only relevant when `rimStrength > 0`.

  - **`vibrance`** — Post-lighting saturation boost/cut (`−0.5`–`0.5`).
    Positive values pull colours toward their saturated hue (useful when
    ambient/diffuse multipliers wash out the original colour). Negative values
    desaturate. `0.0` leaves colours unchanged.

  - **`edgeColor`** — RGB colour of LDraw type-2 edge lines and conditional
    lines. Clamped to `[0, 1]` per channel. Dark values (near black) give
    crisp LEGO-style outlines; matching the background colour makes edges
    invisible.

  - **`edgeWidth`** — Pixel width of edge lines (`0.5`–`8.0`). Because WebGL
    1.0 fixes `gl.lineWidth` at 1 px on most hardware, edges are rendered as
    screen-aligned quads, so any width in the clamped range is honoured.
    `1.5` gives a clean, slightly sub-antialiased outline; `2.0`–`3.0` suits
    larger viewports or stylised renders.

-}
type alias Style =
    { lightDirection : Vec3
    , ambientStrength : Float
    , lightStrength : Float
    , specularStrength : Float
    , specularPower : Float
    , rimStrength : Float
    , rimPower : Float
    , vibrance : Float
    , edgeColor : Vec3
    , edgeWidth : Float
    }


{-| A balanced light-background default tuned for LEGO-style readability.
-}
defaultStyle : Style
defaultStyle =
    { lightDirection = Vec3.normalize (vec3 0.25 1.0 0.35)
    , lightStrength = 0.0
    , ambientStrength = 1.0
    , specularStrength = 0.0
    , specularPower = 18.0
    , rimStrength = 0.0
    , rimPower = 2.2
    , vibrance = 0.25
    , edgeColor = vec3 0.18 0.19 0.2
    , edgeWidth = 1.5
    }


{-| Build a full `Style` from legacy light uniforms.

All non-light fields inherit from `defaultStyle`.

-}
fromLight : LightUniforms -> Style
fromLight light =
    clampStyle
        { defaultStyle
            | lightDirection = normalizedDirection light.lightDirection
            , ambientStrength = light.ambientStrength
        }


{-| Clamp all style values to shader-safe bounds.
-}
clampStyle : Style -> Style
clampStyle style =
    { style
        | lightDirection = normalizedDirection style.lightDirection
        , ambientStrength = clamp 0 1 style.ambientStrength
        , lightStrength = clamp 0 1 style.lightStrength
        , specularStrength = clamp 0 1 style.specularStrength
        , specularPower = clamp 1 64 style.specularPower
        , rimStrength = clamp 0 1 style.rimStrength
        , rimPower = clamp 0.1 8 style.rimPower
        , vibrance = clampVibrance style.vibrance
        , edgeColor = clampVec3 style.edgeColor
        , edgeWidth = clamp 0.5 8.0 style.edgeWidth
    }


{-| Clamp vibrance amount to a conservative range.
-}
clampVibrance : Float -> Float
clampVibrance amount =
    clamp -0.5 0.5 amount


clampVec3 : Vec3 -> Vec3
clampVec3 color =
    vec3
        (clamp 0 1 (Vec3.getX color))
        (clamp 0 1 (Vec3.getY color))
        (clamp 0 1 (Vec3.getZ color))


normalizedDirection : Vec3 -> Vec3
normalizedDirection raw =
    if Vec3.length raw < 1.0e-6 then
        defaultStyle.lightDirection

    else
        Vec3.normalize raw
