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
-}
type alias Style =
    { lightDirection : Vec3
    , ambientStrength : Float
    , specularStrength : Float
    , specularPower : Float
    , rimStrength : Float
    , rimPower : Float
    , vibrance : Float
    , edgeColor : Vec3
    }


{-| A balanced light-background default tuned for LEGO-style readability.
-}
defaultStyle : Style
defaultStyle =
    { lightDirection = Vec3.normalize (vec3 0.25 1.0 0.35)
    , ambientStrength = 0.55
    , specularStrength = 0.14
    , specularPower = 18.0
    , rimStrength = 0.08
    , rimPower = 2.2
    , vibrance = 0.12
    , edgeColor = vec3 0.18 0.19 0.2
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
        , specularStrength = clamp 0 1 style.specularStrength
        , specularPower = clamp 1 64 style.specularPower
        , rimStrength = clamp 0 1 style.rimStrength
        , rimPower = clamp 0.1 8 style.rimPower
        , vibrance = clampVibrance style.vibrance
        , edgeColor = clampVec3 style.edgeColor
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
