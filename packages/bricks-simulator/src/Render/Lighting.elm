module Render.Lighting exposing (LightUniforms, defaultLight)

{-| Directional light uniforms for the Lambert shader.

LEGO plastic is matte, so a simple Lambert model with a single directional
light and an ambient floor is sufficient and fast.

-}

import Math.Vector3 as Vec3 exposing (Vec3, vec3)


{-| Uniforms consumed by the Lambert fragment shader.
-}
type alias LightUniforms =
    { lightDirection : Vec3
    , ambientStrength : Float
    }


{-| Default light: a directional source coming from slightly above and to the
right, giving a gentle top-right highlight on most LEGO geometry.

  - `lightDirection` — world-space direction *toward* the light (not away).
    Normalised before use in the shader via `normalize(lightDirection)`.
  - `ambientStrength = 0.55` — brighter matte fill for more LEGO-like color
    readability and less harsh contrast.

-}
defaultLight : LightUniforms
defaultLight =
    { lightDirection = Vec3.normalize (vec3 0.25 1.0 0.35)
    , ambientStrength = 0.55
    }
