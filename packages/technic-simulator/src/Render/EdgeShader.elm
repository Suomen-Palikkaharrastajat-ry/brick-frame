module Render.EdgeShader exposing (EdgeVertex, Uniforms, fragmentShader, vertexShader)

{-| Minimal shader for LDraw edge lines (type-2 line segments).

Renders all edges in a single dark colour — no lighting calculation needed.
WebGL 1.0 line width is fixed at 1 px on most hardware; accept this limitation.

-}

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import WebGL exposing (Shader)


{-| A vertex carrying only a position — no colour or normal needed.
-}
type alias EdgeVertex =
    { position : Vec3
    }


{-| MVP matrices passed to the vertex shader.
-}
type alias Uniforms =
    { modelMatrix : Mat4
    , viewMatrix : Mat4
    , projectionMatrix : Mat4
    }


type alias Varyings =
    {}


{-| Edge vertex shader — just MVP transform.
-}
vertexShader : Shader EdgeVertex Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec3 position;

        uniform mat4 modelMatrix;
        uniform mat4 viewMatrix;
        uniform mat4 projectionMatrix;

        void main() {
            gl_Position = projectionMatrix * viewMatrix * modelMatrix * vec4(position, 1.0);
        }
    |]


{-| Edge fragment shader — fixed dark colour.
-}
fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;

        void main() {
            gl_FragColor = vec4(0.08, 0.08, 0.08, 1.0);
        }
    |]
