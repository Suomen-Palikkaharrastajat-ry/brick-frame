module Render.GuideShader exposing (Uniforms, fragmentShader, vertexShader)

{-| Shader for helper guide lines (e.g. axle rotation arrows).
-}

import Render.EdgeShader exposing (EdgeVertex)
import Math.Matrix4 exposing (Mat4)
import WebGL exposing (Shader)


type alias Uniforms =
    { modelMatrix : Mat4
    , viewMatrix : Mat4
    , projectionMatrix : Mat4
    }


type alias Varyings =
    {}


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


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;

        void main() {
            gl_FragColor = vec4(0.95, 0.73, 0.12, 1.0);
        }
    |]
