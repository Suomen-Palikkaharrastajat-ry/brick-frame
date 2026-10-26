module Studio.Shader exposing (Uniforms, Vertex, fragmentShader, vertexShader)

{-| A flat-colour shader for the studio's overlays (selection outlines and the
gizmo). Positions are already in render space, so there is no model matrix.

`color` is declared only in the fragment shader. A uniform declared in both
stages must have the same precision, and the vertex stage defaults to `highp`
while the fragment stage here is `mediump`; Safari (iOS especially) refuses to
link such a pair ("Uniform `color` is not linkable between attached shaders").

-}

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import WebGL exposing (Shader)


type alias Vertex =
    { position : Vec3 }


type alias Uniforms =
    { viewMatrix : Mat4
    , projectionMatrix : Mat4
    , color : Vec4
    }


vertexShader : Shader Vertex Uniforms {}
vertexShader =
    [glsl|
        attribute vec3 position;

        uniform mat4 viewMatrix;
        uniform mat4 projectionMatrix;

        void main() {
            gl_Position = projectionMatrix * viewMatrix * vec4(position, 1.0);
        }
    |]


fragmentShader : Shader {} Uniforms {}
fragmentShader =
    [glsl|
        precision mediump float;

        uniform vec4 color;

        void main() {
            gl_FragColor = color;
        }
    |]
