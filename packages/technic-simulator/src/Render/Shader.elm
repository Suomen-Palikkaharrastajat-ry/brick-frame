module Render.Shader exposing (Uniforms, fragmentShader, vertexShader)

{-| Lambert shading GLSL programs for triangle mesh rendering.
-}

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Render.Mesh exposing (Vertex)
import WebGL exposing (Shader)


{-| Uniforms shared between vertex and fragment shaders.
-}
type alias Uniforms =
    { modelMatrix : Mat4
    , viewMatrix : Mat4
    , projectionMatrix : Mat4
    , lightDirection : Vec3
    , ambientStrength : Float
    }


type alias Varyings =
    { vColor : Vec4
    , vNormalWorld : Vec3
    }


{-| Lambert vertex shader.
Transforms position through MVP matrices and passes normal (in world space)
and colour to the fragment shader.
-}
vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec4 color;

        uniform mat4 modelMatrix;
        uniform mat4 viewMatrix;
        uniform mat4 projectionMatrix;

        varying vec4 vColor;
        varying vec3 vNormalWorld;

        void main() {
            gl_Position = projectionMatrix * viewMatrix * modelMatrix * vec4(position, 1.0);
            vColor = color;
            // Transform normal to world space (assumes uniform scale)
            vNormalWorld = mat3(modelMatrix) * normal;
        }
    |]


{-| Lambert fragment shader.
Computes diffuse = max(0, dot(normal, light)) with ambient floor.
-}
fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;

        uniform vec3 lightDirection;
        uniform float ambientStrength;

        varying vec4 vColor;
        varying vec3 vNormalWorld;

        void main() {
            vec3 norm = normalize(vNormalWorld);
            float diffuse = max(0.0, dot(norm, normalize(lightDirection)));
            float light = ambientStrength + (1.0 - ambientStrength) * diffuse;
            gl_FragColor = vec4(vColor.rgb * light, vColor.a);
        }
    |]
