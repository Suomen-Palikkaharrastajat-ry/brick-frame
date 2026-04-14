module Render.Shader exposing (Uniforms, fragmentShader, vertexShader)

{-| Lightweight plastic shading GLSL programs for triangle mesh rendering.
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
    , viewPosition : Vec3
    , lightDirection : Vec3
    , ambientStrength : Float
    , lightStrength : Float
    , specularStrength : Float
    , specularPower : Float
    , rimStrength : Float
    , rimPower : Float
    , vibrance : Float
    }


type alias Varyings =
    { vColor : Vec4
    , vNormalWorld : Vec3
    , vPositionWorld : Vec3
    }


{-| Plastic vertex shader.
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
        varying vec3 vPositionWorld;

        void main() {
            vec4 worldPosition = modelMatrix * vec4(position, 1.0);
            gl_Position = projectionMatrix * viewMatrix * worldPosition;
            vColor = color;
            // Transform normal to world space (assumes uniform scale)
            vNormalWorld = mat3(modelMatrix) * normal;
            vPositionWorld = worldPosition.xyz;
        }
    |]


{-| Plastic fragment shader.
Computes diffuse + soft specular + subtle rim light, then applies a mild
vibrance adjustment.
-}
fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;

        uniform vec3 lightDirection;
        uniform vec3 viewPosition;
        uniform float ambientStrength;
        uniform float lightStrength;
        uniform float specularStrength;
        uniform float specularPower;
        uniform float rimStrength;
        uniform float rimPower;
        uniform float vibrance;

        varying vec4 vColor;
        varying vec3 vNormalWorld;
        varying vec3 vPositionWorld;

        void main() {
            vec3 norm = normalize(vNormalWorld);
            vec3 lightDir = normalize(lightDirection);
            vec3 viewDir = normalize(viewPosition - vPositionWorld);
            float diffuse = max(0.0, dot(norm, lightDir));
            float light = ambientStrength + lightStrength * diffuse;

            float specular = 0.0;
            if (specularStrength > 0.0001) {
                vec3 halfDir = normalize(lightDir + viewDir);
                specular = pow(max(dot(norm, halfDir), 0.0), specularPower) * specularStrength;
            }

            float rim = 0.0;
            if (rimStrength > 0.0001) {
                rim = pow(1.0 - max(dot(norm, viewDir), 0.0), rimPower) * rimStrength;
            }

            vec3 litColor = vColor.rgb * light + vec3(specular + rim);
            if (abs(vibrance) > 0.0001) {
                float luma = dot(litColor, vec3(0.2126, 0.7152, 0.0722));
                litColor = mix(vec3(luma), litColor, 1.0 + clamp(vibrance, -0.5, 0.5));
            }

            gl_FragColor = vec4(clamp(litColor, 0.0, 1.0), vColor.a);
        }
    |]
