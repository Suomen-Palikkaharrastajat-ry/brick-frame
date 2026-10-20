module Render.Shader exposing (Uniforms, fragmentShader, noInstanceColor, vertexShader)

{-| Lightweight plastic shading GLSL programs for triangle mesh rendering.
-}

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import Render.Mesh exposing (Vertex)
import WebGL exposing (Shader)


{-| Uniforms shared between vertex and fragment shaders.

`instanceColor` is the colour substituted into vertices flagged as inheriting
(see `Render.Mesh.Vertex`). Paths that bake colours into the geometry pass
white here and are unaffected, because no vertex they produce is flagged.

-}
type alias Uniforms =
    { modelMatrix : Mat4
    , viewMatrix : Mat4
    , projectionMatrix : Mat4
    , viewPosition : Vec3
    , lightDirection : Vec3
    , instanceColor : Vec4
    , ambientStrength : Float
    , lightStrength : Float
    , specularStrength : Float
    , specularPower : Float
    , rimStrength : Float
    , rimPower : Float
    , vibrance : Float
    , bevelStrength : Float
    , studEmbossStrength : Float
    }


{-| The colour to pass as `instanceColor` when geometry already carries its own
resolved colours.
-}
noInstanceColor : Vec4
noInstanceColor =
    Vec4.vec4 1 1 1 1


type alias Varyings =
    { vColor : Vec4
    , vNormalWorld : Vec3
    , vPositionWorld : Vec3
    , vPositionLocal : Vec3
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
        attribute float inherit;

        uniform mat4 modelMatrix;
        uniform mat4 viewMatrix;
        uniform mat4 projectionMatrix;
        uniform vec4 instanceColor;

        varying vec4 vColor;
        varying vec3 vNormalWorld;
        varying vec3 vPositionWorld;
        varying vec3 vPositionLocal;

        void main() {
            vec4 worldPosition = modelMatrix * vec4(position, 1.0);
            gl_Position = projectionMatrix * viewMatrix * worldPosition;
            // inherit is 1.0 only on surfaces that took LDraw colour 16 all the
            // way down, which is what lets one baked mesh serve every colour a
            // part is placed in.
            vColor = mix(color, instanceColor, inherit);
            // Transform normal to world space (assumes uniform scale)
            vNormalWorld = mat3(modelMatrix) * normal;
            vPositionWorld = worldPosition.xyz;
            vPositionLocal = position;
        }
    |]


{-| Plastic fragment shader.
Computes diffuse + soft specular + subtle rim light, then applies a mild
vibrance adjustment, normal-modulated edge beveling and stud logo embossing.
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
        uniform float bevelStrength;
        uniform float studEmbossStrength;

        varying vec4 vColor;
        varying vec3 vNormalWorld;
        varying vec3 vPositionWorld;
        varying vec3 vPositionLocal;

        float segDist(vec2 p, vec2 a, vec2 b) {
            vec2 pa = p - a;
            vec2 ba = b - a;
            float h = clamp(dot(pa, ba) / dot(ba, ba), 0.0, 1.0);
            return length(pa - ba * h);
        }

        vec3 computeStudEmboss(vec3 pos, vec3 norm) {
            if (abs(norm.y) < 0.75) {
                return vec3(0.0);
            }
            vec2 gridCoord = mod(pos.xz + vec2(10.0), 20.0) - vec2(10.0);
            float distFromCenter = length(gridCoord);
            if (distFromCenter > 5.5) {
                return vec3(0.0);
            }

            vec2 uv = gridCoord / 4.8;
            float c = 0.9063;
            float s = -0.4226;
            vec2 ruv = vec2(uv.x * c - uv.y * s, uv.x * s + uv.y * c);

            float dL = min(segDist(ruv, vec2(-0.70, 0.28), vec2(-0.70, -0.28)),
                           segDist(ruv, vec2(-0.70, -0.28), vec2(-0.50, -0.28)));
            float dE = min(segDist(ruv, vec2(-0.36, 0.28), vec2(-0.36, -0.28)),
                           segDist(ruv, vec2(-0.36, 0.28), vec2(-0.18, 0.28)));
            dE = min(dE, min(segDist(ruv, vec2(-0.36, 0.0), vec2(-0.20, 0.0)),
                             segDist(ruv, vec2(-0.36, -0.28), vec2(-0.18, -0.28))));
            float dG = min(segDist(ruv, vec2(0.14, 0.28), vec2(-0.04, 0.28)),
                           segDist(ruv, vec2(-0.04, 0.28), vec2(-0.04, -0.28)));
            dG = min(dG, min(segDist(ruv, vec2(-0.04, -0.28), vec2(0.14, -0.28)),
                             segDist(ruv, vec2(0.14, -0.28), vec2(0.14, -0.05))));
            dG = min(dG, segDist(ruv, vec2(0.14, -0.05), vec2(0.05, -0.05)));
            float dO = abs(length(vec2((ruv.x - 0.46) * 1.25, ruv.y)) - 0.24);

            float dMin = min(min(dL, dE), min(dG, dO));
            float strokeWidth = 0.06;
            if (dMin < strokeWidth) {
                float factor = (strokeWidth - dMin) / strokeWidth;
                float grad = sin(factor * 3.14159);
                return vec3(ruv.x * grad * 0.4, 0.0, ruv.y * grad * 0.4);
            }
            return vec3(0.0);
        }

        vec3 computeEdgeBevel(vec3 pos, vec3 norm) {
            vec3 cellPos = mod(pos + vec3(10.0, 4.0, 10.0), vec3(20.0, 8.0, 20.0)) - vec3(10.0, 4.0, 10.0);
            vec3 distToFaceEdge = vec3(10.0, 4.0, 10.0) - abs(cellPos);
            float bevelRadius = 1.0;
            vec3 bevelDelta = vec3(0.0);

            if (distToFaceEdge.x < bevelRadius && abs(norm.x) < 0.5) {
                float f = (bevelRadius - distToFaceEdge.x) / bevelRadius;
                bevelDelta.x = sign(cellPos.x) * f * 0.35;
            }
            if (distToFaceEdge.y < bevelRadius && abs(norm.y) < 0.5) {
                float f = (bevelRadius - distToFaceEdge.y) / bevelRadius;
                bevelDelta.y = sign(cellPos.y) * f * 0.35;
            }
            if (distToFaceEdge.z < bevelRadius && abs(norm.z) < 0.5) {
                float f = (bevelRadius - distToFaceEdge.z) / bevelRadius;
                bevelDelta.z = sign(cellPos.z) * f * 0.35;
            }
            return bevelDelta;
        }

        void main() {
            vec3 baseNorm = normalize(vNormalWorld);
            vec3 beveled = baseNorm;
            if (bevelStrength > 0.001) {
                beveled = normalize(beveled + bevelStrength * computeEdgeBevel(vPositionLocal, baseNorm));
            }
            if (studEmbossStrength > 0.001) {
                beveled = normalize(beveled + studEmbossStrength * computeStudEmboss(vPositionLocal, baseNorm));
            }

            vec3 lightDir = normalize(lightDirection);
            vec3 viewDir = normalize(viewPosition - vPositionWorld);
            float diffuse = max(0.0, dot(beveled, lightDir));
            float light = ambientStrength + lightStrength * diffuse;

            float specular = 0.0;
            if (specularStrength > 0.0001) {
                vec3 halfDir = normalize(lightDir + viewDir);
                specular = pow(max(dot(beveled, halfDir), 0.0), specularPower) * specularStrength;
            }

            float rim = 0.0;
            if (rimStrength > 0.0001) {
                rim = pow(1.0 - max(dot(beveled, viewDir), 0.0), rimPower) * rimStrength;
            }

            vec3 litColor = vColor.rgb * light + vec3(specular + rim);
            if (abs(vibrance) > 0.0001) {
                float luma = dot(litColor, vec3(0.2126, 0.7152, 0.0722));
                litColor = mix(vec3(luma), litColor, 1.0 + clamp(vibrance, -0.5, 0.5));
            }

            gl_FragColor = vec4(clamp(litColor, 0.0, 1.0), vColor.a);
        }
    |]
