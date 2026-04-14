module Render.EdgeShader exposing (EdgeVertex, Uniforms, fragmentShader, vertexShader)

{-| Shader for LDraw edge lines (type-2 line segments and conditional lines).

Each line segment is rendered as a screen-aligned quad (two triangles) so that
`lineWidth` is honoured regardless of hardware. WebGL 1.0 fixes `gl.lineWidth`
at 1 px on most drivers; this approach works around that limitation.

The fragment shader applies a smooth alpha falloff from the quad centre to its
edges, producing antialiased strokes. Alpha blending must be enabled on the
entity for this to take effect (see `Render.Scene`).

-}

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import WebGL exposing (Shader)


{-| One corner of a screen-aligned edge quad.

  - `position` — the endpoint this vertex belongs to (world space).
  - `other` — the opposite endpoint of the segment (world space).
  - `side` — which side of the quad: `−1.0` or `+1.0`.

-}
type alias EdgeVertex =
    { position : Vec3
    , other : Vec3
    , side : Float
    }


{-| Uniforms for the edge shaders.
-}
type alias Uniforms =
    { modelMatrix : Mat4
    , viewMatrix : Mat4
    , projectionMatrix : Mat4
    , edgeColor : Vec3
    , viewportWidth : Float
    , viewportHeight : Float
    , lineWidth : Float
    }


type alias Varyings =
    { vSide : Float
    }


{-| Edge vertex shader.

Transforms both endpoints to clip space, computes the perpendicular direction
of the segment in NDC, then offsets this vertex by `lineWidth` pixels along
that perpendicular (scaled back to clip space by multiplying by `clip.w`).
Passes `side` (−1.0 / +1.0) to the fragment shader as `vSide`.

-}
vertexShader : Shader EdgeVertex Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 other;
        attribute float side;

        uniform mat4 modelMatrix;
        uniform mat4 viewMatrix;
        uniform mat4 projectionMatrix;
        uniform float viewportWidth;
        uniform float viewportHeight;
        uniform float lineWidth;

        varying float vSide;

        void main() {
            vec4 clip0 = projectionMatrix * viewMatrix * modelMatrix * vec4(position, 1.0);
            vec4 clip1 = projectionMatrix * viewMatrix * modelMatrix * vec4(other, 1.0);

            // Segment direction in NDC; guard against degenerate zero-length segments
            vec2 dir = clip1.xy / clip1.w - clip0.xy / clip0.w;
            float len = length(dir);
            if (len > 0.0001) {
                dir = dir / len;
            } else {
                dir = vec2(1.0, 0.0);
            }

            vec2 perp = vec2(-dir.y, dir.x);

            // Scale to pixel width; multiply by clip0.w to undo perspective divide
            // so the offset is constant in screen space regardless of depth.
            vec2 offset = perp * (lineWidth / vec2(viewportWidth, viewportHeight)) * clip0.w;

            gl_Position = clip0 + vec4(offset * side, 0.0, 0.0);
            vSide = side;
        }
    |]


{-| Edge fragment shader.

Applies a smooth alpha falloff from the quad centre (fully opaque) to the
quad boundaries (fully transparent), producing antialiased strokes. Requires
alpha blending to be enabled on the entity.

-}
fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 edgeColor;

        varying float vSide;

        void main() {
            // Fade alpha smoothly from 1.0 at centre (vSide = 0) to 0.0 at the
            // quad boundary (|vSide| = 1.0). The inner 50% stays fully opaque;
            // the outer 50% fades, giving a crisp antialiased stroke.
            float alpha = 1.0 - smoothstep(0.5, 1.0, abs(vSide));
            gl_FragColor = vec4(clamp(edgeColor, 0.0, 1.0), alpha);
        }
    |]
