module Viewport.FramingTest exposing (suite)

{-| Auto-fit framing: the distance fits the narrower field-of-view half-angle,
the target is the bounds centre, and a canvas resize re-fits until the user
moves the camera.
-}

import Expect
import LDraw.Resolve as Resolve
import Math.Vector3 as Vec3
import Render.Camera as Camera
import Render.Style as Style
import Test exposing (Test, describe, test)
import Viewport exposing (CameraMode(..), Msg(..))
import Viewport.Framing as Framing exposing (ModelBounds)


config : Viewport.Config
config =
    { ldrawBase = ""
    , ldrawFallbackBase = ""
    , defaultModelUrl = ""
    , initialHash = ""
    , supportsPointerEvents = True
    , useWindowResize = False
    , renderStyle = Style.defaultStyle
    , partCache = Resolve.initCache
    }


idle : Viewport.Model
idle =
    Viewport.init config |> Tuple.first


{-| A cottage-sized model, centred away from the origin so the target can be
checked against it.
-}
cottage : ModelBounds
cottage =
    let
        center =
            Vec3.vec3 30 12 -8

        radius =
            36
    in
    { minCorner = Vec3.vec3 (30 - 40) (12 - 30) (-8 - 40)
    , maxCorner = Vec3.vec3 (30 + 40) (12 + 30) (-8 + 40)
    , center = center
    , radius = radius
    }


{-| Half-angle of the horizontal field of view for a canvas of this size.
-}
halfFovX : Int -> Int -> Float
halfFovX width height =
    let
        aspect =
            toFloat width / toFloat height
    in
    atan (tan (degrees Camera.fovYDegrees / 2) * aspect)


suite : Test
suite =
    describe "Viewport.Framing auto-fit"
        [ describe "cameraForBounds"
            [ test "a portrait phone frames the model from further back than 16:9" <|
                \_ ->
                    let
                        portrait =
                            Framing.cameraForBounds 390 844 cottage.center cottage.radius Camera.init

                        wide =
                            Framing.cameraForBounds 1280 720 cottage.center cottage.radius Camera.init
                    in
                    Expect.greaterThan wide.distance portrait.distance
            , test "the target is the bounds centre" <|
                \_ ->
                    let
                        camera =
                            Framing.cameraForBounds 390 844 cottage.center cottage.radius Camera.init
                    in
                    Expect.all
                        [ \c -> Expect.within (Expect.Absolute 1.0e-6) 30 (Vec3.getX c.target)
                        , \c -> Expect.within (Expect.Absolute 1.0e-6) 12 (Vec3.getY c.target)
                        , \c -> Expect.within (Expect.Absolute 1.0e-6) -8 (Vec3.getZ c.target)
                        ]
                        camera
            , test "the sphere fits both half-angles on a portrait canvas" <|
                \_ ->
                    let
                        camera =
                            Framing.cameraForBounds 390 844 cottage.center cottage.radius Camera.init

                        halfV =
                            degrees Camera.fovYDegrees / 2

                        narrower =
                            min (halfFovX 390 844) halfV
                    in
                    -- The sphere subtends less than the narrower half-angle.
                    Expect.lessThan (sin narrower) (cottage.radius / camera.distance)
            , test "the sphere fits both half-angles on a landscape canvas" <|
                \_ ->
                    let
                        camera =
                            Framing.cameraForBounds 1280 900 cottage.center cottage.radius Camera.init

                        narrower =
                            min (halfFovX 1280 900) (degrees Camera.fovYDegrees / 2)
                    in
                    Expect.lessThan (sin narrower) (cottage.radius / camera.distance)
            ]
        , describe "canvas resize"
            [ test "a resize before any manual move re-fits to the new aspect ratio" <|
                \_ ->
                    let
                        measured =
                            { idle | modelBounds = Just cottage, width = 800, height = 600 }

                        ( resized, _, _ ) =
                            Viewport.update (WindowResize 390 844) measured

                        expected =
                            Framing.cameraForBounds 390 844 cottage.center cottage.radius measured.camera
                    in
                    Expect.all
                        [ \m -> Expect.equal CameraAutoFit m.cameraMode
                        , \m -> Expect.within (Expect.Absolute 1.0e-6) expected.distance m.camera.distance
                        , \m -> Expect.within (Expect.Absolute 1.0e-6) 30 (Vec3.getX m.camera.target)
                        ]
                        resized
            , test "a resize after a manual move keeps the user's view" <|
                \_ ->
                    let
                        measured =
                            { idle | modelBounds = Just cottage, width = 800, height = 600 }

                        ( moved, _, _ ) =
                            Viewport.update (Wheel 100) measured

                        ( resized, _, _ ) =
                            Viewport.update (WindowResize 390 844) moved
                    in
                    Expect.all
                        [ \m -> Expect.equal CameraManual m.cameraMode
                        , \m -> Expect.within (Expect.Absolute 1.0e-6) moved.camera.distance m.camera.distance
                        , \m -> Expect.within (Expect.Absolute 1.0e-6) moved.camera.azimuth m.camera.azimuth
                        , \m -> Expect.within (Expect.Absolute 1.0e-6) (Vec3.getX moved.camera.target) (Vec3.getX m.camera.target)
                        ]
                        resized
            , test "a resize to the same size keeps a camera that was just restored" <|
                \_ ->
                    let
                        measured =
                            { idle | modelBounds = Just cottage, width = 800, height = 600 }

                        camera =
                            measured.camera

                        restored =
                            { measured | camera = { camera | distance = 123 } }

                        ( resized, _, _ ) =
                            Viewport.update (WindowResize 800 600) restored
                    in
                    Expect.within (Expect.Absolute 1.0e-6) 123 resized.camera.distance
            , test "a resize before any model is measured leaves the camera alone" <|
                \_ ->
                    let
                        ( resized, _, _ ) =
                            Viewport.update (WindowResize 390 844) idle
                    in
                    Expect.equal idle.camera.distance resized.camera.distance
            ]
        ]
