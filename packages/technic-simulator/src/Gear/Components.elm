module Gear.Components exposing
    ( ComponentInstance
    , ComponentKind(..)
    , ComponentSpec
    , defaultSpecs
    , extractComponents
    )

{-| Detect Technic non-gear drivetrain components (axles, beams) in a loaded
LDraw part tree.
-}

import Dict exposing (Dict)
import LDraw.Resolve exposing (PartCache, PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)


type ComponentKind
    = AxleLike
    | Beam


type alias ComponentSpec =
    { partFile : String
    , kind : ComponentKind
    }


type alias ComponentInstance =
    { kind : ComponentKind
    , partFile : String
    , worldPosition : Vec3
    , worldAxis : Vec3
    , worldMatrix : Mat4
    }


defaultSpecs : List ComponentSpec
defaultSpecs =
    [ -- Common cross-axles
      { partFile = "3705.dat", kind = AxleLike }
    , { partFile = "3706.dat", kind = AxleLike }
    , { partFile = "3707.dat", kind = AxleLike }
    , { partFile = "4519.dat", kind = AxleLike }
    , { partFile = "32209.dat", kind = AxleLike }
    , { partFile = "6587.dat", kind = AxleLike }
    , { partFile = "18654.dat", kind = AxleLike }
    , { partFile = "99009.dat", kind = AxleLike }
    , -- Common pins / pin-axle connectors (trackable as axle-like shafts)
      { partFile = "2780.dat", kind = AxleLike }
    , { partFile = "3673.dat", kind = AxleLike }
    , { partFile = "3749.dat", kind = AxleLike }
    , -- Common beams and liftarms
      { partFile = "32316.dat", kind = Beam }
    , { partFile = "32523.dat", kind = Beam }
    , { partFile = "32524.dat", kind = Beam }
    , { partFile = "32525.dat", kind = Beam }
    , { partFile = "32526.dat", kind = Beam }
    , { partFile = "32527.dat", kind = Beam }
    , { partFile = "32528.dat", kind = Beam }
    , { partFile = "3709b.dat", kind = Beam }
    , { partFile = "3894.dat", kind = Beam }
    , { partFile = "40490.dat", kind = Beam }
    ]


extractComponents : List ComponentSpec -> List LDrawLine -> PartCache -> List ComponentInstance
extractComponents specs lines cache =
    walkLines specs lines cache Mat4.identity []
        |> List.reverse


walkLines : List ComponentSpec -> List LDrawLine -> PartCache -> Mat4 -> List ComponentInstance -> List ComponentInstance
walkLines specs lines cache worldMat acc =
    List.foldl (walkLine specs cache worldMat) acc lines


walkLine : List ComponentSpec -> PartCache -> Mat4 -> LDrawLine -> List ComponentInstance -> List ComponentInstance
walkLine specs cache worldMat line acc =
    case line of
        SubFileRef { file, transform } ->
            let
                combinedMat =
                    Mat4.mul worldMat transform
            in
            case matchSpec specs file of
                Just spec ->
                    let
                        origin =
                            Mat4.transform combinedMat (Vec3.vec3 0 0 0)

                        axisEnd =
                            Mat4.transform combinedMat (Vec3.vec3 0 0 1)

                        rawAxis =
                            Vec3.sub axisEnd origin

                        axisLen =
                            Vec3.length rawAxis

                        axis =
                            if axisLen < 1.0e-6 then
                                Vec3.vec3 0 0 1

                            else
                                Vec3.scale (1 / axisLen) rawAxis
                    in
                    { kind = spec.kind
                    , partFile = spec.partFile
                    , worldPosition = origin
                    , worldAxis = axis
                    , worldMatrix = combinedMat
                    }
                        :: acc

                Nothing ->
                    case Dict.get file cache of
                        Just (Loaded subLines) ->
                            walkLines specs subLines cache combinedMat acc

                        _ ->
                            acc

        _ ->
            acc


matchSpec : List ComponentSpec -> String -> Maybe ComponentSpec
matchSpec specs file =
    specs
        |> List.filter (\spec -> spec.partFile == file)
        |> List.head
