module LDraw.Export exposing
    ( ExportConfig
    , ExportFormat(..)
    , GlTFPayload
    , UsdzPayload
    , defaultConfig
    , exportBlenderScript
    , exportGlTF
    , exportUSDA
    , exportUSDZ
    )

{-| Universal 3D exporters for Palikkakehys LDraw models.

Supports:

  - glTF 2.0 / GLB: PBR plastic materials (roughness, metallic, alpha blending)
  - Blender Python script: Cycles render setup with procedural edge bevel shader nodes
  - USDA / USDZ: Apple AR QuickLook on iOS (1:1 real-world scale Augmented Reality)

-}

import LDraw.Document exposing (Document)
import LDraw.Export.Blender as Blender
import LDraw.Export.GlTF as GlTF
import LDraw.Export.USDZ as USDZ
import LDraw.Resolve exposing (PartCache)


{-| Supported universal 3D export formats.
-}
type ExportFormat
    = GlTFBinary
    | BlenderScript
    | UsdzArchive


{-| Shared export configuration.
-}
type alias ExportConfig =
    { includeStudLogos : Bool
    , weldVertices : Bool
    , groupSubmodels : Bool
    }


{-| Default export configuration.
-}
defaultConfig : ExportConfig
defaultConfig =
    { includeStudLogos = True
    , weldVertices = True
    , groupSubmodels = True
    }


{-| Payload returned from glTF / GLB export.
-}
type alias GlTFPayload =
    GlTF.GlTFPayload


{-| Payload returned from USDZ export.
-}
type alias UsdzPayload =
    USDZ.UsdzPayload


{-| Export document to glTF 2.0 / GLB.
-}
exportGlTF : Document -> PartCache -> ExportConfig -> GlTFPayload
exportGlTF =
    GlTF.exportGlTF


{-| Export document to standalone Blender Cycles Python setup script.
-}
exportBlenderScript : Document -> ExportConfig -> String
exportBlenderScript =
    Blender.exportBlenderScript


{-| Export document to USDA human-readable text format.
-}
exportUSDA : Document -> PartCache -> ExportConfig -> String
exportUSDA =
    USDZ.exportUSDA


{-| Export document to USDZ archive for Apple AR QuickLook.
-}
exportUSDZ : Document -> PartCache -> ExportConfig -> UsdzPayload
exportUSDZ =
    USDZ.exportUSDZ
