module LDraw.Export.Blender exposing
    ( ExportConfig
    , defaultConfig
    , exportBlenderScript
    , exportBlenderScriptWithGeometry
    )

{-| Blender Python script generator for LDraw models.

Generates a standalone Python script utilizing Blender's `bpy` API.
Sets up:

  - Blender Cycles render engine with film transparency
  - Studio 3-point lighting (Key, Fill, Rim lights) and ground backdrop
  - Principled BSDF plastic materials mapped from LDraw colors
  - Procedural `Bevel` shader nodes (`radius = 0.0004m`) wired into BSDF normals
    for soft, injection-molded specular edge highlights in Cycles raytracing.

-}

import Dict exposing (Dict)
import LDraw.Colors as Colors
import LDraw.Document as Document exposing (Document, Placement)
import LDraw.Geometry as Geometry
import LDraw.Resolve exposing (PartCache)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3
import Math.Vector4 as Vec4 exposing (Vec4)
import Render.Mesh exposing (Vertex)


{-| Export configuration.
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


{-| Export an LDraw document as a standalone Blender Cycles Python setup script.
-}
exportBlenderScript : Document -> ExportConfig -> String
exportBlenderScript doc config =
    let
        allPlacements =
            Document.placements doc

        colorsUsed =
            List.foldl
                (\p set -> Dict.insert p.color True set)
                Dict.empty
                allPlacements
                |> Dict.keys

        materialDefs =
            String.join "\n" (List.map buildMaterialFunction colorsUsed)

        placementCommands =
            String.join "\n" (List.indexedMap buildPlacementCommand allPlacements)
    in
    String.join "\n"
        [ "#!/usr/bin/env python3"
        , "\"\"\""
        , "Palikkakehys LDraw Blender Importer & Cycles Studio Setup"
        , "Generated automatically by Palikkakehys"
        , "\"\"\""
        , ""
        , "import bpy"
        , "import math"
        , "from mathutils import Vector, Matrix, Euler"
        , ""
        , "# --- 1. Scene Cleanup ---"
        , "def setup_clean_scene():"
        , "    bpy.ops.object.select_all(action='SELECT')"
        , "    bpy.ops.object.delete(use_global=False)"
        , ""
        , "# --- 2. Render & Studio Lighting Setup ---"
        , "def setup_studio():"
        , "    scene = bpy.context.scene"
        , "    scene.render.engine = 'CYCLES'"
        , "    scene.cycles.feature_set = 'SUPPORTED'"
        , "    scene.cycles.samples = 128"
        , "    scene.render.film_transparent = True"
        , ""
        , "    # Key Light"
        , "    key_data = bpy.data.lights.new(name='Key_Light', type='AREA')"
        , "    key_data.energy = 500"
        , "    key_data.size = 2.0"
        , "    key_obj = bpy.data.objects.new(name='Key_Light', object_data=key_data)"
        , "    bpy.context.collection.objects.link(key_obj)"
        , "    key_obj.location = (2.0, -3.0, 3.5)"
        , "    key_obj.rotation_euler = (math.radians(50), 0, math.radians(35))"
        , ""
        , "    # Fill Light"
        , "    fill_data = bpy.data.lights.new(name='Fill_Light', type='AREA')"
        , "    fill_data.energy = 250"
        , "    fill_data.size = 3.0"
        , "    fill_obj = bpy.data.objects.new(name='Fill_Light', object_data=fill_data)"
        , "    bpy.context.collection.objects.link(fill_obj)"
        , "    fill_obj.location = (-3.0, -2.0, 2.0)"
        , "    fill_obj.rotation_euler = (math.radians(45), 0, math.radians(-50))"
        , ""
        , "    # Rim Light"
        , "    rim_data = bpy.data.lights.new(name='Rim_Light', type='AREA')"
        , "    rim_data.energy = 400"
        , "    rim_data.size = 1.5"
        , "    rim_obj = bpy.data.objects.new(name='Rim_Light', object_data=rim_data)"
        , "    bpy.context.collection.objects.link(rim_obj)"
        , "    rim_obj.location = (0.0, 3.0, 3.0)"
        , "    rim_obj.rotation_euler = (math.radians(-45), 0, math.radians(180))"
        , ""
        , "    # Studio Floor Ground"
        , "    bpy.ops.mesh.primitive_plane_add(size=30, enter_editmode=False, location=(0, 0, 0))"
        , "    ground = bpy.context.active_object"
        , "    ground.name = 'Studio_Ground'"
        , "    ground_mat = bpy.data.materials.new(name='Studio_Ground_Material')"
        , "    ground_mat.use_nodes = True"
        , "    bsdf = ground_mat.node_tree.nodes.get('Principled BSDF')"
        , "    if bsdf:"
        , "        bsdf.inputs['Base Color'].default_value = (0.08, 0.09, 0.12, 1.0)"
        , "        bsdf.inputs['Roughness'].default_value = 0.4"
        , "    ground.data.materials.append(ground_mat)"
        , ""
        , "# --- 3. PBR Plastic Materials with Bevel Shader Nodes ---"
        , "def create_pbr_plastic(name, color_rgba, roughness=0.25, metallic=0.0, transmission=0.0):"
        , "    mat = bpy.data.materials.new(name=name)"
        , "    mat.use_nodes = True"
        , "    nodes = mat.node_tree.nodes"
        , "    links = mat.node_tree.links"
        , "    nodes.clear()"
        , ""
        , "    output_node = nodes.new(type='ShaderNodeOutputMaterial')"
        , "    output_node.location = (350, 0)"
        , ""
        , "    bsdf_node = nodes.new(type='ShaderNodeBsdfPrincipled')"
        , "    bsdf_node.location = (0, 0)"
        , "    bsdf_node.inputs['Base Color'].default_value = color_rgba"
        , "    bsdf_node.inputs['Roughness'].default_value = roughness"
        , "    bsdf_node.inputs['Metallic'].default_value = metallic"
        , "    if 'Transmission Weight' in bsdf_node.inputs:"
        , "        bsdf_node.inputs['Transmission Weight'].default_value = transmission"
        , "    elif 'Transmission' in bsdf_node.inputs:"
        , "        bsdf_node.inputs['Transmission'].default_value = transmission"
        , ""
        , "    # Procedural Bevel Shader node for injection molded edges"
        , "    bevel_node = nodes.new(type='ShaderNodeBevel')"
        , "    bevel_node.location = (-250, -100)"
        , "    bevel_node.inputs['Radius'].default_value = 0.0004  # 0.4mm bevel radius"
        , "    bevel_node.samples = 8"
        , ""
        , "    links.new(bevel_node.outputs['Normal'], bsdf_node.inputs['Normal'])"
        , "    links.new(bsdf_node.outputs['BSDF'], output_node.inputs['Surface'])"
        , "    return mat"
        , ""
        , materialDefs
        , ""
        , "# --- 4. Main Build Execution ---"
        , "def main():"
        , "    setup_clean_scene()"
        , "    setup_studio()"
        , "    model_col = bpy.data.collections.new('LDraw_Model')"
        , "    bpy.context.scene.collection.children.link(model_col)"
        , ""
        , placementCommands
        , ""
        , "if __name__ == '__main__':"
        , "    main()"
        ]


buildMaterialFunction : Int -> String
buildMaterialFunction code =
    let
        rgba =
            Colors.resolveColor 16 code

        matName =
            "ldraw_mat_" ++ String.fromInt code

        ( roughness, metallic, transmission ) =
            if rgba.alpha < 0.95 then
                ( "0.05", "0.0", "0.95" )

            else if code == 80 || code == 81 || code == 82 || code == 334 || code == 383 then
                ( "0.10", "0.9", "0.0" )

            else
                ( "0.25", "0.0", "0.0" )

        rgbaTuple =
            "("
                ++ String.fromFloat rgba.r
                ++ ", "
                ++ String.fromFloat rgba.g
                ++ ", "
                ++ String.fromFloat rgba.b
                ++ ", "
                ++ String.fromFloat rgba.alpha
                ++ ")"
    in
    String.join "\n"
        [ "def get_material_" ++ String.fromInt code ++ "():"
        , "    name = '" ++ matName ++ "'"
        , "    if name in bpy.data.materials:"
        , "        return bpy.data.materials[name]"
        , "    return create_pbr_plastic(name, "
            ++ rgbaTuple
            ++ ", roughness="
            ++ roughness
            ++ ", metallic="
            ++ metallic
            ++ ", transmission="
            ++ transmission
            ++ ")"
        ]


buildPlacementCommand : Int -> Placement -> String
buildPlacementCommand index placement =
    let
        record =
            Mat4.toRecord placement.matrix

        -- Convert matrix to 4x4 matrix tuple string
        matrixStr =
            "[["
                ++ String.fromFloat record.m11
                ++ ", "
                ++ String.fromFloat record.m21
                ++ ", "
                ++ String.fromFloat record.m31
                ++ ", "
                ++ String.fromFloat (record.m41 * 0.0004)
                -- Scale LDU to meters (1 LDU = 0.4mm = 0.0004m)
                ++ "], ["
                ++ String.fromFloat -record.m12
                ++ ", "
                ++ String.fromFloat -record.m22
                ++ ", "
                ++ String.fromFloat -record.m32
                ++ ", "
                ++ String.fromFloat (-record.m42 * 0.0004)
                ++ "], ["
                ++ String.fromFloat -record.m13
                ++ ", "
                ++ String.fromFloat -record.m23
                ++ ", "
                ++ String.fromFloat -record.m33
                ++ ", "
                ++ String.fromFloat (-record.m43 * 0.0004)
                ++ "], [0, 0, 0, 1]]"

        objName =
            "Part_" ++ String.fromInt index ++ "_" ++ placement.part
    in
    String.join "\n"
        [ "    # Part " ++ String.fromInt index ++ ": " ++ placement.part
        , "    part_mat = get_material_" ++ String.fromInt placement.color ++ "()"
        , "    bpy.ops.mesh.primitive_cube_add(size=0.01)"
        , "    obj = bpy.context.active_object"
        , "    obj.name = '" ++ objName ++ "'"
        , "    obj.matrix_world = Matrix(" ++ matrixStr ++ ")"
        , "    obj.data.materials.clear()"
        , "    obj.data.materials.append(part_mat)"
        , "    model_col.objects.link(obj)"
        , "    bpy.context.collection.objects.unlink(obj)"
        ]


{-| Export Blender script with complete embedded geometry from PartCache.
-}
exportBlenderScriptWithGeometry : Document -> PartCache -> ExportConfig -> String
exportBlenderScriptWithGeometry doc cache config =
    let
        lines =
            Document.toLines doc

        flatGeom =
            Geometry.flattenWith Geometry.worldOptions lines cache 15 Mat4.identity

        triangles =
            flatGeom.triangles

        vertLines =
            triangles
                |> List.concatMap (\( v1, v2, v3 ) -> [ v1.position, v2.position, v3.position ])
                |> List.map
                    (\p ->
                        "("
                            ++ String.fromFloat (Vec3.getX p * 0.0004)
                            ++ ", "
                            ++ String.fromFloat (Vec3.getY p * 0.0004)
                            ++ ", "
                            ++ String.fromFloat (Vec3.getZ p * 0.0004)
                            ++ ")"
                    )

        faceLines =
            List.range 0 (List.length triangles - 1)
                |> List.map
                    (\tIdx ->
                        let
                            i0 =
                                tIdx * 3

                            i1 =
                                tIdx * 3 + 1

                            i2 =
                                tIdx * 3 + 2
                        in
                        "(" ++ String.fromInt i0 ++ ", " ++ String.fromInt i1 ++ ", " ++ String.fromInt i2 ++ ")"
                    )

        vertsCode =
            "    vertices = [" ++ String.join ", " vertLines ++ "]"

        facesCode =
            "    faces = [" ++ String.join ", " faceLines ++ "]"

        baseScript =
            exportBlenderScript doc config
    in
    String.join "\n"
        [ baseScript
        , ""
        , "def create_mesh_geometry():"
        , vertsCode
        , facesCode
        , "    mesh = bpy.data.meshes.new('Palikkakehys_LDraw_Mesh')"
        , "    mesh.from_pydata(vertices, [], faces)"
        , "    mesh.update()"
        , "    obj = bpy.data.objects.new('Palikkakehys_LDraw_Model', mesh)"
        , "    bpy.context.scene.collection.objects.link(obj)"
        ]
