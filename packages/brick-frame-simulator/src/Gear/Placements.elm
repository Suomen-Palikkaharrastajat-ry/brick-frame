module Gear.Placements exposing (gearSources, partSources, placementAt)

{-| Which top-level placement of a document each detected gear, axle or part
came from.

Gear detection works on lines and returns gear instances without placement
ids, so a host that edits the document (or draws only some gears) needs this
map to tell which placement a gear is. The same matching serves the axles and
parts that turn with a gear.

A placement matches when it is the same part file and its origin is within
`originTolerance` (0.01 LDU) of the line's origin. Technic mode adds every
placement at exactly computed positions, so the tolerance only absorbs float
noise.

-}

import Dict exposing (Dict)
import Gear.Types exposing (GearId, GearInstance)
import LDraw.Document as Document exposing (Document)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)


originTolerance : Float
originTolerance =
    0.01


{-| The main-model placement with this part file whose origin is at `origin`
(LDraw coordinates), if any.
-}
placementAt : Document -> String -> Vec3 -> Maybe Int
placementAt document file origin =
    Document.mainPlacements document
        |> List.filter
            (\placement ->
                placement.part
                    == file
                    && Vec3.distance (Mat4.transform placement.matrix (Vec3.vec3 0 0 0)) origin
                    < originTolerance
            )
        |> List.head
        |> Maybe.map .id


{-| The main-model placement each detected gear came from: same part file, same
position. A gear found inside a submodel or another part has no placement of
its own and is left out.
-}
gearSources : Document -> List GearInstance -> Dict GearId Int
gearSources document gears =
    gears
        |> List.filterMap
            (\gear ->
                placementAt document gear.spec.partFile gear.worldPosition
                    |> Maybe.andThen
                        (\placementId ->
                            if Document.isGlued placementId document then
                                Nothing

                            else
                                Just ( gear.id, placementId )
                        )
            )
        |> Dict.fromList


{-| The main-model placement of each driven part (an axle or a part threaded on
a gear), paired with the gear that drives it. Parts without a placement of their
own or glued placements are left out.
-}
partSources : Document -> List { a | file : String, transform : Mat4, drivingGearId : GearId } -> List ( GearId, Int )
partSources document parts =
    parts
        |> List.filterMap
            (\part ->
                placementAt document part.file (Mat4.transform part.transform (Vec3.vec3 0 0 0))
                    |> Maybe.andThen
                        (\placementId ->
                            if Document.isGlued placementId document then
                                Nothing

                            else
                                Just ( part.drivingGearId, placementId )
                        )
            )
