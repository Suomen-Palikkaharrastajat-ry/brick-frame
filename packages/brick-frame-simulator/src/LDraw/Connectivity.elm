module LDraw.Connectivity exposing
    ( BoundingBox3D
    , CollisionMode(..)
    , ConnectionPoint
    , ConnectionType(..)
    , OBB
    , PartConnectivity
    , SnapCandidate
    , SnapGuide(..)
    , WorldConnectionPoint
    , checkCollisions
    , defaultSnapDistance
    , effectiveEnvelope
    , extractConnections
    , extractPartConnectivity
    , invertRigid
    , isCompatible
    , isStudded
    , knownCatalogPart
    , makeAxle
    , makeGear
    , makeGearPinHoles
    , setGuideColliding
    , snapEnterDistance
    , snapReleaseDistance
    , solveSnap
    , solveSnapWithHysteresis
    , studOffset
    , testOBB
    , toOBB
    , transformDirection
    , transformPoint
    )

{-| Procedural connection point detection, magnetic snapping, and 3D SAT
bounding volume collision detection for LDraw models.

Supports studs, anti-studs, Technic pins and axles, clips, bars, and ball
joints.

-}

import Dict exposing (Dict)
import LDraw.Types exposing (LDrawLine(..), SubFileData)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Set


{-| Standard connection types used across the Lego and LDraw ecosystem.
-}
type ConnectionType
    = Stud
    | AntiStud
    | TechnicPinMale { length : Float, friction : Bool }
    | TechnicPinFemale { length : Float }
    | TechnicAxleMale { length : Float }
    | TechnicAxleFemale { length : Float }
    | ClipMale { width : Float }
    | BarFemale { diameter : Float }
    | BallMale
    | BallSocketFemale


{-| A local connection site on a part.
`normal` points outward along the connection engagement axis.
`tangent` provides the reference orientation for rotation snapping.
-}
type alias ConnectionPoint =
    { id : Int
    , connType : ConnectionType
    , localPosition : Vec3
    , normal : Vec3
    , tangent : Vec3
    }


{-| An axis-aligned 3D bounding box in local part space.
-}
type alias BoundingBox3D =
    { min : Vec3
    , max : Vec3
    }


{-| Connectivity and collision descriptor for a part.
-}
type alias PartConnectivity =
    { part : String
    , connections : List ConnectionPoint
    , boundingBoxes : List BoundingBox3D
    }


{-| A connection point transformed into world coordinates by its placement matrix.
-}
type alias WorldConnectionPoint =
    { placementId : Int
    , pointId : Int
    , connType : ConnectionType
    , worldPosition : Vec3
    , worldNormal : Vec3
    , worldTangent : Vec3
    }


{-| An Oriented Bounding Box in 3D for fast Separating Axis Theorem (SAT) tests.
-}
type alias OBB =
    { center : Vec3
    , halfExtents : Vec3
    , axisX : Vec3
    , axisY : Vec3
    , axisZ : Vec3
    }


{-| Collision resolution behaviour in the editor.
-}
type CollisionMode
    = CollisionStrict
    | CollisionSoft
    | CollisionOff


{-| Visual guide displayed in the WebGL overlay when snapping.
-}
type SnapGuide
    = StudRingGuide Vec3 Vec3 Bool
    | AxleAxisGuide Vec3 Vec3 Bool
    | ClipLockGuide Vec3 Vec3 Bool


{-| A solved magnetic snap alignment.
-}
type alias SnapCandidate =
    { draggedPoint : ConnectionPoint
    , targetPoint : WorldConnectionPoint
    , targetPlacementId : Int
    , targetWorldPosition : Vec3
    , targetWorldNormal : Vec3
    , snapMatrix : Mat4
    , distance : Float
    , guide : SnapGuide
    }


{-| Default magnetic snap activation threshold in LDU.
-}
defaultSnapDistance : Float
defaultSnapDistance =
    12.0


{-| Entry threshold for magnetic snapping in LDU.
-}
snapEnterDistance : Float
snapEnterDistance =
    12.0


{-| Release threshold for magnetic snapping hysteresis in LDU.
-}
snapReleaseDistance : Float
snapReleaseDistance =
    18.0



-- ── Compatibility ────────────────────────────────────────────────────────────


{-| Whether two connection types can physically interface.
-}
isCompatible : ConnectionType -> ConnectionType -> Bool
isCompatible a b =
    case ( a, b ) of
        ( Stud, AntiStud ) ->
            True

        ( AntiStud, Stud ) ->
            True

        ( TechnicPinMale _, TechnicPinFemale _ ) ->
            True

        ( TechnicPinFemale _, TechnicPinMale _ ) ->
            True

        ( TechnicAxleMale _, TechnicAxleFemale _ ) ->
            True

        ( TechnicAxleFemale _, TechnicAxleMale _ ) ->
            True

        ( TechnicAxleMale _, TechnicPinFemale _ ) ->
            True

        ( TechnicPinFemale _, TechnicAxleMale _ ) ->
            True

        ( ClipMale _, BarFemale _ ) ->
            True

        ( BarFemale _, ClipMale _ ) ->
            True

        ( BallMale, BallSocketFemale ) ->
            True

        ( BallSocketFemale, BallMale ) ->
            True

        _ ->
            False



-- ── Vector & Coordinate Math ──────────────────────────────────────────────────


{-| Transform a direction vector by a 4x4 matrix and normalize it.
-}
transformDirection : Mat4 -> Vec3 -> Vec3
transformDirection matrix dir =
    let
        p0 =
            Mat4.transform matrix (vec3 0 0 0)

        p1 =
            Mat4.transform matrix dir

        d =
            Vec3.sub p1 p0

        len =
            Vec3.length d
    in
    if len < 1.0e-6 then
        dir

    else
        Vec3.scale (1 / len) d


{-| Transform a local connection point into world space.
-}
transformPoint : Mat4 -> Int -> ConnectionPoint -> WorldConnectionPoint
transformPoint matrix placementId point =
    { placementId = placementId
    , pointId = point.id
    , connType = point.connType
    , worldPosition = Mat4.transform matrix point.localPosition
    , worldNormal = transformDirection matrix point.normal
    , worldTangent = transformDirection matrix point.tangent
    }


{-| Invert a rigid 4x4 matrix (pure rotation + translation).
-}
invertRigid : Mat4 -> Mat4
invertRigid matrix =
    let
        m =
            Mat4.toRecord matrix

        tx =
            -(m.m11 * m.m14 + m.m21 * m.m24 + m.m31 * m.m34)

        ty =
            -(m.m12 * m.m14 + m.m22 * m.m24 + m.m32 * m.m34)

        tz =
            -(m.m13 * m.m14 + m.m23 * m.m24 + m.m33 * m.m34)
    in
    Mat4.fromRecord
        { m11 = m.m11
        , m12 = m.m21
        , m13 = m.m31
        , m14 = tx
        , m21 = m.m12
        , m22 = m.m22
        , m23 = m.m32
        , m24 = ty
        , m31 = m.m13
        , m32 = m.m23
        , m33 = m.m33
        , m34 = tz
        , m41 = 0
        , m42 = 0
        , m43 = 0
        , m44 = 1
        }


{-| Update the collision state on a snap guide.
-}
setGuideColliding : Bool -> SnapGuide -> SnapGuide
setGuideColliding col guide =
    case guide of
        StudRingGuide pos norm _ ->
            StudRingGuide pos norm col

        AxleAxisGuide pos ax _ ->
            AxleAxisGuide pos ax col

        ClipLockGuide pos norm _ ->
            ClipLockGuide pos norm col


{-| Build a right-handed orthonormal basis from a normal and a tangent hint.
-}
orthonormalBasis : Vec3 -> Vec3 -> { normal : Vec3, tangent : Vec3, bitangent : Vec3 }
orthonormalBasis normalRaw tangentHint =
    let
        nLen =
            Vec3.length normalRaw

        n =
            if nLen < 1.0e-6 then
                vec3 0 -1 0

            else
                Vec3.scale (1 / nLen) normalRaw

        dotNT =
            Vec3.dot tangentHint n

        proj =
            Vec3.sub tangentHint (Vec3.scale dotNT n)

        projLen =
            Vec3.length proj

        u =
            if projLen < 1.0e-6 then
                let
                    alt =
                        if abs (Vec3.getX n) < 0.9 then
                            vec3 1 0 0

                        else
                            vec3 0 1 0

                    altDot =
                        Vec3.dot alt n

                    altProj =
                        Vec3.sub alt (Vec3.scale altDot n)

                    altProjLen =
                        Vec3.length altProj
                in
                if altProjLen < 1.0e-6 then
                    vec3 1 0 0

                else
                    Vec3.scale (1 / altProjLen) altProj

            else
                Vec3.scale (1 / projLen) proj

        v =
            Vec3.cross n u
    in
    { normal = n, tangent = u, bitangent = v }



-- ── Snap Solver ──────────────────────────────────────────────────────────────


{-| Solve the best magnetic snap transformation between dragged connection points
and candidate target points in the scene.
-}
solveSnap :
    Float
    -> Int
    -> Mat4
    -> List ConnectionPoint
    -> List WorldConnectionPoint
    -> Maybe SnapCandidate
solveSnap maxDist rotationIndex currentMatrix draggedPoints targetPoints =
    let
        accessibleTargetPoints =
            List.filter
                (\tp ->
                    not (tp.connType == AntiStud && Vec3.getY tp.worldPosition >= -0.01 && Vec3.getY tp.worldNormal > 0.7)
                )
                targetPoints

        evalPair draggedLocal targetWorld =
            if not (isCompatible draggedLocal.connType targetWorld.connType) then
                Nothing

            else
                let
                    draggedWorldPos =
                        Mat4.transform currentMatrix draggedLocal.localPosition

                    dist =
                        Vec3.distance draggedWorldPos targetWorld.worldPosition
                in
                if dist > maxDist then
                    Nothing

                else
                    let
                        draggedWorldNormal =
                            transformDirection currentMatrix draggedLocal.normal

                        desiredNormal =
                            targetDesiredNormal draggedLocal.connType draggedWorldNormal targetWorld.worldNormal

                        cosAngle =
                            Vec3.dot draggedWorldNormal desiredNormal
                    in
                    if isDirectional draggedLocal.connType && cosAngle < -0.2 then
                        Nothing

                    else
                        let
                            snapMat =
                                computeSnapMatrix rotationIndex draggedLocal targetWorld desiredNormal

                            originY =
                                Vec3.getY (Mat4.transform snapMat (vec3 0 0 0))

                            draggedY =
                                Vec3.getY (Mat4.transform snapMat draggedLocal.localPosition)

                            isPenetratingGround =
                                if Vec3.getY targetWorld.worldPosition <= 0.5 then
                                    originY
                                        > 0.5
                                        || draggedY
                                        > 0.5
                                        || List.any (\pt -> Vec3.getY (Mat4.transform snapMat pt.localPosition) > 0.5) draggedPoints

                                else
                                    False
                        in
                        if isPenetratingGround then
                            Nothing

                        else
                            let
                                bias =
                                    if draggedLocal.connType == AntiStud && targetWorld.connType == Stud then
                                        -0.5

                                    else
                                        0.0

                                score =
                                    dist + (1.0 - cosAngle) * 5.0 + bias

                                guide =
                                    makeGuide draggedLocal.connType targetWorld.worldPosition targetWorld.worldNormal False
                            in
                            Just
                                ( score
                                , { draggedPoint = draggedLocal
                                  , targetPoint = targetWorld
                                  , targetPlacementId = targetWorld.placementId
                                  , targetWorldPosition = targetWorld.worldPosition
                                  , targetWorldNormal = targetWorld.worldNormal
                                  , snapMatrix = snapMat
                                  , distance = dist
                                  , guide = guide
                                  }
                                )

        candidates =
            draggedPoints
                |> List.concatMap (\d -> List.filterMap (evalPair d) accessibleTargetPoints)
                |> List.sortBy Tuple.first
    in
    List.head candidates |> Maybe.map Tuple.second


{-| Solve snap with magnetic hysteresis latching:
If an active snap candidate is provided, the connection remains latched as long as
the dragged connection point stays within `releaseDist` (default 18.0 LDU) of the target.
If the distance exceeds `releaseDist` or no active snap is present, candidates are
searched within `enterDist` (default 12.0 LDU).
-}
solveSnapWithHysteresis :
    Float
    -> Float
    -> Maybe SnapCandidate
    -> Int
    -> Mat4
    -> List ConnectionPoint
    -> List WorldConnectionPoint
    -> Maybe SnapCandidate
solveSnapWithHysteresis enterDist releaseDist maybeActive rotationIndex currentMatrix draggedPoints targetPoints =
    case maybeActive of
        Just active ->
            let
                draggedPos =
                    Mat4.transform currentMatrix active.draggedPoint.localPosition

                dist =
                    Vec3.distance draggedPos active.targetPoint.worldPosition
            in
            if dist <= releaseDist then
                let
                    draggedWorldNormal =
                        transformDirection currentMatrix active.draggedPoint.normal

                    desiredNormal =
                        targetDesiredNormal active.draggedPoint.connType draggedWorldNormal active.targetPoint.worldNormal

                    cosAngle =
                        Vec3.dot draggedWorldNormal desiredNormal
                in
                if isDirectional active.draggedPoint.connType && cosAngle < -0.2 then
                    solveSnap enterDist rotationIndex currentMatrix draggedPoints targetPoints

                else
                    let
                        snapMat =
                            computeSnapMatrix rotationIndex active.draggedPoint active.targetPoint desiredNormal

                        guide =
                            makeGuide active.draggedPoint.connType active.targetPoint.worldPosition active.targetPoint.worldNormal False
                    in
                    Just
                        { active
                            | snapMatrix = snapMat
                            , distance = dist
                            , guide = guide
                        }

            else
                solveSnap enterDist rotationIndex currentMatrix draggedPoints targetPoints

        Nothing ->
            solveSnap enterDist rotationIndex currentMatrix draggedPoints targetPoints


isDirectional : ConnectionType -> Bool
isDirectional conn =
    case conn of
        Stud ->
            True

        AntiStud ->
            True

        ClipMale _ ->
            True

        BarFemale _ ->
            True

        BallMale ->
            True

        BallSocketFemale ->
            True

        _ ->
            False


targetDesiredNormal : ConnectionType -> Vec3 -> Vec3 -> Vec3
targetDesiredNormal conn draggedWorldNormal targetWorldNormal =
    case conn of
        TechnicAxleMale _ ->
            if Vec3.dot draggedWorldNormal targetWorldNormal < 0 then
                Vec3.negate targetWorldNormal

            else
                targetWorldNormal

        TechnicAxleFemale _ ->
            if Vec3.dot draggedWorldNormal targetWorldNormal < 0 then
                Vec3.negate targetWorldNormal

            else
                targetWorldNormal

        TechnicPinMale _ ->
            if Vec3.dot draggedWorldNormal targetWorldNormal < 0 then
                Vec3.negate targetWorldNormal

            else
                targetWorldNormal

        TechnicPinFemale _ ->
            if Vec3.dot draggedWorldNormal targetWorldNormal < 0 then
                Vec3.negate targetWorldNormal

            else
                targetWorldNormal

        _ ->
            Vec3.negate targetWorldNormal


computeSnapMatrix : Int -> ConnectionPoint -> WorldConnectionPoint -> Vec3 -> Mat4
computeSnapMatrix rotationIndex draggedLocal targetWorld desiredNormal =
    let
        basisD =
            orthonormalBasis draggedLocal.normal draggedLocal.tangent

        basisT0 =
            orthonormalBasis desiredNormal targetWorld.worldTangent

        step =
            modBy 4 (modBy 4 rotationIndex + 4)

        theta =
            toFloat step * (pi / 2)

        cosT =
            cos theta

        sinT =
            sin theta

        uT =
            Vec3.add (Vec3.scale cosT basisT0.tangent) (Vec3.scale sinT basisT0.bitangent)

        vT =
            Vec3.cross desiredNormal uT

        -- Transform column j: R(e_j) = dot(e_j, Ud)*uT + dot(e_j, Vd)*vT + dot(e_j, Nd)*desiredNormal
        uDx =
            Vec3.getX basisD.tangent

        uDy =
            Vec3.getY basisD.tangent

        uDz =
            Vec3.getZ basisD.tangent

        vDx =
            Vec3.getX basisD.bitangent

        vDy =
            Vec3.getY basisD.bitangent

        vDz =
            Vec3.getZ basisD.bitangent

        nDx =
            Vec3.getX basisD.normal

        nDy =
            Vec3.getY basisD.normal

        nDz =
            Vec3.getZ basisD.normal

        col1 =
            Vec3.add (Vec3.scale uDx uT) (Vec3.add (Vec3.scale vDx vT) (Vec3.scale nDx desiredNormal))

        col2 =
            Vec3.add (Vec3.scale uDy uT) (Vec3.add (Vec3.scale vDy vT) (Vec3.scale nDy desiredNormal))

        col3 =
            Vec3.add (Vec3.scale uDz uT) (Vec3.add (Vec3.scale vDz vT) (Vec3.scale nDz desiredNormal))

        -- Rotate dragged local position by R:
        pLocal =
            draggedLocal.localPosition

        pLx =
            Vec3.getX pLocal

        pLy =
            Vec3.getY pLocal

        pLz =
            Vec3.getZ pLocal

        rotP =
            Vec3.add (Vec3.scale pLx col1) (Vec3.add (Vec3.scale pLy col2) (Vec3.scale pLz col3))

        trans =
            Vec3.sub targetWorld.worldPosition rotP
    in
    Mat4.fromRecord
        { m11 = Vec3.getX col1
        , m21 = Vec3.getY col1
        , m31 = Vec3.getZ col1
        , m41 = 0
        , m12 = Vec3.getX col2
        , m22 = Vec3.getY col2
        , m32 = Vec3.getZ col2
        , m42 = 0
        , m13 = Vec3.getX col3
        , m23 = Vec3.getY col3
        , m33 = Vec3.getZ col3
        , m43 = 0
        , m14 = Vec3.getX trans
        , m24 = Vec3.getY trans
        , m34 = Vec3.getZ trans
        , m44 = 1
        }


makeGuide : ConnectionType -> Vec3 -> Vec3 -> Bool -> SnapGuide
makeGuide conn pos normal isColliding =
    case conn of
        Stud ->
            StudRingGuide pos normal isColliding

        AntiStud ->
            StudRingGuide pos normal isColliding

        TechnicAxleMale _ ->
            AxleAxisGuide pos normal isColliding

        TechnicAxleFemale _ ->
            AxleAxisGuide pos normal isColliding

        TechnicPinMale _ ->
            AxleAxisGuide pos normal isColliding

        TechnicPinFemale _ ->
            AxleAxisGuide pos normal isColliding

        _ ->
            ClipLockGuide pos normal isColliding



-- ── Collision Detection (SAT) ────────────────────────────────────────────────


{-| Build an Oriented Bounding Box from a local bounding box and a transform matrix.
-}
toOBB : BoundingBox3D -> Mat4 -> OBB
toOBB box matrix =
    let
        centerLocal =
            Vec3.scale 0.5 (Vec3.add box.min box.max)

        halfLocal =
            Vec3.scale 0.5 (Vec3.sub box.max box.min)

        worldCenter =
            Mat4.transform matrix centerLocal

        axisX =
            transformDirection matrix (vec3 1 0 0)

        axisY =
            transformDirection matrix (vec3 0 1 0)

        axisZ =
            transformDirection matrix (vec3 0 0 1)

        scaleX =
            Vec3.length (Vec3.sub (Mat4.transform matrix (vec3 1 0 0)) (Mat4.transform matrix (vec3 0 0 0)))

        scaleY =
            Vec3.length (Vec3.sub (Mat4.transform matrix (vec3 0 1 0)) (Mat4.transform matrix (vec3 0 0 0)))

        scaleZ =
            Vec3.length (Vec3.sub (Mat4.transform matrix (vec3 0 0 1)) (Mat4.transform matrix (vec3 0 0 0)))

        halfExtents =
            vec3
                (abs (Vec3.getX halfLocal) * scaleX)
                (abs (Vec3.getY halfLocal) * scaleY)
                (abs (Vec3.getZ halfLocal) * scaleZ)
    in
    { center = worldCenter
    , halfExtents = halfExtents
    , axisX = axisX
    , axisY = axisY
    , axisZ = axisZ
    }


{-| Effective collision envelope that excludes external stud protrusions (height = 4 LDU),
preventing false collisions when studs enter compatible cavities.
-}
effectiveEnvelope : BoundingBox3D -> List ConnectionPoint -> BoundingBox3D
effectiveEnvelope box connections =
    let
        hasTopStuds =
            List.any
                (\c ->
                    case c.connType of
                        Stud ->
                            Vec3.getY c.normal < -0.7

                        _ ->
                            False
                )
                connections

        minPt =
            box.min

        newMinY =
            if hasTopStuds && Vec3.getY minPt < -0.1 then
                0

            else
                Vec3.getY minPt
    in
    { min = vec3 (Vec3.getX minPt) newMinY (Vec3.getZ minPt)
    , max = box.max
    }


{-| 3D Separating Axis Theorem (SAT) test between two OBBs with a skin tolerance.
Returns True when the two boxes intersect by more than `tolerance`.
-}
testOBB : Float -> OBB -> OBB -> Bool
testOBB tolerance a b =
    let
        d =
            Vec3.sub b.center a.center

        axesA =
            [ a.axisX, a.axisY, a.axisZ ]

        axesB =
            [ b.axisX, b.axisY, b.axisZ ]

        crossAxes =
            List.concatMap
                (\ax ->
                    List.filterMap
                        (\bx ->
                            let
                                c =
                                    Vec3.cross ax bx

                                len =
                                    Vec3.length c
                            in
                            if len < 1.0e-5 then
                                Nothing

                            else
                                Just (Vec3.scale (1 / len) c)
                        )
                        axesB
                )
                axesA

        allAxes =
            axesA ++ axesB ++ crossAxes

        overlapsAlong axis =
            let
                rA =
                    abs (Vec3.dot a.axisX axis)
                        * Vec3.getX a.halfExtents
                        + abs (Vec3.dot a.axisY axis)
                        * Vec3.getY a.halfExtents
                        + abs (Vec3.dot a.axisZ axis)
                        * Vec3.getZ a.halfExtents

                rB =
                    abs (Vec3.dot b.axisX axis)
                        * Vec3.getX b.halfExtents
                        + abs (Vec3.dot b.axisY axis)
                        * Vec3.getY b.halfExtents
                        + abs (Vec3.dot b.axisZ axis)
                        * Vec3.getZ b.halfExtents

                dist =
                    abs (Vec3.dot d axis)
            in
            (rA + rB) - dist > tolerance
    in
    List.all overlapsAlong allAxes


{-| Check collisions between dragged placements and static scene placements.
Returns list of colliding placement ID pairs (draggedId, otherId).
-}
checkCollisions :
    Float
    -> List ( Int, Mat4, BoundingBox3D )
    -> List ( Int, Mat4, BoundingBox3D )
    -> List ( Int, Int )
checkCollisions tolerance draggedPlacements scenePlacements =
    let
        draggedOBBs =
            List.map (\( id, mat, box ) -> ( id, toOBB box mat )) draggedPlacements

        sceneOBBs =
            List.map (\( id, mat, box ) -> ( id, toOBB box mat )) scenePlacements
    in
    draggedOBBs
        |> List.concatMap
            (\( dragId, obbA ) ->
                sceneOBBs
                    |> List.filterMap
                        (\( sceneId, obbB ) ->
                            if dragId /= sceneId && testOBB tolerance obbA obbB then
                                Just ( dragId, sceneId )

                            else
                                Nothing
                        )
            )



-- ── Connection Extraction ────────────────────────────────────────────────────


{-| Extract connection points directly from parsed LDraw lines and subfile references.
-}
extractConnections : List LDrawLine -> List ConnectionPoint
extractConnections lines =
    let
        extractSubfile idx line acc =
            case line of
                SubFileRef sub ->
                    case classifySubfile sub of
                        Just ( connType, norm, tang ) ->
                            let
                                pt =
                                    { id = idx
                                    , connType = connType
                                    , localPosition = Mat4.transform sub.transform (vec3 0 0 0)
                                    , normal = transformDirection sub.transform norm
                                    , tangent = transformDirection sub.transform tang
                                    }
                            in
                            pt :: acc

                        Nothing ->
                            acc

                _ ->
                    acc
    in
    List.indexedMap (\i l -> ( i, l )) lines
        |> List.foldl (\( i, l ) acc -> extractSubfile i l acc) []
        |> List.reverse


classifySubfile : SubFileData -> Maybe ( ConnectionType, Vec3, Vec3 )
classifySubfile sub =
    let
        file =
            String.toLower sub.file
    in
    if isStudFile file then
        Just ( Stud, vec3 0 -1 0, vec3 1 0 0 )

    else if isAntiStudFile file then
        Just ( AntiStud, vec3 0 1 0, vec3 1 0 0 )

    else if isAxleHoleFile file then
        Just ( TechnicAxleFemale { length = 20.0 }, vec3 0 0 1, vec3 1 0 0 )

    else if isAxleMaleFile file then
        Just ( TechnicAxleMale { length = 20.0 }, vec3 0 0 1, vec3 1 0 0 )

    else if isPinHoleFile file then
        Just ( TechnicPinFemale { length = 20.0 }, vec3 0 0 1, vec3 1 0 0 )

    else if isPinMaleFile file then
        Just ( TechnicPinMale { length = 20.0, friction = True }, vec3 0 0 1, vec3 1 0 0 )

    else if String.contains "clip" file then
        Just ( ClipMale { width = 4.0 }, vec3 0 0 1, vec3 1 0 0 )

    else if String.contains "bar" file then
        Just ( BarFemale { diameter = 4.0 }, vec3 0 1 0, vec3 1 0 0 )

    else if String.contains "joint8ball" file || String.contains "towball" file then
        Just ( BallMale, vec3 0 -1 0, vec3 1 0 0 )

    else if String.contains "joint8socket" file then
        Just ( BallSocketFemale, vec3 0 1 0, vec3 1 0 0 )

    else
        Nothing


isStudFile : String -> Bool
isStudFile f =
    f
        == "stud.dat"
        || f
        == "stud2.dat"
        || f
        == "stud2a.dat"
        || f
        == "stud10.dat"
        || f
        == "stud15.dat"
        || f
        == "studp01.dat"
        || String.startsWith "stud-logo" f


isAntiStudFile : String -> Bool
isAntiStudFile f =
    f
        == "stud4.dat"
        || f
        == "stud3.dat"
        || f
        == "stud3a.dat"
        || f
        == "tube.dat"
        || f
        == "studtube.dat"
        || f
        == "studtube2.dat"


isAxleHoleFile : String -> Bool
isAxleHoleFile f =
    String.startsWith "axlehole" f
        || String.startsWith "axlehol" f


isAxleMaleFile : String -> Bool
isAxleMaleFile f =
    f
        == "axle.dat"
        || String.startsWith "axleend" f
        || f
        == "axles.dat"


isPinHoleFile : String -> Bool
isPinHoleFile f =
    String.contains "pinhole" f
        || f
        == "peghole.dat"


isPinMaleFile : String -> Bool
isPinMaleFile f =
    String.contains "confric" f
        || f
        == "connect.dat"
        || f
        == "pin.dat"
        || String.startsWith "wpin" f



-- ── Standard Catalog & Recursive Extraction ──────────────────────────────────


{-| Get connectivity for standard Lego parts by name or recursive line expansion.
-}
extractPartConnectivity : String -> (String -> Maybe (List LDrawLine)) -> PartConnectivity
extractPartConnectivity partName lineResolver =
    case knownCatalogPart partName of
        Just pc ->
            pc

        Nothing ->
            let
                lines =
                    lineResolver partName |> Maybe.withDefault []

                direct =
                    extractConnections lines

                expanded =
                    if List.isEmpty direct then
                        extractRecursive lineResolver 0 Mat4.identity lines []

                    else
                        direct

                box =
                    inferBounds expanded
            in
            { part = partName
            , connections = expanded
            , boundingBoxes = [ box ]
            }


extractRecursive : (String -> Maybe (List LDrawLine)) -> Int -> Mat4 -> List LDrawLine -> List ConnectionPoint -> List ConnectionPoint
extractRecursive lineResolver depth currentMat lines acc =
    if depth > 4 then
        acc

    else
        List.foldl
            (\line currentAcc ->
                case line of
                    SubFileRef sub ->
                        let
                            comb =
                                Mat4.mul currentMat sub.transform
                        in
                        case classifySubfile sub of
                            Just ( connType, norm, tang ) ->
                                { id = List.length currentAcc + 1
                                , connType = connType
                                , localPosition = Mat4.transform comb (vec3 0 0 0)
                                , normal = transformDirection comb norm
                                , tangent = transformDirection comb tang
                                }
                                    :: currentAcc

                            Nothing ->
                                case lineResolver (String.toLower sub.file) of
                                    Just subLines ->
                                        extractRecursive lineResolver (depth + 1) comb subLines currentAcc

                                    Nothing ->
                                        currentAcc

                    _ ->
                        currentAcc
            )
            acc
            lines


inferBounds : List ConnectionPoint -> BoundingBox3D
inferBounds points =
    case points of
        [] ->
            { min = vec3 -10 -4 -10, max = vec3 10 24 10 }

        first :: rest ->
            let
                folder pt ( minV, maxV ) =
                    let
                        p =
                            pt.localPosition
                    in
                    ( vec3 (min (Vec3.getX minV) (Vec3.getX p)) (min (Vec3.getY minV) (Vec3.getY p)) (min (Vec3.getZ minV) (Vec3.getZ p))
                    , vec3 (max (Vec3.getX maxV) (Vec3.getX p)) (max (Vec3.getY maxV) (Vec3.getY p)) (max (Vec3.getZ maxV) (Vec3.getZ p))
                    )

                ( minP, maxP ) =
                    List.foldl folder ( first.localPosition, first.localPosition ) rest
            in
            { min = vec3 (Vec3.getX minP - 10) -4 (Vec3.getZ minP - 10)
            , max = vec3 (Vec3.getX maxP + 10) 24 (Vec3.getZ maxP + 10)
            }


{-| Whether a part has any stud (top stud) connection points. Studded parts
sit on the stud lattice; Technic parts (gears, beams, axles, pins, bushes) have
none.
-}
isStudded : PartConnectivity -> Bool
isStudded conn =
    List.any isStudPoint conn.connections


isStudPoint : ConnectionPoint -> Bool
isStudPoint point =
    case point.connType of
        Stud ->
            True

        _ ->
            False


{-| The X and Z offset a studded part's origin needs to sit on the stud lattice,
where studs are at 10 mod 20 (LDU) on both axes:

  - `10` on an axis along which the part, turned by `matrix`, has an odd number
    of studs (a 1×1 or a 1×3 must sit with its origin at ±10, a 2×4 at 0 mod 20);
  - `0` otherwise, and always for a part without stud points (Technic parts keep
    their own 20 LDU pitch, see `isStudded`).

Only the rotation of `matrix` counts: its translation is ignored. A part's stud
columns are the distinct rotated stud X (or Z) positions, so a 2 × 1 turned 90°
has its offset swapped.

    studOffset (extractPartConnectivity "3005.dat" (\_ -> Nothing)) Mat4.identity
        == { x = 10, z = 10 }

-}
studOffset : PartConnectivity -> Mat4 -> { x : Float, z : Float }
studOffset conn matrix =
    let
        origin =
            Mat4.transform matrix (vec3 0 0 0)

        studs =
            conn.connections
                |> List.filter isStudPoint
                |> List.map (\point -> Vec3.sub (Mat4.transform matrix point.localPosition) origin)

        offsetAlong get =
            let
                columns =
                    studs
                        |> List.map (\v -> round (get v))
                        |> Set.fromList
                        |> Set.size
            in
            if modBy 2 columns == 1 then
                10

            else
                0
    in
    { x = offsetAlong Vec3.getX, z = offsetAlong Vec3.getZ }


{-| Pre-defined connection layouts for common standard Lego parts.
-}
knownCatalogPart : String -> Maybe PartConnectivity
knownCatalogPart name =
    let
        clean =
            String.toLower (String.replace ".dat" "" (String.replace "parts/" "" name))
    in
    case clean of
        "60592" ->
            -- Window 1 x 2 x 2 without sill: 2 x 1 studs on top, 2 bricks (48 LDU) tall,
            -- with the underside anti-studs that seat it on the studs below
            Just (makeBrick 2 1 48 "60592.dat")

        "3001" ->
            -- Brick 2 x 4
            Just (makeBrick 4 2 24 "3001.dat")

        "3002" ->
            -- Brick 2 x 3
            Just (makeBrick 3 2 24 "3002.dat")

        "3003" ->
            -- Brick 2 x 2
            Just (makeBrick 2 2 24 "3003.dat")

        "3003b" ->
            Just (makeBrick 2 2 24 "3003b.dat")

        "3004" ->
            -- Brick 1 x 2
            Just (makeBrick 2 1 24 "3004.dat")

        "3004b" ->
            Just (makeBrick 2 1 24 "3004b.dat")

        "3005" ->
            -- Brick 1 x 1
            Just (makeBrick 1 1 24 "3005.dat")

        "3005b" ->
            Just (makeBrick 1 1 24 "3005b.dat")

        "3006" ->
            -- Brick 2 x 5
            Just (makeBrick 5 2 24 "3006.dat")

        "3007" ->
            -- Brick 2 x 8
            Just (makeBrick 8 2 24 "3007.dat")

        "3008" ->
            -- Brick 1 x 8
            Just (makeBrick 8 1 24 "3008.dat")

        "3009" ->
            -- Brick 1 x 6
            Just (makeBrick 6 1 24 "3009.dat")

        "3010" ->
            -- Brick 1 x 4
            Just (makeBrick 4 1 24 "3010.dat")

        "3622" ->
            -- Brick 1 x 3
            Just (makeBrick 3 1 24 "3622.dat")

        "3020" ->
            -- Plate 2 x 4
            Just (makeBrick 4 2 8 "3020.dat")

        "3021" ->
            -- Plate 2 x 3
            Just (makeBrick 3 2 8 "3021.dat")

        "3022" ->
            -- Plate 2 x 2
            Just (makeBrick 2 2 8 "3022.dat")

        "3023" ->
            -- Plate 1 x 2
            Just (makeBrick 2 1 8 "3023.dat")

        "3023b" ->
            Just (makeBrick 2 1 8 "3023b.dat")

        "3024" ->
            -- Plate 1 x 1
            Just (makeBrick 1 1 8 "3024.dat")

        "3034" ->
            -- Plate 2 x 8
            Just (makeBrick 8 2 8 "3034.dat")

        "3460" ->
            -- Plate 1 x 8
            Just (makeBrick 8 1 8 "3460.dat")

        "3666" ->
            -- Plate 1 x 6
            Just (makeBrick 6 1 8 "3666.dat")

        "3710" ->
            -- Plate 1 x 4
            Just (makeBrick 4 1 8 "3710.dat")

        "3623" ->
            -- Plate 1 x 3
            Just (makeBrick 3 1 8 "3623.dat")

        "3700" ->
            Just (makeTechnicBrick 2 "3700.dat")

        "3701" ->
            Just (makeTechnicBrick 4 "3701.dat")

        "3702" ->
            Just (makeTechnicBrick 8 "3702.dat")

        "3703" ->
            Just (makeTechnicBrick 16 "3703.dat")

        "32523" ->
            Just (makeLiftarm 3 "32523.dat")

        "32316" ->
            Just (makeLiftarm 5 "32316.dat")

        "32524" ->
            Just (makeLiftarm 7 "32524.dat")

        "32449" ->
            Just (makeLiftarm 9 "32449.dat")

        "40490" ->
            Just (makeLiftarm 9 "40490.dat")

        "32525" ->
            Just (makeLiftarm 11 "32525.dat")

        "32009" ->
            Just (makeLiftarm 15 "32009.dat")

        "32278" ->
            Just (makeLiftarm 15 "32278.dat")

        "3647" ->
            Just (makeGear 10.0 20.0 "3647.dat")

        "10928" ->
            Just (makeGear 10.0 20.0 "10928.dat")

        "11955" ->
            Just (makeGear 10.0 20.0 "11955.dat")

        "4019" ->
            Just (makeGear 20.0 20.0 "4019.dat")

        "3648" ->
            let
                base =
                    makeGear 30.0 20.0 "3648.dat"

                pinHoles =
                    makeGearPinHoles 10
                        [ ( 10, 10 )
                        , ( -10, 10 )
                        , ( -10, -10 )
                        , ( 10, -10 )
                        ]
            in
            Just { base | connections = base.connections ++ pinHoles }

        "3648b" ->
            let
                base =
                    makeGear 30.0 20.0 "3648b.dat"

                pinHoles =
                    makeGearPinHoles 10
                        [ ( 10, 10 )
                        , ( -10, 10 )
                        , ( -10, -10 )
                        , ( 10, -10 )
                        ]
            in
            Just { base | connections = base.connections ++ pinHoles }

        "3649" ->
            let
                base =
                    makeGear 50.0 20.0 "3649.dat"

                extraHoles =
                    [ { id = 2
                      , connType = TechnicAxleFemale { length = 20.0 }
                      , localPosition = vec3 0 20 0
                      , normal = vec3 0 0 1
                      , tangent = vec3 1 0 0
                      }
                    , { id = 3
                      , connType = TechnicAxleFemale { length = 20.0 }
                      , localPosition = vec3 0 -20 0
                      , normal = vec3 0 0 1
                      , tangent = vec3 1 0 0
                      }
                    , { id = 4
                      , connType = TechnicAxleFemale { length = 20.0 }
                      , localPosition = vec3 20 0 0
                      , normal = vec3 0 0 1
                      , tangent = vec3 1 0 0
                      }
                    , { id = 5
                      , connType = TechnicAxleFemale { length = 20.0 }
                      , localPosition = vec3 -20 0 0
                      , normal = vec3 0 0 1
                      , tangent = vec3 1 0 0
                      }
                    ]

                pinHoles =
                    makeGearPinHoles 10
                        [ ( 10, 10 )
                        , ( -10, 10 )
                        , ( -10, -10 )
                        , ( 10, -10 )
                        , ( 10, 30 )
                        , ( -10, 30 )
                        , ( 10, -30 )
                        , ( -10, -30 )
                        , ( 30, 10 )
                        , ( -30, 10 )
                        , ( 30, -10 )
                        , ( -30, -10 )
                        ]
            in
            Just { base | connections = base.connections ++ extraHoles ++ pinHoles }

        "32198" ->
            Just (makeGear 25.0 20.0 "32198.dat")

        "32269" ->
            Just (makeGear 25.0 20.0 "32269.dat")

        "6589" ->
            Just (makeGear 15.0 20.0 "6589.dat")

        "32270" ->
            Just (makeGear 15.0 20.0 "32270.dat")

        "4143" ->
            Just (makeGear 17.5 20.0 "4143.dat")

        "3650b" ->
            Just (makeGear 30.0 20.0 "3650b.dat")

        "3650" ->
            Just (makeGear 30.0 20.0 "3650.dat")

        "4716" ->
            Just (makeGear 12.0 40.0 "4716.dat")

        "32905" ->
            Just (makeGear 12.0 40.0 "32905.dat")

        "3704" ->
            Just (makeAxle 2 "3704.dat")

        "4519" ->
            Just (makeAxle 3 "4519.dat")

        "3705" ->
            Just (makeAxle 4 "3705.dat")

        "32073" ->
            Just (makeAxle 5 "32073.dat")

        "3706" ->
            Just (makeAxle 6 "3706.dat")

        "44294" ->
            Just (makeAxle 7 "44294.dat")

        "3707" ->
            Just (makeAxle 8 "3707.dat")

        "60485" ->
            Just (makeAxle 9 "60485.dat")

        "3737" ->
            Just (makeAxle 10 "3737.dat")

        "3708" ->
            Just (makeAxle 12 "3708.dat")

        "32062" ->
            Just (makeAxle 2 "32062.dat")

        "6587" ->
            Just (makeAxle 3 "6587.dat")

        "3713" ->
            Just (makeGear 9.0 20.0 "3713.dat")

        "4265c" ->
            Just (makeGear 9.0 10.0 "4265c.dat")

        "32123" ->
            Just (makeGear 9.0 10.0 "4265c.dat")

        "32123a" ->
            Just (makeGear 9.0 10.0 "4265c.dat")

        "2780" ->
            -- Technic Pin with Friction
            Just
                { part = "2780.dat"
                , connections =
                    [ { id = 1
                      , connType = TechnicPinMale { length = 10.0, friction = True }
                      , localPosition = vec3 0 0 10
                      , normal = vec3 0 0 1
                      , tangent = vec3 1 0 0
                      }
                    , { id = 2
                      , connType = TechnicPinMale { length = 10.0, friction = True }
                      , localPosition = vec3 0 0 -10
                      , normal = vec3 0 0 -1
                      , tangent = vec3 1 0 0
                      }
                    ]
                , boundingBoxes =
                    [ { min = vec3 -5 -5 -10, max = vec3 5 5 10 } ]
                }

        "4085" ->
            Just (makeClipPlate "4085.dat")

        "4085c" ->
            Just (makeClipPlate "4085c.dat")

        "30374" ->
            -- Bar 4L
            Just
                { part = "30374.dat"
                , connections =
                    [ { id = 1
                      , connType = BarFemale { diameter = 4.0 }
                      , localPosition = vec3 0 40 0
                      , normal = vec3 0 1 0
                      , tangent = vec3 1 0 0
                      }
                    ]
                , boundingBoxes =
                    [ { min = vec3 -4 0 -4, max = vec3 4 80 4 } ]
                }

        "14417" ->
            -- Plate 1 x 2 with Ball
            let
                base =
                    makeBrick 2 1 8 "14417.dat"

                ball =
                    { id = 50
                    , connType = BallMale
                    , localPosition = vec3 0 4 -20
                    , normal = vec3 0 0 -1
                    , tangent = vec3 1 0 0
                    }
            in
            Just { base | connections = ball :: base.connections }

        "14418" ->
            -- Plate 1 x 2 with Ball Socket
            let
                base =
                    makeBrick 2 1 8 "14418.dat"

                socket =
                    { id = 50
                    , connType = BallSocketFemale
                    , localPosition = vec3 0 4 -20
                    , normal = vec3 0 0 -1
                    , tangent = vec3 1 0 0
                    }
            in
            Just { base | connections = socket :: base.connections }

        _ ->
            Nothing


makeClipPlate : String -> PartConnectivity
makeClipPlate pName =
    let
        base =
            makeBrick 1 1 8 pName

        clip =
            { id = 20
            , connType = ClipMale { width = 4.0 }
            , localPosition = vec3 0 4 -10
            , normal = vec3 0 0 -1
            , tangent = vec3 1 0 0
            }
    in
    { base | connections = clip :: base.connections }


makeBrick : Int -> Int -> Float -> String -> PartConnectivity
makeBrick length width height partName =
    let
        halfL =
            toFloat length * 10.0

        halfW =
            toFloat width * 10.0

        xCoords =
            List.map (\i -> -halfL + 10.0 + toFloat i * 20.0) (List.range 0 (length - 1))

        zCoords =
            List.map (\j -> -halfW + 10.0 + toFloat j * 20.0) (List.range 0 (width - 1))

        studPositions =
            List.concatMap (\x -> List.map (\z -> ( x, z )) zCoords) xCoords

        studPoints =
            List.indexedMap
                (\idx ( x, z ) ->
                    { id = idx + 1
                    , connType = Stud
                    , localPosition = vec3 x 0 z
                    , normal = vec3 0 -1 0
                    , tangent = vec3 1 0 0
                    }
                )
                studPositions

        antiStudPoints =
            List.indexedMap
                (\idx ( x, z ) ->
                    { id = idx + 100
                    , connType = AntiStud
                    , localPosition = vec3 x height z
                    , normal = vec3 0 1 0
                    , tangent = vec3 1 0 0
                    }
                )
                studPositions

        allConnections =
            studPoints ++ antiStudPoints

        bbox =
            { min = vec3 -halfL -4 -halfW
            , max = vec3 halfL height halfW
            }
    in
    { part = partName
    , connections = allConnections
    , boundingBoxes = [ bbox ]
    }


{-| Procedurally generate connectivity descriptor for a Technic axle of given length in studs.
Each stud center along the axle has a connection point.
-}
makeAxle : Int -> String -> PartConnectivity
makeAxle studs partName =
    let
        halfL =
            toFloat studs * 10.0

        totalLen =
            toFloat studs * 20.0

        connections =
            List.range 0 (studs - 1)
                |> List.map
                    (\i ->
                        let
                            x =
                                -10.0 * toFloat studs + 10.0 + toFloat i * 20.0
                        in
                        { id = i + 1
                        , connType = TechnicAxleMale { length = totalLen }
                        , localPosition = vec3 x 0 0
                        , normal = vec3 1 0 0
                        , tangent = vec3 0 1 0
                        }
                    )

        bbox =
            { min = vec3 -halfL -4 -4
            , max = vec3 halfL 4 4
            }
    in
    { part = partName
    , connections = connections
    , boundingBoxes = [ bbox ]
    }


{-| Procedurally generate connectivity descriptor for a Technic gear with given pitch radius and thickness.
Center axle hole at (0, 0, 0).
-}
makeGear : Float -> Float -> String -> PartConnectivity
makeGear radius thickness partName =
    let
        halfThick =
            thickness / 2.0

        connections =
            [ { id = 1
              , connType = TechnicAxleFemale { length = thickness }
              , localPosition = vec3 0 0 0
              , normal = vec3 0 0 1
              , tangent = vec3 1 0 0
              }
            ]

        bbox =
            { min = vec3 -radius -radius -halfThick
            , max = vec3 radius radius halfThick
            }
    in
    { part = partName
    , connections = connections
    , boundingBoxes = [ bbox ]
    }


{-| Procedurally generate connection points for pin holes in a gear given 2D (x, y) coordinates.
Generates center (z=0), front entrance (z=10), and back entrance (z=-10) points for each hole.
-}
makeGearPinHoles : Int -> List ( Float, Float ) -> List ConnectionPoint
makeGearPinHoles startId coords =
    coords
        |> List.indexedMap
            (\i ( x, y ) ->
                let
                    baseId =
                        startId + i * 3
                in
                [ { id = baseId
                  , connType = TechnicPinFemale { length = 20.0 }
                  , localPosition = vec3 x y 0
                  , normal = vec3 0 0 1
                  , tangent = vec3 1 0 0
                  }
                , { id = baseId + 1
                  , connType = TechnicPinFemale { length = 20.0 }
                  , localPosition = vec3 x y 10
                  , normal = vec3 0 0 1
                  , tangent = vec3 1 0 0
                  }
                , { id = baseId + 2
                  , connType = TechnicPinFemale { length = 20.0 }
                  , localPosition = vec3 x y -10
                  , normal = vec3 0 0 -1
                  , tangent = vec3 1 0 0
                  }
                ]
            )
        |> List.concat


makeTechnicBrick : Int -> String -> PartConnectivity
makeTechnicBrick studs partName =
    let
        base =
            makeBrick studs 1 24 partName

        holePoints =
            List.range 0 (studs - 2)
                |> List.concatMap
                    (\i ->
                        let
                            x =
                                -10.0 * toFloat (studs - 2) + toFloat i * 20.0

                            baseId =
                                200 + i * 3
                        in
                        [ { id = baseId
                          , connType = TechnicPinFemale { length = 20.0 }
                          , localPosition = vec3 x 10 0
                          , normal = vec3 0 0 1
                          , tangent = vec3 1 0 0
                          }
                        , { id = baseId + 1
                          , connType = TechnicPinFemale { length = 20.0 }
                          , localPosition = vec3 x 10 10
                          , normal = vec3 0 0 1
                          , tangent = vec3 1 0 0
                          }
                        , { id = baseId + 2
                          , connType = TechnicPinFemale { length = 20.0 }
                          , localPosition = vec3 x 10 -10
                          , normal = vec3 0 0 -1
                          , tangent = vec3 1 0 0
                          }
                        ]
                    )
    in
    { base | connections = base.connections ++ holePoints }


makeLiftarm : Int -> String -> PartConnectivity
makeLiftarm studs partName =
    let
        halfL =
            toFloat studs * 10.0

        connections =
            List.range 0 (studs - 1)
                |> List.concatMap
                    (\i ->
                        let
                            z =
                                -10.0 * toFloat (studs - 1) + toFloat i * 20.0

                            baseId =
                                i * 3 + 1
                        in
                        [ { id = baseId
                          , connType = TechnicPinFemale { length = 20.0 }
                          , localPosition = vec3 0 0 z
                          , normal = vec3 0 1 0
                          , tangent = vec3 0 0 1
                          }
                        , { id = baseId + 1
                          , connType = TechnicPinFemale { length = 20.0 }
                          , localPosition = vec3 0 10 z
                          , normal = vec3 0 1 0
                          , tangent = vec3 0 0 1
                          }
                        , { id = baseId + 2
                          , connType = TechnicPinFemale { length = 20.0 }
                          , localPosition = vec3 0 -10 z
                          , normal = vec3 0 -1 0
                          , tangent = vec3 0 0 1
                          }
                        ]
                    )

        bbox =
            { min = vec3 -10 -10 -halfL
            , max = vec3 10 10 halfL
            }
    in
    { part = partName
    , connections = connections
    , boundingBoxes = [ bbox ]
    }
