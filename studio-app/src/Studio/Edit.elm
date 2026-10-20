module Studio.Edit exposing
    ( Axis(..)
    , Command(..)
    , History
    , NewPart
    , apply
    , axisVector
    , canRedo
    , canUndo
    , emptyHistory
    , pushApplied
    , record
    , redo
    , selectionCenter
    , snap
    , undo
    )

{-| Editing a document: pure commands and an undo history.

A `Command` says what to do to a document. `apply` does it and also returns the
command that undoes it, so a history is just two stacks of commands and every
step — undo or redo — is `apply` again:

    ( edited, inverse ) =
        apply (Translate [ 3 ] delta) document

Commands name placements by id, never by position, so they survive other edits
being undone around them. Removing placements records where they were, so undo
puts them back in the same order with the same ids.

-}

import LDraw.Document as Document exposing (Document, Placement)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)


{-| An axis of the document's coordinate system (LDraw's, Y pointing down).
-}
type Axis
    = AxisX
    | AxisY
    | AxisZ


{-| A part to add: file name, colour and where it goes.
-}
type alias NewPart =
    { part : String
    , color : Int
    , matrix : Mat4
    }


{-| One edit.

  - `Translate` moves placements by a vector.
  - `Rotate` turns placements about an axis through `pivot`.
  - `Recolor` sets colours, as `( id, colour )` pairs.
  - `SetMatrix` sets whole matrices, as `( id, matrix )` pairs: what typing a
    position or angle into an inspector does.
  - `Add` appends new placements to the main submodel.
  - `Remove` deletes placements.
  - `Restore` puts placements back at entry indices; it is what undoing a
    `Remove` is, and is exposed because a history may hold it.
  - `Group` makes several commands one undo step.

-}
type Command
    = Translate (List Int) Vec3
    | Rotate (List Int) { pivot : Vec3, axis : Axis, radians : Float }
    | Recolor (List ( Int, Int ))
    | SetMatrix (List ( Int, Mat4 ))
    | Add (List NewPart)
    | AddIn String (List NewPart)
    | Remove (List Int)
    | Restore (List ( String, Int, Placement ))
    | GroupIntoSubmodel String (List Int)
    | ExplodeSubmodel Int
    | ReplaceDoc Document Document
    | Group (List Command)


{-| Do a command, and get the command that undoes it.
-}
apply : Command -> Document -> ( Document, Command )
apply command doc =
    case command of
        Translate ids delta ->
            ( transform ids (Mat4.mul (Mat4.makeTranslate delta)) doc
            , Translate ids (Vec3.negate delta)
            )

        Rotate ids { pivot, axis, radians } ->
            ( transform ids (Mat4.mul (rotationAbout pivot axis radians) >> clean) doc
            , Rotate ids { pivot = pivot, axis = axis, radians = -radians }
            )

        Recolor pairs ->
            let
                before =
                    pairs
                        |> List.filterMap
                            (\( id, _ ) ->
                                Document.getPlacement id doc |> Maybe.map (\p -> ( id, p.color ))
                            )
            in
            ( List.foldl
                (\( id, colour ) d -> Document.updatePlacement id (\p -> { p | color = colour }) d)
                doc
                pairs
            , Recolor before
            )

        SetMatrix pairs ->
            let
                before =
                    pairs
                        |> List.filterMap
                            (\( id, _ ) ->
                                Document.getPlacement id doc |> Maybe.map (\p -> ( id, p.matrix ))
                            )
            in
            ( List.foldl
                (\( id, matrix ) d -> Document.updatePlacement id (\p -> { p | matrix = matrix }) d)
                doc
                pairs
            , SetMatrix before
            )

        Add parts ->
            apply (AddIn "" parts) doc

        AddIn submodel parts ->
            let
                ( added, ids ) =
                    List.foldl
                        (\part ( d, acc ) ->
                            Document.addPlacement submodel part d
                                |> Tuple.mapSecond (\id -> acc ++ [ id ])
                        )
                        ( doc, [] )
                        parts
            in
            ( added, Remove ids )

        Remove ids ->
            let
                located =
                    ids
                        |> List.filterMap (\id -> Document.locatePlacement id doc)
                        |> List.sortBy (\( _, idx, _ ) -> idx)
            in
            ( List.foldl (\( _, _, p ) d -> Document.removePlacement p.id d) doc located
            , Restore located
            )

        Restore entries ->
            ( List.foldl (\( subName, index, p ) d -> Document.insertPlacementIn subName index p d) doc entries
            , Remove (List.map (\( _, _, p ) -> p.id) entries)
            )

        GroupIntoSubmodel name ids ->
            let
                grouped =
                    Document.groupIntoSubmodel name ids doc
            in
            ( grouped, ReplaceDoc grouped doc )

        ExplodeSubmodel id ->
            let
                exploded =
                    Document.explodeSubmodel id doc
            in
            ( exploded, ReplaceDoc exploded doc )

        ReplaceDoc _ target ->
            ( target, ReplaceDoc target doc )

        Group commands ->
            let
                ( final, inverses ) =
                    List.foldl
                        (\c ( d, acc ) ->
                            let
                                ( next, inverse ) =
                                    apply c d
                            in
                            ( next, inverse :: acc )
                        )
                        ( doc, [] )
                        commands
            in
            -- Inverses were collected last-first, which is the order to undo in.
            ( final, Group inverses )


transform : List Int -> (Mat4 -> Mat4) -> Document -> Document
transform ids change doc =
    List.foldl
        (\id d -> Document.updatePlacement id (\p -> { p | matrix = change p.matrix }) d)
        doc
        ids


rotationAbout : Vec3 -> Axis -> Float -> Mat4
rotationAbout pivot axis radians =
    Mat4.mul
        (Mat4.mul (Mat4.makeTranslate pivot) (Mat4.makeRotate radians (axisVector axis)))
        (Mat4.makeTranslate (Vec3.negate pivot))


{-| The unit vector of an axis.
-}
axisVector : Axis -> Vec3
axisVector axis =
    case axis of
        AxisX ->
            Vec3.vec3 1 0 0

        AxisY ->
            Vec3.vec3 0 1 0

        AxisZ ->
            Vec3.vec3 0 0 1


{-| Round matrix entries to the six decimals a document is written with, so
repeated 15° turns do not leave `0.9999999999` behind for the file to carry.
-}
clean : Mat4 -> Mat4
clean matrix =
    let
        m =
            Mat4.toRecord matrix

        r v =
            toFloat (round (v * 1000000)) / 1000000
    in
    Mat4.fromRecord
        { m11 = r m.m11
        , m21 = r m.m21
        , m31 = r m.m31
        , m41 = 0
        , m12 = r m.m12
        , m22 = r m.m22
        , m32 = r m.m32
        , m42 = 0
        , m13 = r m.m13
        , m23 = r m.m23
        , m33 = r m.m33
        , m43 = 0
        , m14 = r m.m14
        , m24 = r m.m24
        , m34 = r m.m34
        , m44 = 1
        }


{-| Round `value` to the nearest multiple of `step`.
-}
snap : Float -> Float -> Float
snap step value =
    if step <= 0 then
        value

    else
        toFloat (round (value / step)) * step


{-| The middle of the given placements, from their positions: the pivot for
turning a selection.
-}
selectionCenter : List Int -> Document -> Maybe Vec3
selectionCenter ids doc =
    let
        positions =
            ids
                |> List.filterMap (\id -> Document.getPlacement id doc)
                |> List.map (\p -> Mat4.transform p.matrix (Vec3.vec3 0 0 0))
    in
    case positions of
        [] ->
            Nothing

        _ ->
            Just (Vec3.scale (1 / toFloat (List.length positions)) (List.foldl Vec3.add (Vec3.vec3 0 0 0) positions))



-- ── History ───────────────────────────────────────────────────────────────────


{-| What can be undone and redone. Holds commands that, applied, do the undoing
(`undo`) or the redoing (`redo`).
-}
type alias History =
    { undo : List Command
    , redo : List Command
    }


emptyHistory : History
emptyHistory =
    { undo = [], redo = [] }


{-| How many steps are remembered.
-}
limit : Int
limit =
    200


{-| Do a command and remember how to undo it. A new command forgets the redo
stack.
-}
record : Command -> Document -> History -> ( Document, History )
record command doc history =
    let
        ( edited, inverse ) =
            apply command doc
    in
    ( edited, pushApplied inverse { history | redo = [] } )


{-| Remember how to undo something that has already been done, as a drag does
once it ends.
-}
pushApplied : Command -> History -> History
pushApplied inverse history =
    { undo = List.take limit (inverse :: history.undo), redo = [] }


canUndo : History -> Bool
canUndo history =
    not (List.isEmpty history.undo)


canRedo : History -> Bool
canRedo history =
    not (List.isEmpty history.redo)


{-| Step back. Nothing to undo leaves everything as it was.
-}
undo : Document -> History -> ( Document, History )
undo doc history =
    case history.undo of
        command :: rest ->
            let
                ( undone, redoCommand ) =
                    apply command doc
            in
            ( undone, { undo = rest, redo = redoCommand :: history.redo } )

        [] ->
            ( doc, history )


{-| Step forward again.
-}
redo : Document -> History -> ( Document, History )
redo doc history =
    case history.redo of
        command :: rest ->
            let
                ( redone, undoCommand ) =
                    apply command doc
            in
            ( redone, { undo = undoCommand :: history.undo, redo = rest } )

        [] ->
            ( doc, history )
