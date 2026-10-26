port module Ports exposing
    ( cancelSnapshot
    , captureRender
    , fileContentReceived
    , fileLoadError
    , geometryFlattenFailed
    , geometryFlattened
    , requestFileUpload
    , requestGeometryFlatten
    , runtimeEvent
    , saveSnapshot
    , setUrlHash
    , snapshotFailed
    , snapshotProgress
    , snapshotReady
    , viewportResized
    )

{-| JavaScript interop for file upload and URL hash state.

The JS side listens for `requestFileUpload`, opens a hidden file picker, reads
the selected file with `FileReader.readAsText`, then sends the text back via
`fileContentReceived`.

-}

import Json.Encode


{-| Send to JS to open the file picker dialog.
-}
port requestFileUpload : () -> Cmd msg


{-| Receive from JS when a file has been read. The string is the raw text
content of the selected `.ldr` or `.mpd` file.
-}
port fileContentReceived : (String -> msg) -> Sub msg


{-| Receive from JS when file validation or reading fails.
-}
port fileLoadError : (String -> msg) -> Sub msg


{-| Ask JS worker to flatten geometry. Payload is JSON string.
-}
port requestGeometryFlatten : String -> Cmd msg


{-| Receive flattened geometry JSON string from worker.
-}
port geometryFlattened : (String -> msg) -> Sub msg


{-| Receive worker flatten failure details.
-}
port geometryFlattenFailed : (String -> msg) -> Sub msg


{-| Emit runtime lifecycle events as JSON strings (for wrapper integrations).
-}
port runtimeEvent : String -> Cmd msg


{-| Receive viewport dimensions from host runtime (used by Web Components).
-}
port viewportResized : ({ width : Int, height : Int } -> msg) -> Sub msg


{-| Update browser URL hash from Elm app state.
-}
port setUrlHash : String -> Cmd msg


{-| Ask the offline path tracer for a 1920 × 1080 render. The value is the
request built by `Viewport.snapshotRequest`: the camera and every visible triangle.
-}
port captureRender : Json.Encode.Value -> Cmd msg


{-| The snapshot modal was closed while the render was running: stop the path tracer.
-}
port cancelSnapshot : () -> Cmd msg


{-| Path tracer progress: the fraction done and the samples taken so far.
-}
port snapshotProgress : ({ fraction : Float, samples : Int } -> msg) -> Sub msg


{-| The path tracer failed; the string is a message for the user.
-}
port snapshotFailed : (String -> msg) -> Sub msg


{-| Receive when high-resolution render capture is ready for preview.
-}
port snapshotReady : ({ dataUrl : String, filename : String } -> msg) -> Sub msg


{-| Request file download of the approved snapshot.
-}
port saveSnapshot : { dataUrl : String, filename : String } -> Cmd msg
