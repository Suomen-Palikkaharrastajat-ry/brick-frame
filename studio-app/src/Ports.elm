port module Ports exposing
    ( cancelSnapshot
    , captureRender
    , copyToClipboard
    , downloadFile
    , fileContentReceived
    , fileLoadError
    , geometryFlattenFailed
    , geometryFlattened
    , requestFileUpload
    , requestGeometryFlatten
    , requestThumbnails
    , saveDraft
    , saveFavorites
    , saveRecents
    , saveRequested
    , saveSnapshot
    , setUrlHash
    , snapshotFailed
    , snapshotProgress
    , snapshotReady
    , thumbnailLoaded
    , viewportResized
    )

{-| JavaScript interop for the studio.

These are the ports `initBricksRuntime` expects of a host application (see
`docs/library-usage.md`): file picking and reading, the geometry worker, the URL
hash and viewport size. The studio adds its own ports next to them as it needs
them.

-}

import Json.Encode


{-| Send to JS to open the file picker dialog.
-}
port requestFileUpload : () -> Cmd msg


{-| Receive from JS when a file or URL has been read: the LDraw text.
-}
port fileContentReceived : (String -> msg) -> Sub msg


{-| Receive from JS when reading or validating a file fails.
-}
port fileLoadError : (String -> msg) -> Sub msg


{-| Ask JS to flatten geometry. Payload is JSON text.
-}
port requestGeometryFlatten : String -> Cmd msg


{-| Flattened geometry as JSON text.
-}
port geometryFlattened : (String -> msg) -> Sub msg


{-| The worker failed.
-}
port geometryFlattenFailed : (String -> msg) -> Sub msg


{-| Viewport dimensions measured by the host.
-}
port viewportResized : ({ width : Int, height : Int } -> msg) -> Sub msg


{-| Update the browser URL hash.
-}
port setUrlHash : String -> Cmd msg


{-| Hand the browser a file to save: a name, a media type and the text.
-}
port downloadFile : { filename : String, mime : String, text : String } -> Cmd msg


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


{-| Offer the current document to be remembered as a draft. The JavaScript side
debounces and stores it, and gives it back as the `draft` flag next time.
-}
port saveDraft : String -> Cmd msg


{-| The browser asked to save (Ctrl+S), which the page intercepts.
-}
port saveRequested : (() -> msg) -> Sub msg


{-| Copy text to the system clipboard.
-}
port copyToClipboard : String -> Cmd msg


{-| Save starred favorites list to local storage.
-}
port saveFavorites : List String -> Cmd msg


{-| Save recently used part history to local storage.
-}
port saveRecents : List String -> Cmd msg


{-| Request on-demand isometric thumbnail generation for parts in an active colour.
-}
port requestThumbnails : List { file : String, color : Int, rgb : List Float } -> Cmd msg


{-| Receive generated or cached thumbnail image data URL from IndexedDB.
-}
port thumbnailLoaded : ({ file : String, color : Int, url : String } -> msg) -> Sub msg
