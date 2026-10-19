port module Ports exposing
    ( copyToClipboard
    , downloadFile
    , fileContentReceived
    , fileLoadError
    , geometryFlattenFailed
    , geometryFlattened
    , requestFileUpload
    , requestGeometryFlatten
    , saveDraft
    , saveRequested
    , setUrlHash
    , viewportResized
    )

{-| JavaScript interop for the studio.

These are the ports `initBricksRuntime` expects of a host application (see
`docs/library-usage.md`): file picking and reading, the geometry worker, the URL
hash and viewport size. The studio adds its own ports next to them as it needs
them.

-}


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
