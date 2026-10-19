port module Ports exposing
    ( fileContentReceived
    , fileLoadError
    , geometryFlattenFailed
    , geometryFlattened
    , requestFileUpload
    , requestGeometryFlatten
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
