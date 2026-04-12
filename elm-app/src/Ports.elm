port module Ports exposing
    ( fileContentReceived
    , fileLoadError
    , geometryFlattenFailed
    , geometryFlattened
    , requestFileUpload
    , requestGeometryFlatten
    , setUrlHash
    )

{-| JavaScript interop for file upload and URL hash state.

The JS side listens for `requestFileUpload`, opens a hidden file picker, reads
the selected file with `FileReader.readAsText`, then sends the text back via
`fileContentReceived`.

-}


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


{-| Update browser URL hash from Elm app state.
-}
port setUrlHash : String -> Cmd msg
