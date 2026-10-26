port module Ports exposing
    ( captureRender
    , copyToClipboard
    , geometryFlattenFailed
    , geometryFlattened
    , requestGeometryFlatten
    , requestThumbnails
    , setUrlHash
    , thumbnailLoaded
    )

{-| Ports for brick-play: the geometry flattening worker and the part thumbnail
pipeline (`initThumbnails` in `packages/brick-frame-viewport/runtime/thumbnail-pipeline.js`).
-}


{-| Ask JS to flatten geometry. Payload is JSON text.
-}
port requestGeometryFlatten : String -> Cmd msg


{-| Flattened geometry as JSON text.
-}
port geometryFlattened : (String -> msg) -> Sub msg


{-| The worker failed.
-}
port geometryFlattenFailed : (String -> msg) -> Sub msg


{-| Ask JS to render (or fetch from its cache) a thumbnail of each kit part in
its colour. `rgb` is the colour in 0–1, from the LDraw colour table.
-}
port requestThumbnails : List { file : String, color : Int, rgb : List Float } -> Cmd msg


{-| A rendered thumbnail as a data URL, with the part's .dat title (`""` when
unknown). Parts that cannot be rendered send nothing.
-}
port thumbnailLoaded : ({ file : String, color : Int, url : String, title : String } -> msg) -> Sub msg


{-| Update the browser URL hash.
-}
port setUrlHash : String -> Cmd msg


{-| Copy text to the system clipboard.
-}
port copyToClipboard : String -> Cmd msg


{-| Request an offscreen high-resolution render capture as a PNG download.
-}
port captureRender : { width : Int, height : Int, transparent : Bool, filename : String } -> Cmd msg
