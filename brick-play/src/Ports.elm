port module Ports exposing
    ( geometryFlattenFailed
    , geometryFlattened
    , requestGeometryFlatten
    )

{-| Ports for brick-play: geometry flattening worker interface.
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
