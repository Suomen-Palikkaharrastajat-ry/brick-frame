module EmbeddedParts exposing (cache, lodCache)

{-| The part files compiled into the bundle, as ready-to-use part caches.

`Data` is generated from the LDraw library; this module only parses what it
embeds. Seeding a `Viewport` with `cache` means the gear parts and everything
they reference render without a network round trip.

-}

import Data
import Dict
import LDraw.Parser as Parser
import LDraw.Resolve as Resolve exposing (PartCache, PartStatus(..))


{-| Core gear parts and their transitive dependencies.
-}
cache : PartCache
cache =
    parse Data.embeddedParts


{-| Simplified (decimated) versions of the embedded gear parts, keyed by the
same part-file names as `cache`. Used to build the low-detail gear meshes
selected at a distance (see `Gear.Lod`).
-}
lodCache : PartCache
lodCache =
    parse Data.lodParts


parse : Dict.Dict String String -> PartCache
parse parts =
    Dict.foldl
        (\name text acc ->
            Dict.insert name (Loaded (Parser.parseFile text)) acc
        )
        Resolve.initCache
        parts
