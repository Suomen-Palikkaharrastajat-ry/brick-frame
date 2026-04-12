module LDraw.Resolve exposing
    ( PartCache
    , PartStatus(..)
    , ResolverConfig
    , defaultResolverConfig
    , fetchPart
    , fetchPartPrimitive
    , initCache
    , markLoading
    , pendingParts
    , resolverConfig
    , seedFromMpd
    , topLevelPartUrl
    , updateCache
    )

{-| LDraw part library HTTP resolver.

Manages a session-scoped cache of fetched LDraw `.dat` files. Parts are
fetched from a configurable base directory and optional fallback.

Example bases:

  - `/ldraw` (local assets in Vite `public/ldraw`)
  - `https://raw.githubusercontent.com/gkjohnson/ldraw-parts-library/master/complete/ldraw`

## Directory structure on CDN

  - `parts/`      — top-level named parts (e.g. `3001.dat`, `3647.dat`)
  - `parts/s/`    — sub-parts (referenced as `s\stud4.dat` in LDraw files)
  - `p/`          — geometric primitives (e.g. `stud.dat`, `1-4ring3.dat`)
  - `p/48/`       — hi-res primitives (referenced as `48\stud.dat`)

For plain filenames the resolver tries `parts/<name>` and then `p/<name>`
across configured bases. Filenames with a path prefix (`s/`, `48/`) are
routed directly to `parts/` or `p/` as appropriate.

-}

import Dict exposing (Dict)
import Http
import LDraw.Parser as Parser
import LDraw.Types exposing (LDrawLine(..))
import Task exposing (Task)


-- ── Types ─────────────────────────────────────────────────────────────────────


{-| Loading state for a single part file.
-}
type PartStatus
    = Loading
    | Loaded (List LDrawLine)
    | Failed String


{-| Session-scoped cache. Keys are normalised filenames (lowercase, forward
slashes). Values track the loading state of each file.
-}
type alias PartCache =
    Dict String PartStatus


{-| Configures where part files are resolved from.

  - `primaryBase` is required and should point to a directory containing
    `parts/` and `p/` subdirectories (for example `/ldraw`).
  - `fallbackBase` is optional and is used when primary fetches fail.

-}
type alias ResolverConfig =
    { primaryBase : String
    , fallbackBase : Maybe String
    }


-- ── Public API ────────────────────────────────────────────────────────────────


{-| Empty cache — call once on app init. Can be pre-seeded with embedded parts.
-}
initCache : PartCache
initCache =
    Dict.empty


{-| Mark names as in-flight (`Loading`) so they are not re-queued.
-}
markLoading : List String -> PartCache -> PartCache
markLoading names cache =
    List.foldl
        (\name acc ->
            if Dict.member name acc then
                acc

            else
                Dict.insert name Loading acc
        )
        cache
        names


{-| Runtime default resolution strategy:

1. Local synced assets mounted by Vite at `/ldraw`
2. Remote fallback mirror

-}
defaultResolverConfig : ResolverConfig
defaultResolverConfig =
    { primaryBase = "/ldraw"
    , fallbackBase = Just "https://raw.githubusercontent.com/gkjohnson/ldraw-parts-library/master/complete/ldraw"
    }


{-| Build a normalised resolver configuration from environment values.
-}
resolverConfig : String -> Maybe String -> ResolverConfig
resolverConfig primary fallback =
    { primaryBase =
        if String.trim primary == "" then
            defaultResolverConfig.primaryBase

        else
            normalizeBase primary
    , fallbackBase =
        fallback
            |> Maybe.andThen
                (\value ->
                    if String.trim value == "" then
                        Nothing

                    else
                        Just (normalizeBase value)
                )
    }


{-| URL for a top-level model part using the primary base.

    topLevelPartUrl config "3647.dat"

-}
topLevelPartUrl : ResolverConfig -> String -> String
topLevelPartUrl config name =
    joinBase config.primaryBase ("parts/" ++ name)


{-| Pre-populate a cache from an MPD file's embedded sub-files.

    seedFromMpd mpdText existingCache

Splits the raw MPD text on `0 FILE <name>` markers, parses each section, and
inserts the results as `Loaded` entries. Existing cache entries are not
overwritten (MPD-embedded versions take lower priority than already-cached ones).

Returns the seeded cache and the name of the main (first) file so the caller
can extract its top-level lines.

-}
seedFromMpd : String -> PartCache -> ( PartCache, Maybe String )
seedFromMpd text cache =
    let
        sections =
            Parser.splitMpd text

        seeded =
            Dict.foldl
                (\name content acc ->
                    if Dict.member name acc then
                        acc

                    else
                        Dict.insert name (Loaded (Parser.parseFile content)) acc
                )
                cache
                sections

        mainFile =
            case Parser.firstMpdFileName text of
                Just first ->
                    if Dict.member first sections then
                        Just first

                    else
                        Dict.keys sections |> List.head

                Nothing ->
                    Dict.keys sections |> List.head
    in
    ( seeded, mainFile )


{-| Update the cache after a successful or failed fetch.
-}
updateCache : String -> Result Http.Error String -> (String -> List LDrawLine) -> PartCache -> PartCache
updateCache name result parseFn cache =
    case result of
        Ok text ->
            Dict.insert name (Loaded (parseFn text)) cache

        Err err ->
            Dict.insert name (Failed (httpErrorToString err)) cache


{-| Collect the names of all sub-files referenced in `lines` that are not yet
in the cache (not loaded, loading, or failed). These need to be fetched.
-}
pendingParts : List LDrawLine -> PartCache -> List String
pendingParts lines cache =
    lines
        |> List.filterMap subFileName
        |> List.filter (\name -> not (Dict.member name cache))
        |> deduplicate


{-| Fetch a single part by name. The `toMsg` callback wraps the result into
your application's `Msg` type.

    fetchPart config "3647.dat" (\name result -> PartLoaded name result)

-}
fetchPart : ResolverConfig -> String -> (String -> Result Http.Error String -> msg) -> Cmd msg
fetchPart config name toMsg =
    partCandidateUrls config name
        |> fetchFirst
        |> Task.attempt (toMsg name)


{-| Fetch a part from the primitives directory (`p/`).
Call this as a fallback when `fetchPart` returns a 404.
-}
fetchPartPrimitive : ResolverConfig -> String -> (String -> Result Http.Error String -> msg) -> Cmd msg
fetchPartPrimitive config name toMsg =
    primitiveCandidateUrls config name
        |> fetchFirst
        |> Task.attempt (toMsg name)


-- ── Internal ──────────────────────────────────────────────────────────────────


partCandidateUrls : ResolverConfig -> String -> List String
partCandidateUrls config name =
    let
        relativePaths =
            if String.startsWith "parts/" name || String.startsWith "p/" name then
                [ name ]

            else if String.startsWith "s/" name then
                [ "parts/" ++ name ]

            else if String.startsWith "48/" name || String.startsWith "8/" name then
                [ "p/" ++ name ]

            else if isLikelyPrimitiveName name then
                [ "p/" ++ name, "parts/" ++ name, "parts/s/" ++ name ]

            else
                [ "parts/" ++ name, "parts/s/" ++ name, "p/" ++ name ]
    in
    allBases config
        |> List.concatMap
            (\base ->
                relativePaths
                    |> List.map (\path -> joinBase base path)
            )


isLikelyPrimitiveName : String -> Bool
isLikelyPrimitiveName name =
    String.contains "-" name
        || startsWithDigit name
        || List.any
            (\prefix -> String.startsWith prefix name)
            [ "stud"
            , "ring"
            , "edge"
            , "cyli"
            , "cylo"
            , "cyl"
            , "disc"
            , "ndis"
            , "rin"
            , "con"
            , "chrd"
            , "tang"
            , "box"
            , "tooth"
            , "connect"
            , "peghole"
            ]


startsWithDigit : String -> Bool
startsWithDigit value =
    case String.uncons value of
        Just ( first, _ ) ->
            first >= '0' && first <= '9'

        Nothing ->
            False


primitiveCandidateUrls : ResolverConfig -> String -> List String
primitiveCandidateUrls config name =
    allBases config
        |> List.map (\base -> joinBase base ("p/" ++ name))


allBases : ResolverConfig -> List String
allBases config =
    case config.fallbackBase of
        Just fallback ->
            [ config.primaryBase, fallback ]

        Nothing ->
            [ config.primaryBase ]


normalizeBase : String -> String
normalizeBase base =
    base
        |> String.trim
        |> stripTrailingSlash


stripTrailingSlash : String -> String
stripTrailingSlash value =
    if String.endsWith "/" value then
        stripTrailingSlash (String.dropRight 1 value)

    else
        value


joinBase : String -> String -> String
joinBase base path =
    base ++ "/" ++ path


fetchFirst : List String -> Task Http.Error String
fetchFirst urls =
    case urls of
        [] ->
            Task.fail (Http.BadBody "No part URLs configured")

        url :: rest ->
            httpGet url
                |> Task.onError
                    (\err ->
                        case rest of
                            [] ->
                                Task.fail err

                            _ ->
                                fetchFirst rest
                    )


httpGet : String -> Task Http.Error String
httpGet url =
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver =
            Http.stringResolver
                (\response ->
                    case response of
                        Http.BadUrl_ badUrl ->
                            Err (Http.BadUrl badUrl)

                        Http.Timeout_ ->
                            Err Http.Timeout

                        Http.NetworkError_ ->
                            Err Http.NetworkError

                        Http.BadStatus_ metadata _ ->
                            Err (Http.BadStatus metadata.statusCode)

                        Http.GoodStatus_ _ body ->
                            if looksLikeLDraw body then
                                Ok body

                            else
                                Err (Http.BadBody "Response was not LDraw content")
                )
        , timeout = Nothing
        }


looksLikeLDraw : String -> Bool
looksLikeLDraw body =
    body
        |> String.lines
        |> List.filter (\line -> String.trim line /= "")
        |> List.head
        |> Maybe.map isLDrawLineStart
        |> Maybe.withDefault False


isLDrawLineStart : String -> Bool
isLDrawLineStart line =
    case String.words line |> List.head of
        Just token ->
            List.member token [ "0", "1", "2", "3", "4", "5" ]

        Nothing ->
            False


subFileName : LDrawLine -> Maybe String
subFileName line =
    case line of
        SubFileRef { file } ->
            Just file

        _ ->
            Nothing


deduplicate : List String -> List String
deduplicate =
    List.foldl
        (\x ( seen, acc ) ->
            if List.member x seen then
                ( seen, acc )

            else
                ( x :: seen, x :: acc )
        )
        ( [], [] )
        >> Tuple.second
        >> List.reverse


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus code ->
            "HTTP " ++ String.fromInt code

        Http.BadBody msg ->
            "Bad response body: " ++ msg
