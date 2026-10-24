module Studio.BuildGroups exposing
    ( BuildGroup
    , Shelving
    , Tier(..)
    , all
    , classify
    , defaultGroup
    , find
    , isClutter
    , notClassic
    , sizeKey
    )

{-| The palette's build groups: a LEGO Creator–style shelf of the parts people
reach for when building, with Technic last.

The LDraw library files parts by `!CATEGORY`, which is a librarian's view: its
biggest categories are Minifig, Sticker and Sticker Shortcut, and a plain "Brick
2 x 4" sits among Duplo, patterned prints and one-off moulds. A build group picks
out the everyday parts instead and lists a curated set of classics first (1 x 1,
1 x 2 … 2 x 4), then the rest of the group by size.

Printed, stickered, Duplo and similar variants are `isClutter`: they are left
out of every group and ranked last in search, though a search still finds them.

`classify` works this out once per part, when the catalogue is decoded; the
palette filters and sorts twenty thousand parts on every render and must not
redo it there.

-}

import Dict exposing (Dict)


{-| One build group. `essentials` are part files listed first, in this order.
-}
type alias BuildGroup =
    { id : String
    , label : String
    , categories : List String
    , essentials : List String
    }


{-| Every build group, in the order the palette shows them.
-}
all : List BuildGroup
all =
    [ { id = "bricks"
      , label = "Bricks"
      , categories = [ "Brick" ]
      , essentials =
            [ "3005", "3004", "3622", "3010", "3009", "3008", "6111", "6112" ]
                ++ [ "3003", "3002", "3001", "2456", "3007", "3006" ]
                ++ [ "2357", "14716", "3245c", "22886", "3754", "2453b", "30136" ]
      }
    , { id = "plates"
      , label = "Plates"
      , categories = [ "Plate" ]
      , essentials =
            [ "3024", "3023b", "3623", "3710", "3666", "3460", "4477", "60479" ]
                ++ [ "3022", "3021", "3020", "3795", "3034", "3832" ]
                ++ [ "3031", "3032", "3035", "3030", "3029", "3958", "3036", "3033", "3027", "3026" ]
      }
    , { id = "tiles"
      , label = "Tiles"
      , categories = [ "Tile" ]
      , essentials =
            [ "3070b", "3069b", "63864", "2431", "6636", "4162", "3068b", "26603", "87079", "6934" ]
                ++ [ "98138", "14769", "2412b" ]
      }
    , { id = "slopes"
      , label = "Slopes"
      , categories = [ "Slope" ]
      , essentials =
            [ "54200", "85984", "3040b", "3039", "3038", "3037", "4445", "4286", "3298", "3297" ]
                ++ [ "3044b", "3043", "3665b", "3660b", "3747b", "4460b" ]
                ++ [ "11477", "15068", "50950", "61678", "93273" ]
      }
    , { id = "round"
      , label = "Round"
      , categories = [ "Cylinder", "Cone", "Dish", "Sphere" ]
      , essentials =
            [ "3062b", "85861", "98138", "4032b", "3941", "6143", "6222" ]
                ++ [ "4589", "3942c", "3943b", "4740", "3960" ]
      }
    , { id = "arches"
      , label = "Arches"
      , categories = [ "Arch" ]
      , essentials = [ "4490", "3659", "3455", "6005", "6182", "3307", "3308a", "92950", "15254", "14707" ]
      }
    , { id = "windows"
      , label = "Windows & doors"
      , categories = [ "Window", "Door", "Glass", "Windscreen" ]
      , essentials = [ "60592", "60601", "60593", "60602", "60594", "3853", "60596", "60623", "30179" ]
      }
    , { id = "walls"
      , label = "Walls & fences"
      , categories = [ "Panel", "Fence", "Ladder", "Staircase", "Support" ]
      , essentials = [ "87552", "87544", "4215b", "2362b", "30413", "3633", "3185", "30055" ]
      }
    , { id = "connectors"
      , label = "Brackets & hinges"
      , categories = [ "Bracket", "Hinge", "Bar", "Turntable" ]
      , essentials =
            [ "87087", "11211", "30414", "4070", "99781", "99780", "44728", "2436b" ]
                ++ [ "3794b", "15573", "3937", "3938", "2429", "2430", "4276b", "4275b", "4081b", "4085b", "2540" ]
      }
    , { id = "wedges"
      , label = "Wedges & wings"
      , categories = [ "Wedge", "Wing" ]
      , essentials = [ "2450", "43722b", "43723b", "41769b", "41770b", "51739", "41767", "41768", "41747", "41748" ]
      }
    , { id = "wheels"
      , label = "Wheels"
      , categories = [ "Wheel", "Tyre" ]
      , essentials = [ "4600", "4488", "4624", "3641", "30027b", "6015", "55981", "56891" ]
      }
    , { id = "nature"
      , label = "Nature"
      , categories = [ "Plant", "Animal", "Rock" ]
      , essentials = [ "3742", "32607", "2423", "2417", "6148", "3470" ]
      }
    , { id = "baseplates"
      , label = "Baseplates"
      , categories = [ "Baseplate" ]
      , essentials = [ "3811", "3867", "3857", "4186", "3497" ]
      }
    , { id = "technic"
      , label = "Technic"
      , categories = [ "Technic", "Electric" ]
      , essentials =
            [ "3700", "3701", "3894", "3702", "2730", "3703", "32523", "32316", "32524", "40490", "32278" ]
                ++ [ "4519", "3705", "32073", "3706", "44294", "3707", "32062", "2780", "43093", "6558", "3713", "32123a" ]
                ++ [ "3647", "69778", "4019", "94925", "3648b", "3649", "6589", "32270", "32269", "32498", "4716", "32072" ]
      }
    ]


{-| The group the palette opens on.
-}
defaultGroup : String
defaultGroup =
    "bricks"


{-| The group with this id.
-}
find : String -> Maybe BuildGroup
find id =
    List.filter (\group -> group.id == id) all |> List.head


{-| How a part sorts in search: everyday building parts, then the rest (Technic,
minifigs, …), then clutter.
-}
type Tier
    = Basic
    | Other
    | Clutter


{-| Where a part is shelved: the groups it is in, its search tier, its place
among the classics of each group that curates it (group id, position), and its
place among all classics in group order (Bricks first), for search ties.
-}
type alias Shelving =
    { groups : List String
    , tier : Tier
    , classics : List ( String, Int )
    , classicOrder : Int
    }


{-| Shelve a part: in a group if it is in one of the group's categories or is
curated into it, and not clutter.
-}
classify : { a | file : String, description : String, category : String } -> Shelving
classify part =
    if isClutter part then
        { groups = [], tier = Clutter, classics = [], classicOrder = notClassic }

    else
        let
            classics =
                Dict.get (stem part.file) classicsByFile |> Maybe.withDefault []

            groups =
                (Dict.get part.category groupsByCategory |> Maybe.withDefault [])
                    ++ List.map Tuple.first classics
                    |> dedupe
        in
        { groups = groups
        , tier =
            if List.any (\id -> id /= "technic") groups then
                Basic

            else
                Other
        , classics = classics
        , classicOrder = Dict.get (stem part.file) classicOrderByFile |> Maybe.withDefault notClassic
        }


{-| Sorts after every classic.
-}
notClassic : Int
notClassic =
    1000000


{-| Each curated part's first place in the whole curated list, built once.
-}
classicOrderByFile : Dict String Int
classicOrderByFile =
    all
        |> List.concatMap .essentials
        |> List.indexedMap (\i file -> ( file, i ))
        |> List.foldr (\( file, i ) -> Dict.insert file i) Dict.empty


{-| Group ids by LDraw category, built once.
-}
groupsByCategory : Dict String (List String)
groupsByCategory =
    List.foldr
        (\group acc ->
            List.foldl (\category -> Dict.update category (\ids -> Just (group.id :: Maybe.withDefault [] ids))) acc group.categories
        )
        Dict.empty
        all


{-| Each curated part's groups and place in each, built once.
-}
classicsByFile : Dict String (List ( String, Int ))
classicsByFile =
    all
        |> List.concatMap (\group -> List.indexedMap (\i file -> ( file, ( group.id, i ) )) group.essentials)
        |> List.foldr (\( file, place ) -> Dict.update file (\places -> Just (place :: Maybe.withDefault [] places))) Dict.empty


dedupe : List String -> List String
dedupe ids =
    List.foldr
        (\id acc ->
            if List.member id acc then
                acc

            else
                id :: acc
        )
        []
        ids


{-| Prints, stickers, Duplo, and other systems that do not build with System
bricks. Found by search, never shelved.

Other systems are recognised by name anywhere in the description ("Wheel …
Znap"), and third-party parts by LDraw's `|` description prefix.

-}
isClutter : { a | description : String, category : String } -> Bool
isClutter part =
    let
        lower =
            String.toLower part.description
    in
    List.member part.category clutterCategories
        || List.any (\word -> String.contains word lower) clutterWords
        || String.startsWith "|" part.description
        || List.any (\word -> List.member word otherSystems) (String.words part.description)


clutterCategories : List String
clutterCategories =
    [ "Sticker", "Sticker Shortcut", "Duplo", "Modulex", "Constraction", "Constraction Accessory", "Scala", "Znap", "Clikits", "Belville", "Homemaker" ]


{-| Lower-case. "(Needs Work)" is an LDraw quality flag, not a system, so it is
not here.
-}
clutterWords : List String
clutterWords =
    [ " pattern", " sticker" ]


otherSystems : List String
otherSystems =
    [ "Duplo", "Modulex", "Quatro", "Primo", "Fabuland", "Znap", "Mursten", "Scala", "Clikits", "Belville" ]


{-| The dimensions in a description ("Brick 1 x 2 x 3" gives `[ 1, 2, 3 ]`), so
smaller parts sort first. Parts with no dimensions sort after those with them.
-}
sizeKey : String -> List Float
sizeKey description =
    let
        dims =
            description
                |> String.words
                |> dimensionRun
    in
    if List.isEmpty dims then
        [ 1.0e9 ]

    else
        dims


{-| The first run of numbers joined by "x" in a list of words.
-}
dimensionRun : List String -> List Float
dimensionRun words =
    case words of
        first :: "x" :: rest ->
            case String.toFloat first of
                Just n ->
                    n :: continueRun rest

                Nothing ->
                    dimensionRun ("x" :: rest)

        _ :: rest ->
            dimensionRun rest

        [] ->
            []


continueRun : List String -> List Float
continueRun words =
    case words of
        next :: "x" :: rest ->
            case String.toFloat next of
                Just n ->
                    n :: continueRun rest

                Nothing ->
                    []

        next :: _ ->
            String.toFloat next |> Maybe.map List.singleton |> Maybe.withDefault []

        [] ->
            []


stem : String -> String
stem file =
    if String.endsWith ".dat" file then
        String.dropRight 4 file

    else
        file
