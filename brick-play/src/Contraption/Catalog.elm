module Contraption.Catalog exposing
    ( Category(..)
    , PartItem
    , categoryLabel
    , curatedParts
    , fromKitItem
    , partsForCategory
    , partsForCategoryWithProject
    )

{-| Curated catalog of Technic parts, offered in Technic mode (see `Main`): its
gears, beams, axles and pins, in tabs by category. Every entry is unlimited. A
URL kit adds a `ProjectCategory` tab in front of them, listing the kit's parts
(`fromKitItem`), also unlimited.
-}

import Studio.UrlPayload exposing (PartKitItem)


{-| Curated part item definition.
-}
type alias PartItem =
    { name : String
    , file : String
    , color : Int
    , category : Category
    , description : String
    }


{-| Technic part category tabs.
-}
type Category
    = ProjectCategory
    | GearsCategory
    | BeamsCategory
    | AxlesCategory
    | PinsCategory


{-| Human-readable label for a category.
-}
categoryLabel : Category -> String
categoryLabel category =
    case category of
        ProjectCategory ->
            "Project Parts"

        GearsCategory ->
            "Gears"

        BeamsCategory ->
            "Beams"

        AxlesCategory ->
            "Axles"

        PinsCategory ->
            "Pins & Bushings"


{-| The curated list of Technic parts.
-}
curatedParts : List PartItem
curatedParts =
    [ -- Gears
      { name = "8T Gear"
      , file = "3647.dat"
      , color = 7
      , category = GearsCategory
      , description = "8-tooth spur gear"
      }
    , { name = "16T Gear"
      , file = "4019.dat"
      , color = 7
      , category = GearsCategory
      , description = "16-tooth spur gear"
      }
    , { name = "24T Gear"
      , file = "3648.dat"
      , color = 7
      , category = GearsCategory
      , description = "24-tooth spur gear"
      }
    , { name = "40T Gear"
      , file = "3649.dat"
      , color = 7
      , category = GearsCategory
      , description = "40-tooth spur gear"
      }

    -- Beams
    , { name = "Beam 1x5"
      , file = "32316.dat"
      , color = 1
      , category = BeamsCategory
      , description = "1x5 Technic liftarm"
      }
    , { name = "Beam 1x7"
      , file = "32524.dat"
      , color = 1
      , category = BeamsCategory
      , description = "1x7 Technic liftarm"
      }
    , { name = "Beam 1x9"
      , file = "40490.dat"
      , color = 1
      , category = BeamsCategory
      , description = "1x9 Technic liftarm"
      }

    -- Axles
    , { name = "Axle 2"
      , file = "3704.dat"
      , color = 4
      , category = AxlesCategory
      , description = "Length 2 axle (Red)"
      }
    , { name = "Axle 4"
      , file = "3705.dat"
      , color = 0
      , category = AxlesCategory
      , description = "Length 4 axle (Black)"
      }
    , { name = "Axle 6"
      , file = "3706.dat"
      , color = 0
      , category = AxlesCategory
      , description = "Length 6 axle (Black)"
      }
    , { name = "Axle 8"
      , file = "3707.dat"
      , color = 0
      , category = AxlesCategory
      , description = "Length 8 axle (Black)"
      }

    -- Pins & Bushings
    , { name = "Bushing"
      , file = "6590.dat"
      , color = 7
      , category = PinsCategory
      , description = "Technic half/full bush"
      }
    , { name = "Pin with Friction"
      , file = "2780.dat"
      , color = 0
      , category = PinsCategory
      , description = "Technic connector pin"
      }
    ]


{-| Filter curated parts by category. The project category has no curated parts.
-}
partsForCategory : Category -> List PartItem
partsForCategory category =
    List.filter (\p -> p.category == category) curatedParts


{-| A kit item as a catalog part in `ProjectCategory`. The kit's count is not
carried: a project part is unlimited.
-}
fromKitItem : PartKitItem -> PartItem
fromKitItem item =
    { name = String.replace ".dat" "" item.part
    , file = item.part
    , color = item.color
    , category = ProjectCategory
    , description = "Project kit part"
    }


{-| The parts a dock tab lists: the project parts for `ProjectCategory`, else the
curated parts of the category.
-}
partsForCategoryWithProject : List PartItem -> Category -> List PartItem
partsForCategoryWithProject projectParts category =
    case category of
        ProjectCategory ->
            projectParts

        _ ->
            partsForCategory category
