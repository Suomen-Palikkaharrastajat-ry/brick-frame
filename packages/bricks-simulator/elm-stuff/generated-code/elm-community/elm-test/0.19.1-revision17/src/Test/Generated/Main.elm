module Test.Generated.Main exposing (main)

import Gear.ComponentsTest
import Gear.DetectTest
import Gear.PhysicsTest
import LDraw.ColorsTest
import LDraw.GeometryTest
import LDraw.ParserTest
import PackageSmokeTest
import Render.StyleTest

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    Test.Runner.Node.run
        { runs = 100
        , report = ConsoleReport Monochrome
        , seed = 224645066583449
        , processes = 12
        , globs =
            []
        , paths =
            [ "/workspaces/bricks/packages/bricks-simulator/tests/Gear/ComponentsTest.elm"
            , "/workspaces/bricks/packages/bricks-simulator/tests/Gear/DetectTest.elm"
            , "/workspaces/bricks/packages/bricks-simulator/tests/Gear/PhysicsTest.elm"
            , "/workspaces/bricks/packages/bricks-simulator/tests/LDraw/ColorsTest.elm"
            , "/workspaces/bricks/packages/bricks-simulator/tests/LDraw/GeometryTest.elm"
            , "/workspaces/bricks/packages/bricks-simulator/tests/LDraw/ParserTest.elm"
            , "/workspaces/bricks/packages/bricks-simulator/tests/PackageSmokeTest.elm"
            , "/workspaces/bricks/packages/bricks-simulator/tests/Render/StyleTest.elm"
            ]
        }
        [ ( "Gear.ComponentsTest"
          , [ Test.Runner.Node.check Gear.ComponentsTest.suite
            ]
          )
        , ( "Gear.DetectTest"
          , [ Test.Runner.Node.check Gear.DetectTest.suite
            ]
          )
        , ( "Gear.PhysicsTest"
          , [ Test.Runner.Node.check Gear.PhysicsTest.suite
            ]
          )
        , ( "LDraw.ColorsTest"
          , [ Test.Runner.Node.check LDraw.ColorsTest.suite
            ]
          )
        , ( "LDraw.GeometryTest"
          , [ Test.Runner.Node.check LDraw.GeometryTest.suite
            ]
          )
        , ( "LDraw.ParserTest"
          , [ Test.Runner.Node.check LDraw.ParserTest.suite
            ]
          )
        , ( "PackageSmokeTest"
          , [ Test.Runner.Node.check PackageSmokeTest.suite
            ]
          )
        , ( "Render.StyleTest"
          , [ Test.Runner.Node.check Render.StyleTest.suite
            ]
          )
        ]