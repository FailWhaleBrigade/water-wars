module Main where

-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
import Test.Tasty.Hspec
import WaterWars.Core.CollisionTest
import WaterWars.Core.GameUtilsTest
import WaterWars.Core.GeometryTest
import WaterWars.Network.ProtocolTest
import Test.Hspec
import Test.Tasty ( defaultMain, testGroup )

main :: IO ()
main = do
  test <- testSpec "water-wars" spec
  Test.Tasty.defaultMain
    ( testGroup
        "all"
        [ test
        , protocolTests
        ]
    )

spec :: Spec
spec = parallel $ do
  it "is trivially true" $
    True `shouldBe` True
  physicsTests
  geometryTests
  gameUtilsTest

-- TODO: fuzz tests on games / physics
