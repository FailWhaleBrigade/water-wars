module Main where
-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import           ClassyPrelude
import qualified Test.Tasty
import           Test.Tasty.Hspec
import           WaterWars.Core.PhysicsTest
import           WaterWars.Core.GeometryTest

main :: IO ()
main = do
    test <- testSpec "water-wars" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
    it "is trivially true" $
        True `shouldBe` True
    physicsTests
    geometryTests
