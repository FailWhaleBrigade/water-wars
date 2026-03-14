module WaterWars.Core.GameNgTest where

import           Test.Hspec

gameNgTests :: Spec
gameNgTests = parallel $ describe "single player move tests" moveTests

moveTests :: Spec
moveTests =
    return ()
