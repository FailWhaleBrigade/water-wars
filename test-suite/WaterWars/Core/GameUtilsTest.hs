module WaterWars.Core.GameUtilsTest where

import           Test.Hspec
import           ClassyPrelude
import           WaterWars.Core.Game

gameUtilsTest :: Spec
gameUtilsTest =
    parallel $ describe "test minimum verctor utility" minimumVectorTest


minimumVectorTest :: Spec
minimumVectorTest = do
    it "should give zero-vector if no vectors are given"
        $          minimumVector []
        `shouldBe` VelocityVector 0 0
    it "should give least of every coordinate"
        $          minimumVector [VelocityVector (-1) 0, VelocityVector 0 1]
        `shouldBe` VelocityVector 0 0
