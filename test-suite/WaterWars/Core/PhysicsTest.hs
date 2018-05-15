module WaterWars.Core.PhysicsTest where

import           Test.Hspec
import           ClassyPrelude
import           WaterWars.Core.Physics.Collision
import           WaterWars.Core.GameState
import           WaterWars.Core.GameMap
import           WaterWars.Core.DefaultGame
import           Data.Array.IArray
import           WaterWars.Core.Terrain.Block


physicsTests :: Spec
physicsTests = parallel $ describe "collision tests" collisionTests

smallBounds :: (BlockLocation, BlockLocation)
smallBounds = (BlockLocation (-1, -1), BlockLocation (1, 1))

terrainEmpty :: Terrain
terrainEmpty = Terrain $ listArray smallBounds $ replicate 9 NoBlock

terrainWithBlockAt :: BlockLocation -> Terrain
terrainWithBlockAt location = Terrain $ accumArray
    (flip const)
    NoBlock
    smallBounds
    [(location, SolidBlock Middle)]

collisionTests :: Spec
collisionTests = do
    collisionBlockFind
    collisionTargetLocation

collisionBlockFind :: Spec
collisionBlockFind = describe "find collision block" $ do
    it "should be no colliding block"
        $ collidingBlock terrainEmpty (Location (0, 0)) (VelocityVector 0 0)
        `shouldBe` Nothing
    it "should collide with to block"
        $          collidingBlock (terrainWithBlockAt $ BlockLocation (0, 1))
                                  (Location (0, 0.3))
                                  (VelocityVector 0 0.5)
        `shouldBe` Just (BlockLocation (0, 1))
    it "should collide with bottom block"
        $          collidingBlock (terrainWithBlockAt $ BlockLocation (0, -1))
                                  (Location (0, -0.3))
                                  (VelocityVector 0 (-0.5))
        `shouldBe` Just (BlockLocation (0, -1))
    it "should collide with right block"
        $          collidingBlock (terrainWithBlockAt $ BlockLocation (1, 0))
                                  (Location (0.3, 0))
                                  (VelocityVector 0.5 0)
        `shouldBe` Just (BlockLocation (1, 0))
    it "should collide with left block"
        $          collidingBlock (terrainWithBlockAt $ BlockLocation (-1, 0))
                                  (Location (-0.3, 0))
                                  (VelocityVector (-0.5) 0)
        `shouldBe` Just (BlockLocation (-1, 0))
    it "should collide with top right corner"
        $          collidingBlock (terrainWithBlockAt $ BlockLocation (1, 1))
                                  (Location (0.3, 0.3))
                                  (VelocityVector 0.5 0.5)
        `shouldBe` Just (BlockLocation (1, 1))
    it "should collide with top left corner"
        $          collidingBlock (terrainWithBlockAt $ BlockLocation (-1, 1))
                                  (Location (-0.3, 0.3))
                                  (VelocityVector (-0.5) 0.5)
        `shouldBe` Just (BlockLocation (-1, 1))
    it "should collide with bottom right corner"
        $          collidingBlock (terrainWithBlockAt $ BlockLocation (1, -1))
                                  (Location (0.3, -0.3))
                                  (VelocityVector 0.5 (-0.5))
        `shouldBe` Just (BlockLocation (1, -1))
    it "should collide with bottom left corner"
        $          collidingBlock (terrainWithBlockAt $ BlockLocation (-1, -1))
                                  (Location (-0.3, -0.3))
                                  (VelocityVector (-0.5) (-0.5))
        `shouldBe` Just (BlockLocation (-1, -1))

    mapM_ testForObservedGlitch  observedGlitches
    mapM_ testForObservedGlitch2 observedGlitches2

testForObservedGlitch :: (Location, VelocityVector, BlockLocation) -> Spec
testForObservedGlitch config@(l, v, _) =
    it ("should not not collide : " ++ show config)
        $          collidingBlock defaultTerrain l v
        `shouldBe` Nothing

testForObservedGlitch2 :: (Location, VelocityVector, Location) -> Spec
testForObservedGlitch2 config@(l, v, _) =
    it ("should not not enter block : " ++ show config)
        $          collidingBlock defaultTerrain l v
        `shouldBe` Nothing -- TODO: make correct assertion

observedGlitches :: [(Location, VelocityVector, BlockLocation)]
observedGlitches =
    [ ( Location (0.48279497, -5.4049997)
      , VelocityVector 4.62041e-2 (-0.23499991)
      , BlockLocation (0, -6)
      )
    ]

observedGlitches2 :: [(Location, VelocityVector, Location)]
observedGlitches2 =
    [ ( Location (7.498761, -4.307909)
      , VelocityVector 8.644776e-3 (-0.1949999)
      , Location (7.507277, -4.5)
      )
    , ( Location (-2.0688167, -2.5549994)
      , VelocityVector (-0.105503686) 0.105000064
      , Location (-2.1240797, -2.5)
      )
    , ( Location (-7.498761, -4.4562984)
      , VelocityVector (-8.644776e-3) (-7.499994e-2)
      , Location (-7.5037985, -4.5)
      )
    ]

collisionTargetLocation :: Spec
collisionTargetLocation = describe "find location after collision with" $ do
    it "should collide with top block"
        $          collideWithBlock (Location (0, 0.3))
                                    (VelocityVector 0 0.5)
                                    (BlockLocation (0, 1))
        `shouldBe` (Location (0, 0.5), VelocityVector 0 0)
    it "should collide with bottom block"
        $          collideWithBlock (Location (0, -0.3))
                                    (VelocityVector 0 (-0.5))
                                    (BlockLocation (0, -1))
        `shouldBe` (Location (0, -0.5), VelocityVector 0 0)
    it "should collide with right block"
        $          collideWithBlock (Location (0.3, 0))
                                    (VelocityVector 0.5 0)
                                    (BlockLocation (1, 0))
        `shouldBe` (Location (0.49, 0), VelocityVector 0 0)
    it "should collide with left block"
        $          collideWithBlock (Location (-0.3, 0))
                                    (VelocityVector (-0.5) 0)
                                    (BlockLocation (-1, 0))
        `shouldBe` (Location (-0.49, 0), VelocityVector 0 0)
    it "should collide with top right corner up"
        $          collideWithBlock (Location (0.3, 0.4))
                                    (VelocityVector 0.5 0.5)
                                    (BlockLocation (1, 1))
        `shouldBe` (Location (0.49, 0.6), VelocityVector 0 0.5)
    it "should collide with top left corner up"
        $          collideWithBlock (Location (-0.3, 0.4))
                                    (VelocityVector (-0.5) 0.5)
                                    (BlockLocation (-1, 1))
        `shouldBe` (Location (-0.49, 0.6), VelocityVector 0 0.5)
    it "should collide with top right corner down"
        $          collideWithBlock (Location (0.4, 0.3))
                                    (VelocityVector 0.5 0.5)
                                    (BlockLocation (1, 1))
        `shouldBe` (Location (0.6, 0.5), VelocityVector 0.5 0)
    it "should collide with top left corner down"
        $          collideWithBlock (Location (-0.4, 0.3))
                                    (VelocityVector (-0.5) 0.5)
                                    (BlockLocation (-1, 1))
        `shouldBe` (Location (-0.6, 0.5), VelocityVector (-0.5) 0)
    it "should collide with bottom right corner down"
        $          collideWithBlock (Location (0.3, -0.4))
                                    (VelocityVector 0.5 (-0.5))
                                    (BlockLocation (1, -1))
        `shouldBe` (Location (0.49, -0.6), VelocityVector 0 (-0.5))
    it "should collide with bottom left corner down"
        $          collideWithBlock (Location (-0.3, -0.4))
                                    (VelocityVector (-0.5) (-0.5))
                                    (BlockLocation (-1, -1))
        `shouldBe` (Location (-0.49, -0.6), VelocityVector 0 (-0.5))
    it "should collide with bottom right corner up"
        $          collideWithBlock (Location (0.4, -0.3))
                                    (VelocityVector 0.5 (-0.5))
                                    (BlockLocation (1, -1))
        `shouldBe` (Location (0.6, -0.5), VelocityVector 0.5 0)
    it "should collide with bottom left corner up"
        $          collideWithBlock (Location (-0.4, -0.3))
                                    (VelocityVector (-0.5) (-0.5))
                                    (BlockLocation (-1, -1))
        `shouldBe` (Location (-0.6, -0.5), VelocityVector (-0.5) 0)
