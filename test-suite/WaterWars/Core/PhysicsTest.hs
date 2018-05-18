{-# LANGUAGE TupleSections #-}

module WaterWars.Core.PhysicsTest where

import           Test.Hspec
import           ClassyPrelude
import           WaterWars.Core.Physics.Collision
import           WaterWars.Core.Game
import           WaterWars.Core.Terrains
import           Data.Array.IArray


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

terrainWithBlocksAt :: [BlockLocation] -> Terrain
terrainWithBlocksAt locations =
    Terrain $ accumArray (flip const) NoBlock smallBounds $ map
        (, SolidBlock Middle)
        locations

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
    it "should collide with top block when going top right"
        $          collidingBlock
                       (terrainWithBlocksAt [BlockLocation (0, 1), BlockLocation (1, 1)])
                       (Location (0.2, 0.4))
                       (VelocityVector 0.5 0.5)
        `shouldBe` Just (BlockLocation (0, 1))
    mapM_ testForObservedGhostCollide observedGhostCollide
    mapM_ testForObservedEnteredBlock observedEnteredBlocks

testForObservedGhostCollide
    :: (Terrain, Location, VelocityVector, Location, BlockLocation) -> Spec
testForObservedGhostCollide (t, l, v, l1, b) =
    it ("should not not collide : " ++ show (l, v, b, l1))
        $          collidingBlock t l v
        `shouldBe` Nothing

testForObservedEnteredBlock
    :: (Terrain, Location, VelocityVector, Location, BlockLocation) -> Spec
testForObservedEnteredBlock (t, l, v, l1, b) =
    it ("should not not enter block : " ++ show (l, v, l1, b))
        $          collidingBlock t l v
        `shouldBe` Just b

observedGhostCollide :: [(Terrain, Location, VelocityVector, Location, BlockLocation)]
observedGhostCollide = []

observedEnteredBlocks
    :: [(Terrain, Location, VelocityVector, Location, BlockLocation)]
observedEnteredBlocks =
    [ ( terrain2
      , Location (-7, 1.4990239)
      , VelocityVector 0 (-0.18999991)
      , moveLocation (VelocityVector 0 (-0.18999991)) $ Location (-7, 1.4990239)
      , BlockLocation (-7, 1)
      )
    ]



-- TODO: detailled tests for middle-blocks

collisionTargetLocation :: Spec
collisionTargetLocation = describe "find location after collision with" $ do
    it "should collide with top block"
        $          collideWithBlock (Location (0, 0.3))
                                    (VelocityVector 0 0.5)
                                    (BlockLocation (0, 1))
        `shouldBe` (Location (0, 0.498), VelocityVector 0 0)
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
        `shouldBe` (Location (0.598, 0.498), VelocityVector 0.5 0)
    it "should collide with top left corner down"
        $          collideWithBlock (Location (-0.4, 0.3))
                                    (VelocityVector (-0.5) 0.5)
                                    (BlockLocation (-1, 1))
        `shouldBe` (Location (-0.598, 0.498), VelocityVector (-0.5) 0)
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
