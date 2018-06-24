module WaterWars.Core.CollisionTest where

import           Test.Hspec
import           ClassyPrelude
import           WaterWars.Core.Physics.Collision
import           WaterWars.Core.Game
import           WaterWars.Core.TerrainsUtils
import           WaterWars.Core.Physics.Geometry


physicsTests :: Spec
physicsTests = parallel $ describe "collision tests" collisionTests

collisionTests :: Spec
collisionTests = do
    describe "check for traversal candidates" traversedBlocksCandidatesTest
    describe "check for traversed blocks"     traversedBlocksTest
    describe
        "check that correct block-borders are selected for intersection"
        possibleCollisionLinesTest
    describe "check for colliding blocks" collidingBlockTest
    describe "check that the velocityvector get truncated correctly"
             truncateVelocityTest
    describe "check for status after a collision" collideWithBlockTest

-- TODO: detailled tests for middle-blocks

traversedBlocksCandidatesTest :: Spec
traversedBlocksCandidatesTest = do
    it "no velocity results in only one traversal candidate"
        $          getTraversedBlocksCandidates
                       (Line (Location (0, 0)) (VelocityVector 0 0))
        `shouldBe` [BlockLocation (0, 0)]
    it "velocity to right results in two traversal candidate"
        $          getTraversedBlocksCandidates
                       (Line (Location (0, 0)) (VelocityVector 0 1))
        `shouldBe` [BlockLocation (0, 0), BlockLocation (0, 1)]
    it "velocity to bottom left results in four traversal candidate"
        $          getTraversedBlocksCandidates
                       (Line (Location (0, 0)) (VelocityVector (-1) (-1)))
        `shouldBe` [ BlockLocation (0, 0)
                   , BlockLocation (-1, 0)
                   , BlockLocation (0, -1)
                   , BlockLocation (-1, -1)
                   ]
    it "diagonal down on edge of block results in two traversals"
        $          getTraversedBlocksCandidates
                       (Line (Location (0.2, 0.5)) (VelocityVector 0.4 (-0.3)))
        `shouldBe` [ BlockLocation (0, 0)
                   , BlockLocation (1, 0)
                   , BlockLocation (0, -1)
                   , BlockLocation (1, -1)
                   ]

traversedBlocksTest :: Spec
traversedBlocksTest = do
    it "no velocity results in only one traversed block"
        $ getTraversedBlocks (Line (Location (0, 0)) (VelocityVector 0 0))
        `shouldBe` [BlockLocation (0, 0)]
    it "no velocity on edge results in only one traversed block"
        $ getTraversedBlocks (Line (Location (0.5, 0.5)) (VelocityVector 0 0))
        `shouldBe` []
    it "should traverse center block if not leaving"
        $ getTraversedBlocks (Line (Location (0, 0)) (VelocityVector 0.2 0.1))
        `shouldBe` [BlockLocation (0, 0)]
    it "should traverse only top block if moving out of center"
        $ getTraversedBlocks (Line (Location (0, 0.3)) (VelocityVector 0 0.5))
        `shouldBe` [BlockLocation (0, 1)]
    it "should traverse only top block if starting from edge"
        $ getTraversedBlocks (Line (Location (0, 0.5)) (VelocityVector 0.1 0.5))
        `shouldBe` [BlockLocation (0, 1)]
    it "should traverse bottom block"
        $          getTraversedBlocks
                       (Line (Location (0, -0.5)) (VelocityVector 0.1 (-0.5)))
        `shouldBe` [BlockLocation (0, -1)]
    it "should traverse right block"
        $ getTraversedBlocks (Line (Location (0.5, 0)) (VelocityVector 0.5 0.2))
        `shouldBe` [BlockLocation (1, 0)]
    it "should traverse left block"
        $          getTraversedBlocks
                       (Line (Location (-0.5, 0)) (VelocityVector (-0.5) (-0.2)))
        `shouldBe` [BlockLocation (-1, 0)]
    it "diagonal down on edge of block results in two traversals"
        $          getTraversedBlocks
                       (Line (Location (0.2, 0.5)) (VelocityVector 0.4 (-0.3)))
        `shouldBe` [BlockLocation (0, 0), BlockLocation (1, 0)]

collidingBlockTest :: Spec
collidingBlockTest = do
    it "empty terrain gives no colliding block"
        $          collidingBlock
                       terrainEmpty
                       (Line (Location (0.2, 0.2)) (VelocityVector 0.4 (-0.3)))
        `shouldBe` Nothing
    it "should collide with right block"
        $ collidingBlock (terrainWithBlockAt (1, 0))
                         (Line (Location (0.2, 0.2)) (VelocityVector 0.4 0))
        `shouldBe` Just (BlockLocation (1, 0))
    it "should collide with right block when going through"
        $          collidingBlock
                       (terrainWithBlockAt (1, 0))
                       (Line (Location (0.4, 0.2)) (VelocityVector 0.4 0.4))
        `shouldBe` Just (BlockLocation (1, 0))
    it "should collide with corner block"
        $          collidingBlock
                       (terrainWithBlockAt (1, 1))
                       (Line (Location (0.4, 0.2)) (VelocityVector 0.4 0.4))
        `shouldBe` Just (BlockLocation (1, 1))

possibleCollisionLinesTest :: Spec
possibleCollisionLinesTest = do
    it "straight to right should give only left block-line"
        $          getPossibleCollisionLines
                       (Line (Location (0.2, 0.2)) (VelocityVector 0.4 (-0.3)))
                       (BlockLocation (1, 0))
        `shouldBe` [blockLeftLine $ BlockLocation (1, 0)]
    it "straight to left should give only right block-line"
        $          getPossibleCollisionLines
                       (Line (Location (-1.4, 0.2)) (VelocityVector (-0.3) 0.1))
                       (BlockLocation (-2, 0))
        `shouldBe` [blockRightLine $ BlockLocation (-2, 0)]
    it "diagonal to block should give 2 candidates"
        $          getPossibleCollisionLines
                       (Line (Location (-1.4, 0.8)) (VelocityVector (-0.3) (-0.4)))
                       (BlockLocation (-2, 0))
        `shouldBe` [ blockTopLine $ BlockLocation (-2, 0)
                   , blockRightLine $ BlockLocation (-2, 0)
                   ]

truncateVelocityTest :: Spec
truncateVelocityTest = do
    let dummyBlock = BlockLocation (0, 0)
    let topLine    = lineVector $ blockTopLine dummyBlock
    let botLine    = lineVector $ blockBotLine dummyBlock
    let leftLine   = lineVector $ blockLeftLine dummyBlock
    it "should truncate y-velocity on top border line"
        $          truncateOnBorderLine (VelocityVector 0.1 0.2) topLine
        `shouldBe` VelocityVector 0.1 0
    it "should truncate y-velocity on bottom border line"
        $          truncateOnBorderLine (VelocityVector 0.1 0.2) botLine
        `shouldBe` VelocityVector 0.1 0
    it "should truncate x-velocity on left border line"
        $          truncateOnBorderLine (VelocityVector 0.1 0.2) leftLine
        `shouldBe` VelocityVector 0 0.2


-- TODO: redo tests
collideWithBlockTest :: Spec
collideWithBlockTest = do
    it "should stop at top-block border"
        $ collideWithBlock (Line (Location (0, 0.3)) (VelocityVector 0 0.3))
                           (BlockLocation (0, 1))
        `shouldBe` Just (Location (0, 0.5), VelocityVector 0 0)
    it "should collide with bottom block"
        $          collideWithBlock
                       (Line (Location (0, -0.3)) (VelocityVector 0 (-0.5)))
                       (BlockLocation (0, -1))
        `shouldBe` Just (Location (0, -0.5), VelocityVector 0 0)
    it "should collide with right block"
        $ collideWithBlock (Line (Location (0.3, 0)) (VelocityVector 0.5 0))
                           (BlockLocation (1, 0))
        `shouldBe` Just (Location (0.5, 0), VelocityVector 0 0)
    it "should collide with left block"
        $          collideWithBlock
                       (Line (Location (-0.3, 0)) (VelocityVector (-0.5) 0))
                       (BlockLocation (-1, 0))
        `shouldBe` Just (Location (-0.5, 0), VelocityVector 0 0)
    it "should collide with top right corner up"
        $          collideWithBlock
                       (Line (Location (0.3, 0.4)) (VelocityVector 0.5 0.5))
                       (BlockLocation (1, 1))
        `shouldBe` Just (Location (0.5, 0.6), VelocityVector 0 0.5)
    it "should collide with top left corner up"
        $          collideWithBlock
                       (Line (Location (-0.3, 0.4)) (VelocityVector (-0.5) 0.5))
                       (BlockLocation (-1, 1))
        `shouldBe` Just (Location (-0.5, 0.6), VelocityVector 0 0.5)
    it "should collide with top right corner down"
        $          collideWithBlock
                       (Line (Location (0.4, 0.3)) (VelocityVector 0.5 0.5))
                       (BlockLocation (1, 1))
        `shouldBe` Just (Location (0.6, 0.5), VelocityVector 0.5 0)
    it "should collide with top left corner down"
        $          collideWithBlock
                       (Line (Location (-0.4, 0.3)) (VelocityVector (-0.5) 0.5))
                       (BlockLocation (-1, 1))
        `shouldBe` Just (Location (-0.6, 0.5), VelocityVector (-0.5) 0)
    it "should collide with bottom right corner down"
        $          collideWithBlock
                       (Line (Location (0.3, -0.4)) (VelocityVector 0.5 (-0.5)))
                       (BlockLocation (1, -1))
        `shouldBe` Just (Location (0.5, -0.6), VelocityVector 0 (-0.5))
    it "should collide with bottom left corner down"
        $          collideWithBlock
                       (Line (Location (-0.3, -0.4)) (VelocityVector (-0.5) (-0.5)))
                       (BlockLocation (-1, -1))
        `shouldBe` Just (Location (-0.5, -0.6), VelocityVector 0 (-0.5))
    it "should collide with bottom right corner up"
        $          collideWithBlock
                       (Line (Location (0.4, -0.3)) (VelocityVector 0.5 (-0.5)))
                       (BlockLocation (1, -1))
        `shouldBe` Just (Location (0.6, -0.5), VelocityVector 0.5 0)
    it "should collide with bottom left corner up"
        $          collideWithBlock
                       (Line (Location (-0.4, -0.3)) (VelocityVector (-0.5) (-0.5)))
                       (BlockLocation (-1, -1))
        `shouldBe` Just (Location (-0.6, -0.5), VelocityVector (-0.5) 0)
