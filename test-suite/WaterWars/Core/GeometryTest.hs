module WaterWars.Core.GeometryTest where

import           Test.Hspec
import           ClassyPrelude
import           WaterWars.Core.Physics.Geometry
import           WaterWars.Core.Game

geometryTests :: Spec
geometryTests = parallel $ do
    describe "intersection of lines"                  intersectionTests
    describe "intersection of line segments"          segmentIntersectionTests
    describe "normal intersections of line and point" normalIntersectionTests
    describe "check for traversals of blocks"         blockTraversalTest

intersectionTests :: Spec
intersectionTests = do
    it "identical lines should not intersect"
        $ intersectionOfLines (Line (Location (1, 1)) (VelocityVector 1 0))
                              (Line (Location (1, 1)) (VelocityVector 1 0))
        `shouldBe` Nothing
    it "parallel lines should not intersect"
        $          intersectionOfLines
                       (Line (Location (1, 1)) (VelocityVector 2 2))
                       (Line (Location (2, -3)) (VelocityVector (-1) (-1)))
        `shouldBe` Nothing
    it "should not give intersection if one line has null vector"
        $ intersectionOfLines (Line (Location (1, 1)) (VelocityVector 2 2))
                              (Line (Location (2, -3)) (VelocityVector 0 0))
        `shouldBe` Nothing
    it "simple crossing lines should give intersection point"
        $          intersectionOfLines
                       (Line (Location (1, 1)) (VelocityVector (-1) (-1)))
                       (Line (Location (-1, 1)) (VelocityVector 1 (-1)))
        `shouldBe` (Just $ Intersection (Location (0, 0)) 1 1)
    it "almost parallel lines should not intersect"
        $          intersectionOfLines
                       (Line (Location (1, 1)) (VelocityVector 1 1))
                       (Line (Location (2, 0)) (VelocityVector 1.0004 1))
        `shouldBe` Nothing

segmentIntersectionTests :: Spec
segmentIntersectionTests = do
    it "parallel lines should not intersect"
        $          intersectionOfSegments
                       (Line (Location (1, 1)) (VelocityVector 2 2))
                       (Line (Location (2, -3)) (VelocityVector (-1) (-1)))
        `shouldBe` Nothing
    it "intersection out of bounds should be ignored" $ do
        -- check that lines really do intersect
        intersectionOfLines
                (Line (Location (0, 0)) (VelocityVector 1 1))
                (Line (Location (3, 0)) (VelocityVector (-1) 1))
            `shouldBe` Just (Intersection (Location (1.5, 1.5)) 1.5 1.5)
        -- check that intersection is ignored
        intersectionOfSegments
                (Line (Location (0, 0)) (VelocityVector 1 1))
                (Line (Location (3, 0)) (VelocityVector (-1) 1))
            `shouldBe` Nothing
    it "intersection out of bounds of first segment should be ignored" $ do
        -- check that lines do intersect
        intersectionOfLines (Line (Location (0, 0)) (VelocityVector 1 1))
                            (Line (Location (0, 2)) (VelocityVector 4 0))
            `shouldBe` Just (Intersection (Location (2, 2)) 2 0.5)
        -- check that intersection is ignored
        intersectionOfSegments (Line (Location (0, 0)) (VelocityVector 1 1))
                               (Line (Location (0, 2)) (VelocityVector 4 0))
            `shouldBe` Nothing
    it "intersection out of bounds of second segment be ignored" $ do
        -- check that lines do intersect
        intersectionOfLines (Line (Location (0, 2)) (VelocityVector 4 0))
                            (Line (Location (0, 0)) (VelocityVector 1 1))
            `shouldBe` Just (Intersection (Location (2, 2)) 0.5 2)
        intersectionOfSegments (Line (Location (0, 2)) (VelocityVector 4 0))
                               (Line (Location (0, 0)) (VelocityVector 1 1))
            `shouldBe` Nothing
    it "intersection at end points of segments should be contained"
        $          intersectionOfSegments
                       (Line (Location (1, 1)) (VelocityVector 1 1))
                       (Line (Location (-3, 1)) (VelocityVector 4 0))
        `shouldBe` Just (Intersection (Location (1, 1)) 0 1)


normalIntersectionTests :: Spec
normalIntersectionTests = do
    it "normal intersection on diagonal line"
        $ intersectionLocation
        <$> normalIntersection (Line (Location (0, 0)) (VelocityVector 1 1))
                               (Location (2, 0))
        `shouldBe` Just (Location (1, 1))
    it "normal intersection with point on line"
        $ intersectionLocation
        <$> normalIntersection (Line (Location (0, 0)) (VelocityVector 1 1))
                               (Location (-1, -1))
        `shouldBe` Just (Location (-1, -1))


blockTraversalTest :: Spec
blockTraversalTest = do
    it "point entering block traverses block"
        $          traversesBlock (Line (Location (0, 0)) (VelocityVector 1 1))
                                  (BlockLocation (1, 1))
        `shouldBe` True
    it "should traverse center block if not leaving"
        $ traversesBlock (Line (Location (0, 0)) (VelocityVector 0.2 0.1))
                         (BlockLocation (0, 0))
        `shouldBe` True
    it "point away from block does not traverse block"
        $ traversesBlock (Line (Location (0, 0)) (VelocityVector (-1) 1))
                         (BlockLocation (1, 1))
        `shouldBe` False
    it "line at border of block should not traverse block" $ do
            -- does traverse block
        traversesBlock (Line (Location (0.6, 0)) (VelocityVector 0 1))
                       (BlockLocation (1, 1))
            `shouldBe` True
        -- does NOT traverse block
        traversesBlock (Line (Location (0.5, 0)) (VelocityVector 0 1))
                       (BlockLocation (1, 1))
            `shouldBe` False
    it "line at that goes through block and out should traverse block"
        $          traversesBlock
                       (Line (Location (0.3, 0.55)) (VelocityVector 1 (-1)))
                       (BlockLocation (0, 0))
        `shouldBe` True
    it "point at edge of block is no traversal"
        $ traversesBlock (Line (Location (0.5, 0)) (VelocityVector 0 0))
                         (BlockLocation (0, 0))
        `shouldBe` False
    it "point at corner of block is no traversal"
        $ traversesBlock (Line (Location (0.5, 0.5)) (VelocityVector 0 0))
                         (BlockLocation (0, 0))
        `shouldBe` False
    it "diagonal down on from edge of block through block is a traversal"
        $          traversesBlock
                       (Line (Location (0.2, 0.5)) (VelocityVector 0.4 (-0.3)))
                       (BlockLocation (0, 0))
        `shouldBe` True
