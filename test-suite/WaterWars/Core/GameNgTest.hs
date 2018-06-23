{-# LANGUAGE TupleSections #-}

module WaterWars.Core.GameNgTest where

import           Test.Hspec
import           ClassyPrelude
import           WaterWars.Core.Physics.Collision
import           WaterWars.Core.Game
import           WaterWars.Core.DefaultGame
import           WaterWars.Core.Terrains
import           Data.Array.IArray


gameNgTests :: Spec
gameNgTests = parallel $ describe "single player move tests" moveTests

smallBounds :: (BlockLocation, BlockLocation)
smallBounds = (BlockLocation (-2, -2), BlockLocation (2, 2))

terrainEmpty :: Terrain
terrainEmpty = Terrain $ listArray smallBounds $ replicate 25 NoBlock

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

moveTests :: Spec
moveTests = do
    return ()
