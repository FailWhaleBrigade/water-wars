module WaterWars.Core.TerrainsUtils where

import           ClassyPrelude
import           WaterWars.Core.Game.Map
import           Data.Array.IArray

smallBounds :: (BlockLocation, BlockLocation)
smallBounds = (BlockLocation (-2, -2), BlockLocation (2, 2))

terrainEmpty :: Terrain
terrainEmpty = Terrain $ listArray smallBounds $ replicate 25 NoBlock

terrainWithBlockAt :: (Int, Int) -> Terrain
terrainWithBlockAt location = Terrain $ accumArray
    (flip const)
    NoBlock
    smallBounds
    [(BlockLocation location, SolidBlock Middle)]

terrainWithBlocksAt :: [(Int, Int)] -> Terrain
terrainWithBlocksAt locations =
    Terrain $ accumArray (flip const) NoBlock smallBounds $ map
        (\l -> (BlockLocation l, SolidBlock Middle))
        locations
