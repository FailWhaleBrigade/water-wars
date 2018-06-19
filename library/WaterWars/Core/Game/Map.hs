module WaterWars.Core.Game.Map
    ( module WaterWars.Core.Game.Map
    , module WaterWars.Core.Terrain.Block
    , module WaterWars.Core.Terrain.Decoration
    )
where

import           ClassyPrelude
import           WaterWars.Core.Terrain.Block
import           Data.Array.IArray
import           WaterWars.Core.Terrain.Decoration

data GameMap = GameMap
    { gameTerrain :: Terrain
    , terrainDecoration :: TerrainDecoration
    }
    deriving (Show, Read, Eq)

-- |Terrain description of theBlockId
newtype Terrain = Terrain
    { terrainBlocks :: Array BlockLocation Block
    }
    deriving (Show, Read, Eq)

newtype TerrainDecoration = TerrainDecoration
    { terrainDecorationArray :: Array BlockLocation [Decoration]
    }
    deriving (Eq, Read, Show)

terrainBounds :: Terrain -> (BlockLocation, BlockLocation)
terrainBounds Terrain {..} = bounds terrainBlocks

blockAt :: Terrain -> BlockLocation -> Block
blockAt Terrain {..} l = terrainBlocks ! l

instance Semigroup Terrain where
    Terrain blocks1 <> Terrain blocks2 =
        Terrain (accum useSolidBlock blocks1 $ assocs blocks2)
        where
            useSolidBlock :: Block -> Block -> Block
            useSolidBlock (SolidBlock x) _ = SolidBlock x
            useSolidBlock _ y = y

newtype BlockLocation = BlockLocation (Int, Int)
  deriving (Read, Show, Eq, Ord, Ix)
