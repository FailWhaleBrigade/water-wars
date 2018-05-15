module WaterWars.Core.GameMap where

import           ClassyPrelude
import           WaterWars.Core.Terrain.Block
import           Data.Array.IArray
import           WaterWars.Core.GameState


data GameMap = GameMap
  { gameTerrain :: Terrain
  , gamePlayers :: Seq Player -- TODO: what is this for
  , terrainBackground :: String -- TODO: how to send info about background?
  }
  deriving (Show, Read, Eq)


-- |Terrain description of theBlockId
newtype Terrain = Terrain
  { terrainBlocks :: Array BlockLocation Block
  }
  deriving (Show, Read, Eq)

terrainBounds :: Terrain -> (BlockLocation, BlockLocation)
terrainBounds Terrain{..} = bounds terrainBlocks

blockAt :: Terrain -> BlockLocation -> Block
blockAt Terrain{..} l = terrainBlocks ! l

instance Semigroup Terrain where
    Terrain blocks1 <> Terrain blocks2 =
        Terrain (accum useSolidBlock blocks1 $ assocs blocks2)
        where
            useSolidBlock :: Block -> Block -> Block
            useSolidBlock (SolidBlock x) _ = SolidBlock x
            useSolidBlock _ y = y

newtype BlockLocation = BlockLocation (Int, Int)
  deriving (Read, Show, Eq, Ord, Ix)
