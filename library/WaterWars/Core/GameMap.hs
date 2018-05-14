module WaterWars.Core.GameMap where

import ClassyPrelude
import WaterWars.Core.Terrain.Block
import Data.Array.IArray
import WaterWars.Core.GameState


data GameMap = GameMap
  { gameTerrain :: Terrain
  , gamePlayers :: Seq Player -- TODO: what is this for?
  }
  deriving (Show, Read, Eq)


-- |Terrain description of theBlockId
data Terrain = Terrain
  { terrainBlocks :: Array BlockLocation Block
  , terrainBackground :: String -- TODO: how to send info about background?
  }
  deriving (Show, Read, Eq)


instance Semigroup Terrain where
    Terrain blocks1 _ <> Terrain blocks2 background =
        Terrain (accum useSolidBlock blocks1 $ assocs blocks2) background
        where
            useSolidBlock :: Block -> Block -> Block
            useSolidBlock (SolidBlock x) _ = SolidBlock x
            useSolidBlock _ y = y

newtype BlockLocation = BlockLocation (Int, Int)
  deriving (Read, Show, Eq, Ord, Ix)
