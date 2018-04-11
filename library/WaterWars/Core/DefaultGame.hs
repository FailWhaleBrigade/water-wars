module WaterWars.Core.DefaultGame where

import ClassyPrelude
import WaterWars.Core.GameState
import Data.Array.IArray
import Data.List (cycle)

defaultGameMap :: GameMap
defaultGameMap = GameMap
  { gameTerrain = defaultTerrain
  , gamePlayers = singleton $ Player "Player One"
  }

defaultTerrain :: Terrain
defaultTerrain = Terrain
  { terrainBlocks = listArray (BlockLocation (-1, -1), BlockLocation (1, 1))
                              (take 9 $ cycle [NoBlock, SolidBlock, NoBlock])
  , terrainBackground = "Use Default Background"
  }
