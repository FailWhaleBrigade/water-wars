{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WaterWars.Core.GameState where

import ClassyPrelude
import Data.Array.IArray

data GameInformation 
  = Map GameMap 
  | State GameState 
  deriving (Show, Read, Eq)

data GameMap = GameMap
  { gameTerrain :: Terrain
  , gamePlayers :: Seq Player
  }
  deriving (Show, Read, Eq)

-- |Terrain description of theBlockId
data Terrain = Terrain
  { terrainBlocks :: Array BlockLocation Block
  , terrainBackground :: String -- TODO: how to send info about background?
  }
  deriving (Show, Read, Eq)

-- |The content of a grid-cell in the map. This can be empty or a block
data Block
  = SolidBlock
  | NoBlock
  deriving (Show, Read, Eq)

-- |Master-state of the whole game
data GameState = GameState
  { gameEntities :: Entities
  , gameProjectiles :: Projectiles
  } deriving (Show, Read, Eq)

newtype Entities = Entities (Seq Entity)
  deriving (Read, Show, Eq)

data Entity = EntityPlayer InGamePlayer | Npc
  deriving (Show, Read, Eq)


data InGamePlayer = InGamePlayer
  { playerDesciption :: Player
  , playerLocation :: Location
  , playerMaxHealth :: Int
  , playerHealth :: Int
  , playerViewDirection :: Angle
  , playerMoveDirection :: Angle
  }
  deriving (Show, Read, Eq)


newtype Player = Player
  { playerId :: Text
  }
  deriving (Show, Read, Eq)

newtype Projectiles = Projectiles (Seq Projectile) deriving (Show, Eq, Read) 

data Projectile = Projectile
  { projectileLocation :: Location
  , projectileSpeed :: Speed
  , projectileDirection :: Angle
  }
  deriving (Show, Read, Eq)

newtype Location = Location (Float, Float)
  deriving (Show, Read, Eq)

newtype BlockLocation = BlockLocation (Int, Int)
  deriving (Read, Show, Eq, Ord, Ix)

newtype Angle = Angle Float
  deriving (Show, Read, Num, Eq)

newtype Speed = Speed Float
  deriving (Show, Read, Num, Eq)

-- TODO: module for the following
moveLocation :: (Speed, Angle) -> Location -> Location
moveLocation (Speed speed, Angle angle) (Location (x, y)) = Location
    (x + dx, y + dy)
  where
    dx = speed * cos angle
    dy = speed * sin angle




