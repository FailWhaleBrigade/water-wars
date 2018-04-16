{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WaterWars.Core.GameState where

import ClassyPrelude
import Data.Array.IArray

data GameMap = GameMap
  { gameTerrain :: Terrain
  , gamePlayers :: Seq Player
  }
  deriving (Show, Read)

-- |Terrain description of theBlockId
data Terrain = Terrain
  { terrainBlocks :: Array BlockLocation Block
  , terrainBackground :: String -- TODO: how to send info about background?
  }
  deriving (Show, Read)

-- |The content of a grid-cell in the map. This can be empty or a block
data Block
  = SolidBlock
  | NoBlock
  deriving (Show, Read, Eq)

-- |Master-state of the whole game
data GameState = GameState
  { gameEntities :: Entities
  , gameProjectiles :: Projectiles
  }

newtype Entities = Entities (Seq Entity)
  deriving (Read, Show)

data Entity = EntityPlayer InGamePlayer | Npc
  deriving (Show, Read)


data InGamePlayer = InGamePlayer
  { playerDesciption :: Player
  , playerLocation :: Location
  , playerMaxHealth :: Int
  , playerHealth :: Int
  , playerViewDirection :: Angle
  , playerMoveDirection :: Angle
  }
  deriving (Show, Read)


newtype Player = Player
  { playerId :: Text
  }
  deriving (Show, Read)

newtype Projectiles = Projectiles (Seq Projectile)

data Projectile = Projectile
  { projectileLocation :: Location
  , projectileSpeed :: Speed
  , projectileDirection :: Angle
  }
  deriving (Show, Read)

newtype Location = Location (Float, Float)
  deriving (Show, Read)

newtype BlockLocation = BlockLocation (Int, Int)
  deriving (Read, Show, Eq, Ord, Ix)

newtype Angle = Angle Float
  deriving (Show, Read, Num)

newtype Speed = Speed Float
  deriving (Show, Read, Num)

-- TODO: module for the following
moveLocation :: (Speed, Angle) -> Location -> Location
moveLocation (Speed speed, Angle angle) (Location (x, y)) = Location
    (x + dx, y + dy)
  where
    dx = speed * cos angle
    dy = speed * sin angle




