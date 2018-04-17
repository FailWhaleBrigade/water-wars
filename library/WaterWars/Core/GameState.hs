{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WaterWars.Core.GameState where

import ClassyPrelude
import Data.Array.IArray

import WaterWars.Core.Terrain.Block

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
  { playerDescription :: Player
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
  deriving (Show, Read, Eq, Ord)

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
  deriving (Show, Read, Num, Eq, Floating, Fractional)

newtype Speed = Speed Float
  deriving (Show, Read, Num, Eq, Floating, Fractional)
