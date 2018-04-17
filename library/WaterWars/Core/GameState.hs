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
  { inGamePlayers :: InGamePlayers
  , gameProjectiles :: Projectiles
  } deriving (Show, Read, Eq)

newtype InGamePlayers = InGamePlayers (Seq InGamePlayer)
  deriving (Read, Show, Eq)

data InGamePlayer = InGamePlayer
  { playerDescription :: Player
  , playerLocation :: Location
  , playerMaxHealth :: Int
  , playerHealth :: Int
  , playerViewDirection :: Angle
  , playerVelocity :: VelocityVector
  }
  deriving (Show, Read, Eq)

newtype Player = Player
  { playerId :: Text
  }
  deriving (Show, Read, Eq, Ord)

newtype Projectiles = Projectiles (Seq Projectile) deriving (Show, Eq, Read)

data Projectile = Projectile
  { projectileLocation :: Location
  , projectileVelocity :: VelocityVector
  }
  deriving (Show, Read, Eq)

newtype Location = Location (Float, Float)
  deriving (Show, Read, Eq)

instance Monoid Location where
  mempty = Location (0, 0)
  mappend (Location (x, y)) (Location (a, b)) = Location (x + a, y + b)

newtype BlockLocation = BlockLocation (Int, Int)
  deriving (Read, Show, Eq, Ord, Ix)

data VelocityVector = VelocityVector Float Float
    deriving (Show, Read, Eq)

newtype Angle = Angle Float
  deriving (Show, Read, Num, Eq, Floating, Fractional)

newtype Speed = Speed Float
  deriving (Show, Read, Num, Eq, Floating, Fractional)
