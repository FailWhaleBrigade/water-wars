{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module WaterWars.Core.GameState where

import ClassyPrelude

-- |Master-state of the whole game
data GameState = GameState
    { inGamePlayers :: InGamePlayers
    , gameProjectiles :: Projectiles
    } deriving (Show, Read, Eq)

newtype InGamePlayers = InGamePlayers 
    { getInGamePlayers :: Seq InGamePlayer 
    }
    deriving (Read, Show, Eq, MonoFunctor)

type instance Element InGamePlayers = InGamePlayer

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

newtype Projectiles = Projectiles { getProjectiles :: Seq Projectile } deriving (Show, Eq, Read)

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

data VelocityVector = VelocityVector Float Float
    deriving (Show, Read, Eq)

instance Semigroup VelocityVector where
    VelocityVector vx1 vy1 <> VelocityVector vx2 vy2 =
        VelocityVector (vx1 + vx2) (vy1 + vy2)

instance Monoid VelocityVector where
    mempty = VelocityVector 0 0
    mappend = (<>)

newtype Angle = Angle Float
    deriving (Show, Read, Num, Eq, Floating, Fractional)

newtype Speed = Speed Float
    deriving (Show, Read, Num, Eq, Floating, Fractional)
