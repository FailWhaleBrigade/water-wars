{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module WaterWars.Core.Game.State
    ( module WaterWars.Core.Game.State
    , module WaterWars.Core.Game.Base
    )
where

import           ClassyPrelude
import           WaterWars.Core.Game.Base

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
    , playerLastRunDirection :: RunDirection
    , playerVelocity :: VelocityVector
    , playerShootCooldown :: Int
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

-- TODO: better name
data IsOnGround = OnGround | InAir
    deriving (Show, Read, Enum)
