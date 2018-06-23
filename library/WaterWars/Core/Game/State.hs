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
    , gameDeadPlayers :: DeadPlayers
    , gameProjectiles :: Projectiles
    , gameTicks :: Integer
    } deriving (Show, Read, Eq)

newtype InGamePlayers = InGamePlayers
    { getInGamePlayers :: Seq InGamePlayer
    }
    deriving (Read, Show, Eq, MonoFunctor, Monoid)

type instance Element InGamePlayers = InGamePlayer

data InGamePlayer = InGamePlayer
    { playerDescription :: Player
    , playerLocation :: Location
    , playerMaxHealth :: Int
    , playerHealth :: Int
    , playerLastRunDirection :: RunDirection
    , playerVelocity :: VelocityVector
    , playerShootCooldown :: Int
    , playerWidth :: Float
    , playerHeight :: Float
    }
    deriving (Show, Read, Eq)

newtype Player = Player
    { playerId :: Text
    }
    deriving (Show, Read, Eq, Ord)

newtype DeadPlayers = DeadPlayers
    { getDeadPlayers :: Seq DeadPlayer
    }
    deriving (Read, Show, Eq, MonoFunctor, Monoid)

type instance Element DeadPlayers = DeadPlayer

data DeadPlayer = DeadPlayer
    { deadPlayerDescription :: Player
    , deadPlayerLocation :: Location
    , playerDeathTick :: Integer
    }
    deriving (Show, Read, Eq)

newtype Projectiles = Projectiles
    { getProjectiles :: Seq Projectile
    }
    deriving (Show, Eq, Read)

data Projectile = Projectile
    { projectileLocation :: Location
    , projectileVelocity :: VelocityVector
    }
    deriving (Show, Read, Eq)

-- TODO: better name
data IsOnGround = OnGround | InAir
    deriving (Show, Read, Enum)
