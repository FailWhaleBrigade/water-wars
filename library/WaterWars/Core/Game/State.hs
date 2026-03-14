{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module WaterWars.Core.Game.State
    ( module WaterWars.Core.Game.State
    , module WaterWars.Core.Game.Base
    )
where

import           WaterWars.Core.Game.Base
import GHC.Generics
import Data.Text (Text)
import Data.Sequence

-- |Master-state of the whole game
data GameState = GameState
    { inGamePlayers :: InGamePlayers
    , gameDeadPlayers :: DeadPlayers
    , gameProjectiles :: Projectiles
    , gameTicks :: Integer
    } deriving (Show, Read, Eq, Generic)

newtype InGamePlayers = InGamePlayers
    { getInGamePlayers :: Seq InGamePlayer
    }
    deriving (Read, Show, Eq, Semigroup, Monoid, Generic)

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
    deriving (Show, Read, Eq, Generic)

newtype Player = Player
    { playerId :: Text
    }
    deriving (Show, Read, Eq, Ord, Generic)

newtype DeadPlayers = DeadPlayers
    { getDeadPlayers :: Seq DeadPlayer
    }
    deriving (Read, Show, Eq, Semigroup, Monoid, Generic)

data DeadPlayer = DeadPlayer
    { deadPlayerDescription :: Player
    , deadPlayerLocation :: Location
    , playerDeathTick :: Integer
    }
    deriving (Show, Read, Eq, Generic)

newtype Projectiles = Projectiles
    { getProjectiles :: Seq Projectile
    }
    deriving (Show, Eq, Read, Generic)

data Projectile = Projectile
    { projectileLocation :: Location
    , projectileVelocity :: VelocityVector
    , projectilePlayer :: Player
    }
    deriving (Show, Read, Eq, Ord, Generic)

-- TODO: better name
data IsOnGround = OnGround | InAir
    deriving (Show, Read, Enum, Generic)
