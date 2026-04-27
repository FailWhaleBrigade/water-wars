{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WaterWars.Core.Game.Events
    ( module WaterWars.Core.Game.Events
    , module WaterWars.Core.Game.State
    , module WaterWars.Core.Game.Base
    )
where

import           WaterWars.Core.Game.Base
import           WaterWars.Core.Game.State
import GHC.Generics

newtype GameEvents = GameEvents
    { getGameEvents :: [GameEvent]
    }
    deriving (Read, Show, Eq, Generic)
    deriving newtype (Semigroup, Monoid)

newtype GameEvent = ShotProjectile Projectile
    deriving (Read, Show, Eq, Generic)
