{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WaterWars.Core.Game.Events
    ( module WaterWars.Core.Game.Events
    , module WaterWars.Core.Game.State
    , module WaterWars.Core.Game.Base
    )
where

import           WaterWars.Core.Game.Base
import           WaterWars.Core.Game.State
import Data.Sequence
import GHC.Generics

newtype GameEvents = GameEvents
    { getGameEvents :: Seq GameEvent
    }
    deriving (Read, Show, Eq, Semigroup, Monoid, Generic)

newtype GameEvent = ShotProjectile Projectile
    deriving (Read, Show, Eq, Generic)
