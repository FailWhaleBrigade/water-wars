{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module WaterWars.Core.Game.Events
    ( module WaterWars.Core.Game.Events
    , module WaterWars.Core.Game.State
    , module WaterWars.Core.Game.Base
    )
where

import           ClassyPrelude
import           WaterWars.Core.Game.Base
import           WaterWars.Core.Game.State

newtype GameEvents = GameEvents
    { getGameEvents :: Seq GameEvent
    }
    deriving (Read, Show, Eq, Monoid)

newtype GameEvent = ShotProjectile Projectile
    deriving (Read, Show, Eq)
