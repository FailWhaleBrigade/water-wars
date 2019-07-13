{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module WaterWars.Core.Game.Base where

import           ClassyPrelude

data RunDirection = RunLeft | RunRight
    deriving (Show, Read, Eq, Enum, Bounded, Generic)

newtype Location = Location (Float, Float)
  deriving (Show, Read, Eq, Ord, Generic)

instance Semigroup Location where
    (Location (x, y)) <> (Location (a, b)) = Location (x + a, y + b)

instance Monoid Location where
    mempty = Location (0, 0)

data VelocityVector = VelocityVector
    { velocityX :: Float
    , velocityY :: Float
    } deriving (Show, Read, Eq, Ord, Generic)

instance Semigroup VelocityVector where
    VelocityVector vx1 vy1 <> VelocityVector vx2 vy2 =
        VelocityVector (vx1 + vx2) (vy1 + vy2)

instance Monoid VelocityVector where
    mempty = VelocityVector 0 0
    mappend = (<>)

newtype Angle = Angle Float
    deriving (Show, Read, Num, Eq, Floating, Fractional, Generic)

newtype Speed = Speed Float
    deriving (Show, Read, Num, Eq, Floating, Fractional, Generic)

type MovementState = (Location, VelocityVector)
