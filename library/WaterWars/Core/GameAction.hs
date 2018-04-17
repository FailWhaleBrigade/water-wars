{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WaterWars.Core.GameAction where

import ClassyPrelude
import WaterWars.Core.GameState

-- TODO: only allow an action of one type in here. Maybe Map?
newtype Action = Action
    { rawActions :: Seq RawAction
    } deriving (Show, Read, Eq, Monoid)

data RawAction
    = Run { runDirection :: RunDirection }
    | Jump
    | Shoot { shootAngle :: Angle }
    deriving (Show, Read, Eq)

data RunDirection = RunLeft | RunRight deriving (Show, Read, Eq, Enum, Bounded)

isRunAction :: RawAction -> Bool
isRunAction (Run _) = True
isRunAction _ = False

runVelocityVector :: RunDirection -> VelocityVector
runVelocityVector RunLeft = VelocityVector (-0.5) 0
runVelocityVector RunRight = VelocityVector 0.5 0
