module WaterWars.Core.Physics where

import ClassyPrelude
import WaterWars.Core.GameState

-- TODO: module for the following
moveLocation :: VelocityVector -> Location -> Location
moveLocation (VelocityVector dx dy) (Location (x, y)) = Location
    (x + dx, y + dy)

jumpVector :: VelocityVector
jumpVector = VelocityVector 
