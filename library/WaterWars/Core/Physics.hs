module WaterWars.Core.Physics where

import ClassyPrelude
import WaterWars.Core.GameState

-- TODO: module for the following
moveLocation :: (Speed, Angle) -> Location -> Location
moveLocation (Speed speed, Angle angle) (Location (x, y)) = Location
    (x + dx, y + dy)
  where
    dx = speed * cos angle
    dy = speed * sin angle
