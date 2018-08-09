module WaterWars.Core.Game.Constants where

import           ClassyPrelude

defaultPlayerHeight :: Float
defaultPlayerHeight = 1.6 * defaultPlayerWidth

defaultPlayerWidth :: Float
defaultPlayerWidth = 2

playerHeadHeight :: Float
playerHeadHeight = 1 / 2 * defaultPlayerHeight

shootCooldown :: Int
shootCooldown = 50

outOfBoundsTolerance :: Float
outOfBoundsTolerance = 5
