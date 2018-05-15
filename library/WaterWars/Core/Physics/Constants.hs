module WaterWars.Core.Physics.Constants where

import ClassyPrelude
import WaterWars.Core.GameState

gravityForce :: Float
gravityForce = 0.005

jumpForce :: Float
jumpForce = 0.2

runSpeed :: Float
runSpeed = 0.15

runAccelerationGround :: Float
runAccelerationGround = 0.05

runAccelerationAir :: Float
runAccelerationAir = 0.003

verticalDragGround :: Float
verticalDragGround = 0.8

verticalDragAir :: Float
verticalDragAir = 0.98

maxVelocity :: (Float, Float)
maxVelocity = (0.25, 0.5)

projectileSpeed :: Speed
projectileSpeed = 0.5

shootCooldown :: Int
shootCooldown = 10
