module WaterWars.Client.Render.Animation where

import ClassyPrelude

import Graphics.Gloss as Gloss

import WaterWars.Core.Game

data Direction = LeftDir | RightDir deriving (Eq, Show, Read, Ord, Enum, Bounded)

data Animation = Animation
    { countDownTilNext :: Integer
    , countDownMax :: Integer
    , animationPictures :: [Picture]
    } deriving (Show, Eq)

data PlayerAnimation
    = PlayerIdleAnimation Animation
    | PlayerRunningAnimation Animation
    | PlayerDeathAnimation BackgroundAnimation

data BackgroundAnimation = BackgroundAnimation
    { animation :: Animation
    , location :: Location
    , updateOperation :: BackgroundAnimation -> BackgroundAnimation
    , direction :: Direction
    }

playerToAnimation :: PlayerAnimation -> Animation
playerToAnimation (PlayerIdleAnimation    anim) = anim
playerToAnimation (PlayerRunningAnimation anim) = anim
playerToAnimation (PlayerDeathAnimation   backgroundAnimation) = animation backgroundAnimation

updatePlayerAnimation :: PlayerAnimation -> PlayerAnimation
updatePlayerAnimation (PlayerIdleAnimation anim) =
    PlayerIdleAnimation $ updateAnimation anim
updatePlayerAnimation (PlayerRunningAnimation anim) =
    PlayerRunningAnimation $ updateAnimation anim
updatePlayerAnimation (PlayerDeathAnimation anim) =
    PlayerDeathAnimation $ updateBackgroundAnimation anim

updateAnimation :: Animation -> Animation
updateAnimation a@Animation {..} 
    | countDownTilNext == 0 =  a { animationPictures = tailEx animationPictures
           , countDownTilNext  = countDownMax
           }
    | otherwise = a { countDownTilNext = countDownTilNext - 1 }

updateBackgroundAnimation :: BackgroundAnimation -> BackgroundAnimation
updateBackgroundAnimation a = b { animation = newAnimation }
  where
    newAnimation = updateAnimation (animation a)
    b            = (updateOperation a) a
