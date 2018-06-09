module WaterWars.Client.Render.Animation where

import ClassyPrelude

import Graphics.Gloss as Gloss

data Animation = Animation
    { countDownTilNext :: Integer
    , countDownMax :: Integer
    , animationPictures :: [Picture]
    , picInd :: Int
    } deriving (Show, Eq)

data PlayerAnimation 
    = PlayerIdleAnimation Animation
    | PlayerRunningAnimation Animation
    deriving (Show, Eq)

playerToAnimation :: PlayerAnimation -> Animation
playerToAnimation (PlayerIdleAnimation anim) = anim
playerToAnimation (PlayerRunningAnimation anim) = anim

updatePlayerAnimation :: PlayerAnimation -> PlayerAnimation
updatePlayerAnimation (PlayerIdleAnimation anim) = PlayerIdleAnimation $ updateAnimation anim
updatePlayerAnimation (PlayerRunningAnimation anim) = PlayerRunningAnimation $ updateAnimation anim

updateAnimation :: Animation -> Animation
updateAnimation a@Animation {..} = if countDownTilNext == 0
    then a { picInd = picInd + 1, countDownTilNext = countDownMax }
    else a { countDownTilNext = countDownTilNext - 1 }
