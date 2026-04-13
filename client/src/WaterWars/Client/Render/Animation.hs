module WaterWars.Client.Render.Animation where

import WaterWars.Core.Game
import WaterWars.Client.Resources.Image (GameImage)
import GHC.Generics
import Miso

data Direction = LeftDir | RightDir deriving (Eq, Show, Read, Ord, Enum, Bounded)

data Animation = Animation
    { countDownTilNext :: Int
    , countDownMax :: Int
    , animationPictures :: [GameImage]
    } deriving Generic

instance FromJSVal Animation where
data PlayerAnimation
    = PlayerIdleAnimation Animation
    | PlayerRunningAnimation Animation
    | PlayerDeathAnimation BackgroundAnimation
    deriving Generic

-- instance FromJSVal PlayerAnimation where

data BackgroundAnimation = BackgroundAnimation
    { animation :: Animation
    , location :: Location
    , updateOperation :: BackgroundAnimation -> BackgroundAnimation
    , direction :: Direction
    } deriving Generic


-- instance FromJSVal BackgroundAnimation where

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
    | countDownTilNext == 0 =  a { animationPictures = tail animationPictures
           , countDownTilNext  = countDownMax
           }
    | otherwise = a { countDownTilNext = countDownTilNext - 1 }

updateBackgroundAnimation :: BackgroundAnimation -> BackgroundAnimation
updateBackgroundAnimation a = b { animation = newAnimation }
  where
    newAnimation = updateAnimation (animation a)
    b            = (updateOperation a) a
