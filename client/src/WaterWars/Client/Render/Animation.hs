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
    } deriving (Generic, Eq)

instance FromJSVal Animation where

data PlayerAnimation
    = PlayerIdleAnimation Animation
    | PlayerRunningAnimation Animation
    | PlayerDeathAnimation BackgroundAnimation
    deriving (Generic, Eq)

data BackgroundAnimation = BackgroundAnimation
    { animation :: Animation
    , location :: Location
    , direction :: Direction
    } deriving (Generic, Eq)

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
    PlayerDeathAnimation $ updatePlayerBackgroundAnimation anim

updateAnimation :: Animation -> Animation
updateAnimation a@Animation {..}
    | countDownTilNext == 0 =  a { animationPictures = tail animationPictures
           , countDownTilNext  = countDownMax
           }
    | otherwise = a { countDownTilNext = countDownTilNext - 1 }

updatePlayerBackgroundAnimation :: BackgroundAnimation -> BackgroundAnimation
updatePlayerBackgroundAnimation a = b { animation = newAnimation }
  where
    newAnimation = updateAnimation (animation a)
    b            = deadPlayerUpdateOperation a

deadPlayerUpdateOperation :: BackgroundAnimation -> BackgroundAnimation
deadPlayerUpdateOperation BackgroundAnimation{..} =
  BackgroundAnimation
    { location = Location (x, newY)
    , ..
    }
 where
  Location (x, y) = location
  newY = y + 0.05
