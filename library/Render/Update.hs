module Render.Update where

import ClassyPrelude 
import Graphics.Gloss
import Render.State
import Graphics.Gloss.Interface.IO.Game


handleKeys :: Event -> MyGame -> MyGame
handleKeys (EventKey (Char c) _ _ _) game
    | c == 'a' = game { playerVel = (-v, 0) }
    | c == 'w' = game { playerVel = (0, v) }
    | c == 's' = game { playerVel = (0, -v) }
    | c == 'd' = game { playerVel = (v, 0) }
    where v = 10
handleKeys _ game = game { playerVel = (0, 0) }

handleKeysIO :: Event -> MyGame -> IO MyGame
handleKeysIO e state = return $ handleKeys e state

movePlayer :: Float -> MyGame -> MyGame
movePlayer seconds game = game { playerLoc = (x', y') }
  where
    (x , y ) = playerLoc game
    (vx, vy) = playerVel game
    x'       = x + vx * seconds
    y'       = y + vy * seconds

update :: Float -> MyGame -> MyGame
update seconds = movePlayer seconds

updateIO :: Float -> MyGame -> IO MyGame
updateIO diff state = return $ update diff state
