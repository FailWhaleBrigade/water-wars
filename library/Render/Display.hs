module Render.Display where

import ClassyPrelude
import Graphics.Gloss
import Render.State 

-- convert a game state into a picture
render :: MyGame -> IO Picture
render game =
   return $ pictures ([backgroundTexture game, player, wall] ++ toList solidPictures)
  where
    player =
        uncurry translate (playerLoc game) $ color playerColor $ circleSolid 20
    playerColor = red
    wall        = color wallColor $ rectangleWire fieldWidth fieldHeight
    wallColor   = black
    solidPictures =
        map 
            (\solid -> 
                (\(x, y) -> translate x y)
                    (solidCenter solid) 
                    (solidTexture solid)
            )
            (solids game) 
                
