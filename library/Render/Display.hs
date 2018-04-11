module Render.Display where

import ClassyPrelude
import Graphics.Gloss
import Render.State 

-- convert a game state into a picture
render :: MyGame -> IO Picture
render game =
   return $ pictures ([backgroundTexture game, player] ++ toList solidPictures)
  where
    player =
        uncurry translate (playerLoc game) $ color playerColor $ circleSolid 20
    playerColor = red
    solidPictures =
        map 
            (\solid -> 
                (uncurry translate)
                    (solidCenter solid) 
                    (solidTexture solid)
            )
            (solids game) 
                
