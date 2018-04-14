module WaterWars.Client.Render.Display where

import ClassyPrelude

import Graphics.Gloss

import WaterWars.Client.Render.State
import WaterWars.Client.Render.Entity.Solid

-- convert a game state into a picture
render :: MyGame -> IO Picture
render game = return
    $ pictures ([backgroundTexture game, player] ++ toList solidPictures)
  where
    player =
        uncurry translate (playerLoc game) $ color playerColor $ circleSolid 20
    playerColor   = red
    solidPictures = map solidToPicture (solids game)

solidToPicture :: Solid -> Picture
solidToPicture solid =
    uncurry translate (solidCenter solid) (solidTexture solid)
