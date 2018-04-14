module WaterWars.Client.Render.Display where

import ClassyPrelude

import Graphics.Gloss

import WaterWars.Client.Render.State
import WaterWars.Client.Render.Entity.Solid

-- convert a game state into a picture
renderIO :: WorldSTM -> IO Picture
renderIO (WorldSTM tvar) = render <$> readTVarIO tvar


render :: World -> Picture
render World{..} = pictures ([backgroundTexture, playerPicture] ++ toList solidPictures)
        where
        playerPicture =
            uncurry translate (playerLoc player) $ color playerColor $ circleSolid 20
        playerColor   = red
        solidPictures = map solidToPicture solids

solidToPicture :: Solid -> Picture
solidToPicture solid =
    uncurry translate (solidCenter solid) (solidTexture solid)
