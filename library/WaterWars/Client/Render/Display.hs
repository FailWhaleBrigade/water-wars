module WaterWars.Client.Render.Display where

import ClassyPrelude

import Graphics.Gloss

import WaterWars.Client.Render.State
import WaterWars.Client.Render.Terrain.Solid

-- convert a game state into a picture
renderIO :: WorldSTM -> IO Picture
renderIO (WorldSTM tvar) = render <$> readTVarIO tvar

-- TODO: render WorldInfo in combination with RenderInfo
render :: World -> Picture
render World {..} = pictures
    ([backgroundTexture renderInfo, playerPicture] ++ toList solidPictures)
  where
    playerPicture =
        uncurry translate (playerLoc $ player worldInfo)
            $ color playerColor
            $ circleSolid 20
    playerColor   = red
    solidPictures = map solidToPicture (solids renderInfo)

solidToPicture :: Solid -> Picture
solidToPicture solid =
    uncurry translate (solidCenter solid) (solidTexture solid)
