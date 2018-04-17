module WaterWars.Client.Render.Display where

import ClassyPrelude

import Graphics.Gloss as Gloss

import WaterWars.Client.Render.State
import WaterWars.Client.Render.Terrain.Solid

-- convert a game state into a picture
renderIO :: WorldSTM -> IO Picture
renderIO (WorldSTM tvar) = render <$> readTVarIO tvar

-- TODO: render WorldInfo in combination with RenderInfo
render :: World -> Picture
render World {..} = Gloss.pictures
    (  [backgroundTexture renderInfo]
    ++ toList solidPictures
    ++ [playerPicture]
    ++ toList projectilePictures
    ++ [mantaPicture]
    )
  where
    Location (x, y) = playerLocation $ player worldInfo
    playerPicture   = translate x y $ color playerColor $ circleSolid 20
    projectilePictures =
        map (\p -> projectileToPicture p $ projectileTexture renderInfo)
            (projectiles worldInfo) :: Seq Picture
    playerColor   = red
    solidPictures = map solidToPicture (solids renderInfo)
    mantaPicture  = animateAnimation (animation renderInfo)

solidToPicture :: Solid -> Picture
solidToPicture solid =
    uncurry translate (solidCenter solid) (solidTexture solid)

projectileToPicture :: Projectile -> Picture -> Picture
projectileToPicture p tex = scale 0.2 0.2 $ translate x y tex
    where Location (x, y) = projectileLocation p

animateAnimation :: Animation -> Picture
animateAnimation Animation {..} = translate x y img
  where
    Location (x, y) = location
    img             = animationPictures `indexEx` picInd

updateAnimation :: Animation -> Animation
updateAnimation a@Animation {..} = if countDownTilNext == 0
    then a { picInd = picInd + 1, location = Location (x + 1, y) }
    else a { location = Location (x + 1, y) }
    where Location (x, y) = location
