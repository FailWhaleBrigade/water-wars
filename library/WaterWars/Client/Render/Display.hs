module WaterWars.Client.Render.Display where

import ClassyPrelude

import Graphics.Gloss as Gloss

import WaterWars.Client.Render.State
import WaterWars.Client.Render.Terrain.Solid
import WaterWars.Client.Render.Config

-- |Convert a game state into a picture
renderIO :: WorldSTM -> IO Picture
renderIO (WorldSTM tvar) = render <$> readTVarIO tvar

-- TODO: render WorldInfo in combination with RenderInfo
render :: World -> Picture
render World {..} = Gloss.pictures
    (  [backgroundTexture renderInfo]
    ++ toList solidPictures
    ++ playerPictures
    ++ toList projectilePictures
    ++ [mantaPicture]
    )
  where
    allPlayers =
        maybeToList (player worldInfo) ++ toList (otherPlayers worldInfo)
    playerPictures = map
        (\p ->
            let Location (x, y) = playerLocation p
            in
                translate (blockSize * x) (blockSize * y + blockSize / 2)
                $ color playerColor
                $ circleSolid 20
        )
        allPlayers
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
projectileToPicture p = translate (x * blockSize) (y * blockSize) . scale 0.2 0.2
    where Location (x, y) = projectileLocation p

animateAnimation :: Animation -> Picture
animateAnimation Animation {..} = translate x y img
  where
    Location (x, y) = location
    img             = animationPictures `indexEx` picInd
