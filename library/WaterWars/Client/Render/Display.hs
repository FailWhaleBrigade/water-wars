module WaterWars.Client.Render.Display where

import ClassyPrelude

import Graphics.Gloss as Gloss

import WaterWars.Client.Render.State
import WaterWars.Client.Render.Terrain.Solid
import WaterWars.Client.Render.Config
import WaterWars.Core.Game

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
        (\InGamePlayer{..} ->
            let Location (x, y) = playerLocation
            in
                translate (blockSize * x) (blockSize * y + blockSize * playerHeight / 2)
                $ color playerColor
                $ scale blockSize blockSize
                $ scale playerWidth playerHeight
                $ scale (1 / mermaidHeight) (1 / mermaidHeight)
                $ scale directionComponent 1 ((animationPictures $ playerAnim) `indexEx` (picInd $ playerAnim))
        )
        allPlayers
    projectilePictures =
        map (\p -> projectileToPicture p $ projectileTexture renderInfo)
            (projectiles worldInfo) :: Seq Picture
    playerColor   = red
    solidPictures = map solidToPicture (solids renderInfo)
    mantaPicture  = animateAnimation (mantaAnimation renderInfo)
    directionComponent = if (lastDirection worldInfo) == RunLeft then -1 else 1
    playerAnim = case player worldInfo of
                    Just p -> if abs (velocityX $ playerVelocity p) <= 0.01
                        then playerAnimation renderInfo
                            else playerRunningAnimation renderInfo
                    Nothing -> playerAnimation renderInfo

solidToPicture :: Solid -> Picture
solidToPicture solid =
    uncurry translate (solidCenter solid)
        $ scale blockSize blockSize
        $ scale (1 / blockImgWidth) (1 / blockImgHeight)
        $ solidTexture solid

projectileToPicture :: Projectile -> Picture -> Picture
projectileToPicture p = translate (x * blockSize) (y * blockSize) . scale 0.2 0.2
    where Location (x, y) = projectileLocation p

animateAnimation :: Animation -> Picture
animateAnimation Animation {..} = img
  where
    img             = animationPictures `indexEx` picInd
