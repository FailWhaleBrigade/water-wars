module WaterWars.Client.Render.Display where

import ClassyPrelude

import Graphics.Gloss as Gloss

import WaterWars.Client.Render.Config
import WaterWars.Client.Render.Animation
import WaterWars.Client.Render.State
import WaterWars.Client.Render.Terrain.Solid

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
    playerPictures :: [Picture]
    playerPictures = map (inGamePlayerToPicture renderInfo) allPlayers
    projectilePictures =
        map (\p -> projectileToPicture p $ projectileTexture renderInfo)
            (projectiles worldInfo) :: Seq Picture

    solidPictures = map solidToPicture (solids renderInfo)
    mantaPicture  = animateAnimation (mantaAnimation renderInfo)

inGamePlayerColor :: Color
inGamePlayerColor = red

solidToPicture :: Solid -> Picture
solidToPicture solid =
    uncurry translate (solidCenter solid) (solidTexture solid)

inGamePlayerToPicture :: RenderInfo -> InGamePlayer -> Picture
inGamePlayerToPicture RenderInfo {..} InGamePlayer {..} =
    let
        Location (x, y)    = playerLocation
        directionComponent = case playerLastRunDirection of
            RunLeft  -> -1
            RunRight -> 1
        maybeAnimation = lookup playerDescription playerAnimations
        Animation {..} =
            playerToAnimation $ fromMaybe defaultPlayerAnimation maybeAnimation
    in
        translate (blockSize * x) (blockSize * y + blockSize)
        $ color inGamePlayerColor
        $ scale (0.624 * directionComponent)
                0.624
                (animationPictures `indexEx` picInd)


projectileToPicture :: Projectile -> Picture -> Picture
projectileToPicture p = translate (x * blockSize) (y * blockSize)
    . scale 0.2 0.2
    where Location (x, y) = projectileLocation p

animateAnimation :: Animation -> Picture
animateAnimation Animation {..} = animationPictures `indexEx` picInd
