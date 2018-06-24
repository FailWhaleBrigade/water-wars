module WaterWars.Client.Render.Display where

import ClassyPrelude
import Graphics.Gloss as Gloss

import WaterWars.Client.Render.Config
import WaterWars.Client.Render.Animation
import WaterWars.Client.Render.State
import WaterWars.Client.Render.Terrain.Solid

import WaterWars.Core.Game
import WaterWars.Core.Game.Constants

-- |Convert a game state into a picture
renderIO :: WorldSTM -> IO Picture
renderIO (WorldSTM tvar) = render <$> readTVarIO tvar

render :: World -> Picture
render World {..} = Gloss.pictures
    (  [backgroundTexture]
    ++ [mantaPicture]
    ++ toList solidPictures
    ++ singleton playerPicture
    ++ deadPlayerPictures
    ++ playerPictures
    ++ toList projectilePictures
    ++ maybeToList readyPicture
    ++ maybeToList winPicture
    )
  where
    RenderInfo {..} = renderInfo
    WorldInfo {..}  = worldInfo
    GameState {..}  = gameStateUpdate lastGameUpdate
    Resources {..}  = resources

    livingPlayers :: [InGamePlayer]
    livingPlayers = toList $ getInGamePlayers inGamePlayers

    deadPlayers :: [DeadPlayer]
    deadPlayers = toList
        (filter
            (\DeadPlayer {..} -> abs (gameTicks - playerDeathTick) < 500)
            (getDeadPlayers gameDeadPlayers)
        )

    playerPictures :: [Picture]
    playerPictures = map (inGamePlayerToPicture renderInfo) livingPlayers

    deadPlayerPictures :: [Picture]
    deadPlayerPictures = map (deadPlayerToPicture renderInfo) deadPlayers

    playerPicture :: Picture
    playerPicture = case localPlayer of
        Nothing ->
            {- we are not connected yet -}
            displayText (displayAnimation connectingAnimation)
        Just p ->
            case
                    find ((== p) . playerDescription)
                         (getInGamePlayers inGamePlayers)
                of
                    Nothing -> {- We are dead -}
                        displayText youLostTexture
                    Just inGamePlayer -> {- We are alive -}
                        inGamePlayerToPicture renderInfo inGamePlayer

    projectilePictures :: Seq Picture
    projectilePictures = map (projectileToPicture renderInfo) projectiles

    solidPictures :: Seq Picture
    solidPictures = map solidToPicture solids

    mantaPicture :: Picture
    mantaPicture = backgroundAnimationToPicture renderInfo mantaAnimation

    readyPicture :: Maybe Picture
    readyPicture = do
        down <- countdown
        return $ countdownToPicture renderInfo (down - gameTicks)

    winPicture :: Maybe Picture
    winPicture = do
        p <- localPlayer
        guard gameRunning
        guard (length livingPlayers == 1)
        guard (isJust $ find ((== p) . playerDescription) livingPlayers)
        return $ displayText youWinTexture

inGamePlayerColor :: Color
inGamePlayerColor = red

solidToPicture :: Solid -> Picture
solidToPicture solid =
    uncurry translate (solidCenter solid)
        $ scale blockSize           blockSize
        $ scale (1 / blockImgWidth) (1 / blockImgHeight)
        $ solidTexture solid

inGamePlayerToPicture :: RenderInfo -> InGamePlayer -> Picture
inGamePlayerToPicture RenderInfo {..} InGamePlayer {..} =
    let Resources {..}     = resources
        Location (x, y)    = playerLocation
        directionComponent = case playerLastRunDirection of
            RunLeft  -> -1
            RunRight -> 1
        maybeAnimation = lookup playerDescription playerAnimations
        Animation {..} =
            playerToAnimation $ fromMaybe defaultPlayerAnimation maybeAnimation
    in  translate (blockSize * x) (blockSize * y + blockSize * playerHeight / 2)
        $ color inGamePlayerColor
        $ scale blockSize          blockSize
        $ scale playerWidth        playerHeight
        $ scale (1 / mermaidWidth) (1 / mermaidHeight)
        $ scale directionComponent 1 (headEx animationPictures)

deadPlayerToPicture :: RenderInfo -> DeadPlayer -> Picture
deadPlayerToPicture RenderInfo {..} DeadPlayer {..} =
    let
        Resources {..}  = resources

        maybeAnimation  = lookup deadPlayerDescription playerAnimations
        Location (x, y) = case maybeAnimation of
            Just (PlayerDeathAnimation ba) -> location ba
            _                              -> deadPlayerLocation
        Animation {..} =
            playerToAnimation $ fromMaybe defaultPlayerAnimation maybeAnimation
    in
        translate (blockSize * x)
                  (blockSize * y + blockSize * defaultPlayerHeight / 2)
        $ color inGamePlayerColor
        $ scale blockSize          blockSize
        $ scale defaultPlayerWidth defaultPlayerHeight
        $ scale (1 / mermaidWidth)
                (1 / mermaidHeight)
                (headEx animationPictures)

projectileToPicture :: RenderInfo -> Projectile -> Picture
projectileToPicture RenderInfo {..} p = translate
    (x * blockSize)
    (y * blockSize)
    (projectileTexture resources)
    where Location (x, y) = projectileLocation p

countdownToPicture :: RenderInfo -> Integer -> Picture
countdownToPicture RenderInfo {..} tick = displayText pic
  where
    Resources {..} = resources
    pic | tick >= 180 = countdownTextures `indexEx` 0
        | tick >= 120 = countdownTextures `indexEx` 1
        | tick >= 60  = countdownTextures `indexEx` 2
        | otherwise {- tick >= 0 -}
                    = countdownTextures `indexEx` 3

backgroundAnimationToPicture :: RenderInfo -> BackgroundAnimation -> Picture
backgroundAnimationToPicture _ BackgroundAnimation {..} = translate x y
    $ scale scaleFactor 1 pic
  where
    scaleFactor = case direction of
        RightDir -> -1
        LeftDir  -> 1
    pic             = displayAnimation animation
    Location (x, y) = location

displayAnimation :: Animation -> Picture
displayAnimation Animation {..} = headEx animationPictures

displayText :: Picture -> Picture
displayText = translate 0 100


