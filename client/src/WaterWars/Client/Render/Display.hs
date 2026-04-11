module WaterWars.Client.Render.Display where

import           WaterWars.Client.Render.Config
import           WaterWars.Client.Render.Animation
import           WaterWars.Client.Render.State
import           WaterWars.Client.Render.Terrain.Solid

import           WaterWars.Core.Game
import           WaterWars.Core.Game.Constants
import Data.Sequence (Seq)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Foldable as Foldable
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map

-- |Convert a game state into a picture
renderIO :: WorldSTM -> IO Can
renderIO (WorldSTM tvar) = render <$> readTVarIO tvar

render :: World -> Canvas 
render World {..} = Gloss.pictures
    (  [backgroundTexture]
    <> [mantaPicture]
    <> Foldable.toList solidPictures
    <> deadPlayerPictures
    <> playerPictures
    <> Foldable.toList projectilePictures
    <> Maybe.maybeToList readyPicture
    <> Maybe.maybeToList playerPicture
    <> Maybe.maybeToList serverTextMessage
    <> Maybe.maybeToList shootTargetPicture
    )
  where
    RenderInfo {..} = renderInfo
    WorldInfo {..}  = worldInfo
    GameState {..}  = gameStateUpdate lastGameUpdate
    Resources {..}  = resources

    livingPlayers :: [InGamePlayer]
    livingPlayers = Foldable.toList $ getInGamePlayers inGamePlayers

    deadPlayers :: [DeadPlayer]
    deadPlayers =
        (filter
            (\DeadPlayer {..} -> abs (gameTicks - playerDeathTick) < 500)
            (Foldable.toList $ getDeadPlayers gameDeadPlayers)
        )

    playerPictures :: [Picture]
    playerPictures = map (inGamePlayerToPicture renderInfo) livingPlayers

    deadPlayerPictures :: [Picture]
    deadPlayerPictures = map (deadPlayerToPicture renderInfo) deadPlayers

    stateOf :: Maybe Player -> PlayerState
    stateOf Nothing = Disconnected
    stateOf (Just p)
        | Maybe.isJust $ List.find ((== p) . playerDescription) livingPlayers = Alive
        | otherwise = Dead

    serverTextMessage :: Maybe Picture
    serverTextMessage
        | state == Disconnected = Just
            (displayText (displayAnimation connectingAnimation))
        | state == Dead = Just (displayText youLostTexture)
        | state == Alive && localPlayer == winnerPlayer = Just
            (displayText youWinTexture)
        | otherwise = Nothing
        where state = stateOf localPlayer

    playerPicture :: Maybe Picture
    playerPicture = do
        p     <- localPlayer
        alive <- List.find ((== p) . playerDescription)
                      (getInGamePlayers inGamePlayers)
        Just (inGamePlayerToPicture renderInfo alive)

    projectilePictures :: Seq Picture
    projectilePictures = fmap (projectileToPicture renderInfo) projectiles

    solidPictures :: Seq Picture
    solidPictures = fmap solidToPicture (solids <> decorations)

    mantaPicture :: Picture
    mantaPicture = backgroundAnimationToPicture renderInfo mantaAnimation

    readyPicture :: Maybe Picture
    readyPicture = do
        down <- countdown
        return $ countdownToPicture renderInfo (down - gameTicks)


    shootTargetPicture :: Maybe Picture
    shootTargetPicture = do
        Location (x, y) <- lastShot
        return $ translate (blockSize * x) (blockSize * y) $ circle 5

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
        maybeAnimation = Map.lookup playerDescription playerAnimations
        Animation {..} =
            playerToAnimation $ Maybe.fromMaybe defaultPlayerAnimation maybeAnimation
    in  translate (blockSize * x) (blockSize * y + blockSize * playerHeight / 2)
        $ color inGamePlayerColor
        $ scale blockSize          blockSize
        $ scale playerWidth        playerHeight
        $ scale (1 / mermaidWidth) (1 / mermaidHeight)
        $ scale directionComponent 1 (head animationPictures)

deadPlayerToPicture :: RenderInfo -> DeadPlayer -> Picture
deadPlayerToPicture RenderInfo {..} DeadPlayer {..}
    = let
          Resources {..}  = resources

          maybeAnimation  = Map.lookup deadPlayerDescription playerAnimations
          Location (x, y) = case maybeAnimation of
              Just (PlayerDeathAnimation ba) -> location ba
              _                              -> deadPlayerLocation
          Animation {..} = playerToAnimation
              $ Maybe.fromMaybe defaultPlayerAnimation maybeAnimation
      in
          translate (blockSize * x)
                    (blockSize * y + blockSize * defaultPlayerHeight / 2)
          $ color inGamePlayerColor
          $ scale blockSize          blockSize
          $ scale defaultPlayerWidth defaultPlayerHeight
          $ scale (1 / mermaidWidth)
                  (1 / mermaidHeight)
                  (head animationPictures)

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
    pic | tick >= 180 = countdownTextures !! 0
        | tick >= 120 = countdownTextures !! 1
        | tick >= 60  = countdownTextures !! 2
        | otherwise {- tick >= 0 -}
                    = countdownTextures !! 3

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
displayAnimation Animation {..} = head animationPictures

displayText :: Picture -> Picture
displayText = translate 0 100

data PlayerState = Alive | Disconnected | Dead deriving (Eq, Show, Enum, Bounded)
