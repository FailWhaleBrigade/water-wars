{-# LANGUAGE TypeApplications #-}

module WaterWars.Client.Render.Display where

import WaterWars.Client.Render.Animation
import WaterWars.Client.Render.Config
import WaterWars.Client.Render.State
import WaterWars.Client.Render.Terrain.Solid

import Data.Bifunctor (bimap)
import Data.Foldable (traverse_)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Miso.CSS.Color
import Miso.Canvas
import WaterWars.Client.Resources.Image (GameImage (image))
import WaterWars.Core.Game
import WaterWars.Core.Game.Constants
import WaterWars.Client.Resources.Block (lookupBlockMap, BlockMap)

render :: Resources -> RenderInfo -> World -> Canvas ()
render resources renderInfo World{..} = do
  renderBackground backgroundTexture
  renderEnvironment
  renderManta
 where
  -- deadPlayerPictures
  -- playerPictures
  -- projectilePictures
  -- readyPicture
  -- playerPicture
  -- serverTextMessage
  -- shootTargetPicture

  RenderInfo{..} = renderInfo
  WorldInfo{..} = worldInfo
  GameState{..} = gameStateUpdate lastGameUpdate
  Resources{..} = resources

  livingPlayers :: [InGamePlayer]
  livingPlayers = Foldable.toList $ getInGamePlayers inGamePlayers

  deadPlayers :: [DeadPlayer]
  deadPlayers =
    ( filter
        (\DeadPlayer{..} -> abs (gameTicks - playerDeathTick) < 500)
        (Foldable.toList $ getDeadPlayers gameDeadPlayers)
    )

  playerPictures :: Canvas ()
  playerPictures = traverse_ (inGamePlayerToPicture resources renderInfo) livingPlayers

  deadPlayerPictures :: Canvas ()
  deadPlayerPictures = traverse_ (deadPlayerToPicture resources renderInfo) deadPlayers

  stateOf :: Maybe Player -> PlayerState
  stateOf Nothing = Disconnected
  stateOf (Just p)
    | Maybe.isJust $ List.find ((== p) . playerDescription) livingPlayers = Alive
    | otherwise = Dead

  serverTextMessage :: Canvas ()
  serverTextMessage
    | state == Disconnected = do
        displayText
        drawImage (image $ displayAnimation connectingAnimation, 0, 0)
    | state == Dead = do
        displayText
        drawImage (image youLostTexture, 0, 0)
    | state == Alive && localPlayer == winnerPlayer = do
        displayText
        drawImage (image youWinTexture, 0, 0)
    | otherwise = pure ()
   where
    state = stateOf localPlayer

  playerPicture :: Canvas ()
  playerPicture = do
    case localPlayer of
      Nothing -> pure ()
      Just p -> do
        case List.find ((== p) . playerDescription) (getInGamePlayers inGamePlayers) of
          Nothing -> pure ()
          Just alive -> do
            inGamePlayerToPicture resources renderInfo alive

  projectilePictures :: Canvas ()
  projectilePictures = traverse_ (projectileToPicture resources renderInfo) projectiles

  renderEnvironment :: Canvas ()
  renderEnvironment = do
    traverse_ (solidToPicture (flip lookupBlockMap blockMap)) solids
    traverse_ (solidToPicture (flip lookupDecorationMap decorationMap)) decorations

  renderManta :: Canvas ()
  renderManta = backgroundAnimationToPicture mantaAnimation

  readyPicture :: Canvas ()
  readyPicture = do
    case countdown of
      Nothing -> pure ()
      Just down ->
        countdownToPicture resources renderInfo (down - gameTicks)

  shootTargetPicture :: Canvas ()
  shootTargetPicture = do
    case lastShot of
      Nothing -> pure ()
      Just (Location (x, y)) -> do
        translate (toDouble (blockSize * x), toDouble (blockSize * y))


-- circle_ []

renderBackground :: GameImage -> Canvas ()
renderBackground img = do
  save ()
  drawImage' (image img, 0, 0, 800, 600)
  restore ()

inGamePlayerColor :: Color
inGamePlayerColor = red

solidToPicture :: (a -> GameImage) -> Solid a -> Canvas ()
solidToPicture getImage solid = do
  save ()
  translate (bimap toDouble toDouble $ solidCenter solid)
  translate (350, 250)
  drawImage (image $ getImage (solidContent solid), toDouble blockSize, toDouble blockSize)
  restore ()

inGamePlayerToPicture :: Resources -> RenderInfo -> InGamePlayer -> Canvas ()
inGamePlayerToPicture Resources{..} RenderInfo{..} InGamePlayer{..} = do
  let
    Location (x, y) = playerLocation
    directionComponent = case playerLastRunDirection of
      RunLeft -> -1
      RunRight -> 1
    maybeAnimation = lookupPlayerAnimationMap playerDescription playerAnimations
    Animation{..} =
      playerToAnimation $ Maybe.fromMaybe defaultPlayerAnimation maybeAnimation
  scale (directionComponent, 1)
  scale (toDouble (1 / mermaidWidth), toDouble (1 / mermaidHeight))
  scale (toDouble playerWidth, toDouble playerHeight)
  scale (toDouble blockSize, toDouble blockSize)
  -- color inGamePlayerColor
  translate (toDouble (blockSize * x), toDouble (blockSize * y + blockSize * playerHeight / 2))
  drawImage (image $ head animationPictures, 0, 0)

deadPlayerToPicture :: Resources -> RenderInfo -> DeadPlayer -> Canvas ()
deadPlayerToPicture Resources{..} RenderInfo{..} DeadPlayer{..} = do
  let
    maybeAnimation = lookupPlayerAnimationMap deadPlayerDescription playerAnimations
    Location (x, y) = case maybeAnimation of
      Just (PlayerDeathAnimation ba) -> location ba
      _ -> deadPlayerLocation
    Animation{..} =
      playerToAnimation $
        Maybe.fromMaybe defaultPlayerAnimation maybeAnimation

  scale (toDouble (1 / mermaidWidth), toDouble (1 / mermaidHeight))
  scale (toDouble blockSize, toDouble blockSize)
  scale (toDouble defaultPlayerWidth, toDouble defaultPlayerHeight)
  translate (toDouble (blockSize * x), toDouble (blockSize * y + blockSize * defaultPlayerHeight / 2))
  drawImage (image $ head animationPictures, 0, 0)

projectileToPicture :: Resources -> RenderInfo -> Projectile -> Canvas ()
projectileToPicture Resources{..} RenderInfo{..} p = do
  translate (toDouble (x * blockSize), toDouble (y * blockSize))
  drawImage (image projectileTexture, 0, 0)
 where
  Location (x, y) = projectileLocation p

countdownToPicture :: Resources ->  RenderInfo -> Integer -> Canvas ()
countdownToPicture Resources{..} RenderInfo{..} tick = do
  displayText
  drawImage (image pic, 0, 0)
 where
  pic
    | tick >= 180 = countdownTextures !! 0
    | tick >= 120 = countdownTextures !! 1
    | tick >= 60 = countdownTextures !! 2
    | otherwise {- tick >= 0 -} =
        countdownTextures !! 3

backgroundAnimationToPicture :: BackgroundAnimation -> Canvas ()
backgroundAnimationToPicture BackgroundAnimation{..} = do
  save ()
  translate (64, 0)
  scale scaleFactor
  drawImage' (image pic, 0, 0, 64, 64)
  translate (toDouble x, toDouble y)
  restore ()
 where
  scaleFactor = case direction of
    RightDir -> (-1, 1)
    LeftDir -> (1, 1)
  pic = displayAnimation animation
  Location (x, y) = location

toDouble :: (Real a) => a -> Double
toDouble = realToFrac @_ @Double

displayAnimation :: Animation -> GameImage
displayAnimation Animation{..} = head animationPictures

displayText :: Canvas ()
displayText = translate (0, 100)

data PlayerState = Alive | Disconnected | Dead deriving (Eq, Show, Enum, Bounded)
