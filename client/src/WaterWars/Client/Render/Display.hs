{-# LANGUAGE TypeApplications #-}

module WaterWars.Client.Render.Display where

import WaterWars.Client.Render.Animation
import WaterWars.Client.Render.Config
import WaterWars.Client.Render.State
import WaterWars.Client.Render.Terrain.Solid
import WaterWars.Client.Resources.Block
import WaterWars.Client.Resources.Image (GameImage (image))
import WaterWars.Core.Game
import WaterWars.Core.Game.Constants

import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor (Bifunctor (..), bimap)
import Data.Foldable (traverse_)
import qualified Data.Foldable as Foldable
import Data.Function
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Miso
import qualified Miso.CSS.Color as CSS
import Miso.Canvas (Canvas)
import qualified Miso.Canvas as Canvas
import WaterWars.Client.Render.Utils
import WaterWars.Client.World
import Miso.Prelude (Image)

render :: Resources -> AnimationState -> World -> Canvas ()
render resources animationState World{..} = do
  renderBackground backgroundTexture
  renderEnvironment
  -- renderManta
  playerPicture
  playerPictures
  projectilePictures
  shootTargetPicture
 where
  -- deadPlayerPictures
  -- readyPicture
  -- serverTextMessage

  AnimationState{..} = animationState
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
  playerPictures = traverse_ (inGamePlayerToPicture resources animationState) livingPlayers

  deadPlayerPictures :: Canvas ()
  deadPlayerPictures = traverse_ (deadPlayerToPicture resources animationState) deadPlayers

  stateOf :: Maybe Player -> PlayerState
  stateOf Nothing = Disconnected
  stateOf (Just p) = case List.find ((== p) . playerDescription) livingPlayers of
    Just _ -> Alive
    Nothing -> Dead

  serverTextMessage :: Canvas ()
  serverTextMessage = do
    Canvas.save ()
    case stateOf localPlayer of
      Disconnected -> do
        displayText
        Canvas.drawImage (image $ getAnimationFrame connectingTextures connectingAnimation, 0, 0)
      Dead -> do
        displayText
        Canvas.drawImage (image youLostTexture, 0, 0)
      Alive
        | localPlayer == winnerPlayer -> do
            displayText
            Canvas.drawImage (image youWinTexture, 0, 0)
      _ -> pure ()
    Canvas.restore ()

  playerPicture :: Canvas ()
  playerPicture = do
    case currentPlayerLocation inGamePlayers localPlayer of
      Nothing -> pure ()
      Just alive -> do
        inGamePlayerToPicture resources animationState alive

  projectilePictures :: Canvas ()
  projectilePictures = traverse_ (projectileToPicture resources) projectiles

  renderEnvironment :: Canvas ()
  renderEnvironment = do
    traverse_ (solidToPicture (`lookupBlockMap` blockMap)) solids
    traverse_ (solidToPicture (`lookupDecorationMap` decorationMap)) decorations

  renderManta :: Canvas ()
  renderManta = backgroundAnimationToPicture mantaTextures mantaAnimation

  readyPicture :: Canvas ()
  readyPicture = do
    case countdown of
      Nothing -> pure ()
      Just down -> do
        countdownToPicture resources (down - gameTicks)

  shootTargetPicture :: Canvas ()
  shootTargetPicture = do
    case lastShot of
      Nothing -> pure ()
      Just (RealLocation (x, y)) -> do
        drawImageOrigCenter projectileTexture (x, y) (0.5, 0.5)

renderBackground :: GameImage -> Canvas ()
renderBackground img = do
  -- Canvas.drawImage' (image img, -5, 5, 10, 10)
  drawImageOrigCenter img (0, 0) (40, 25)

inGamePlayerColor :: CSS.Color
inGamePlayerColor = CSS.red

solidToPicture :: (a -> GameImage) -> Solid a -> Canvas ()
solidToPicture getImage solid = do
  let LogicalLocation (x, y) = solidCenter solid
  drawImageOrigCenter (getImage $ solidContent solid) (fromIntegral x, fromIntegral y) (1, 1)
  -- Canvas.drawImage' (image $ getImage (solidContent solid), x, y, 1, 1)

flipImage :: RunDirection -> Canvas ()
flipImage = \case
  RunRight -> do
    pure ()
  RunLeft -> do
    Canvas.scale (-1, 1)

drawImageOrigCenter :: GameImage -> (Double, Double) -> (Double, Double) -> Canvas ()
drawImageOrigCenter img (cx, cy) (w, h) = do
  Canvas.save ()
  Canvas.scale (1, -1)
  Canvas.drawImage' (image img, cx - w / 2, - cy + h / 2, w, -h)
  Canvas.restore ()

drawImageOrigBottomCenter :: GameImage -> RunDirection -> (Double, Double) -> (Double, Double) -> Canvas ()
drawImageOrigBottomCenter img dir (cx, cy) (w, h) = do
  Canvas.save ()
  Canvas.translate (cx, cy)
  case dir of
    RunRight -> do
      Canvas.scale (1, -1)
    RunLeft -> do
      Canvas.scale (-1, -1)

  Canvas.drawImage' (image img, - w / 2, 0, w, -h)
  Canvas.restore ()

inGamePlayerToPicture :: Resources -> AnimationState -> InGamePlayer -> Canvas ()
inGamePlayerToPicture Resources{..} AnimationState{..} InGamePlayer{..} = do
  let
    maybeAnimation = lookupPlayerAnimationMap playerDescription playerAnimations
    animation =
      playerToAnimation $ Maybe.fromMaybe defaultPlayerAnimation maybeAnimation
    RealLocation (x, y) = l2rl playerLocation

  drawImageOrigBottomCenter (getAnimationFrame runningPlayerTextures animation)
    playerLastRunDirection
    (x, y)
    (toDouble playerWidth, toDouble playerHeight)

deadPlayerToPicture :: Resources -> AnimationState -> DeadPlayer -> Canvas ()
deadPlayerToPicture Resources{..} AnimationState{..} DeadPlayer{..} = do
  let
    maybeAnimation = lookupPlayerAnimationMap deadPlayerDescription playerAnimations
    RealLocation (x, y) = l2rl $ case maybeAnimation of
      Just (PlayerDeathAnimation ba) -> location ba
      _ -> deadPlayerLocation
    a =
      playerToAnimation $
        Maybe.fromMaybe defaultPlayerAnimation maybeAnimation

  Canvas.save ()
  Canvas.scale (toDouble (1 / mermaidWidth), toDouble (1 / mermaidHeight))
  Canvas.scale (toDouble blockSize, toDouble blockSize)
  Canvas.scale (toDouble defaultPlayerWidth, toDouble defaultPlayerHeight)
  Canvas.translate (toDouble (blockSize * x), toDouble (blockSize * y + blockSize * toDouble defaultPlayerHeight / 2))
  Canvas.drawImage (image $ getAnimationFrame playerDeathTextures a, 0, 0)
  Canvas.restore ()

projectileToPicture :: Resources -> Projectile -> Canvas ()
projectileToPicture Resources{..} p = do
  let
    RealLocation (x, y) =
      l2rl (projectileLocation p)

  drawImageOrigCenter projectileTexture (x, y) (0.5, 0.5)

countdownToPicture :: Resources -> Integer -> Canvas ()
countdownToPicture Resources{..} tick = do
  Canvas.save ()
  displayText
  Canvas.drawImage (image pic, 0, 0)
  Canvas.restore ()
 where
  pic
    | tick >= 180 = countdownTextures Vector.! 0
    | tick >= 120 = countdownTextures Vector.! 1
    | tick >= 60 = countdownTextures Vector.! 2
    | otherwise {- tick >= 0 -} =
        countdownTextures Vector.! 3

backgroundAnimationToPicture :: Vector GameImage -> BackgroundAnimation -> Canvas ()
backgroundAnimationToPicture texs BackgroundAnimation{..} = do
  Canvas.save ()
  Canvas.scale scaleFactor
  Canvas.drawImage' (image pic, x, y, 64, 64)
  Canvas.restore ()
 where
  scaleFactor = case direction of
    RightDir -> (-1, 1)
    LeftDir -> (1, 1)
  pic = getAnimationFrame texs animation
  RealLocation (x, y) = l2rl location

getAnimationFrame :: Vector GameImage -> Animation -> GameImage
getAnimationFrame imgs a =
  imgs Vector.! (animationPictures a `rem` Vector.length imgs)

displayText :: Canvas ()
displayText = Canvas.translate (0, 100)

data PlayerState = Alive | Disconnected | Dead deriving (Eq, Show, Enum, Bounded)
