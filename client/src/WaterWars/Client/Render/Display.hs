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

import Control.Monad.IO.Class
import Data.Bifunctor (bimap, Bifunctor (..))
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

type Size = (Double, Double)
type Center = (Double, Double)

render :: Size -> Resources -> AnimationState -> World -> Canvas ()
render dims@(w, h) resources animationState World{..} = do
  renderBackground dims backgroundTexture
  renderEnvironment
  renderManta
  playerPicture
 where
  -- playerPictures

  -- deadPlayerPictures
  -- projectilePictures
  -- readyPicture
  -- serverTextMessage
  -- shootTargetPicture

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
  playerPictures = traverse_ (inGamePlayerToPicture dims resources animationState) livingPlayers

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
    case localPlayer of
      Nothing -> pure ()
      Just p -> do
        case List.find ((== p) . playerDescription) (getInGamePlayers inGamePlayers) of
          Nothing -> pure ()
          Just alive -> do
            inGamePlayerToPicture dims resources animationState alive

  projectilePictures :: Canvas ()
  projectilePictures = traverse_ (projectileToPicture resources animationState) projectiles

  renderEnvironment :: Canvas ()
  renderEnvironment = do
    traverse_ (solidToPicture dims (flip lookupBlockMap blockMap)) solids
    traverse_ (solidToPicture dims (flip lookupDecorationMap decorationMap)) decorations

  renderManta :: Canvas ()
  renderManta = backgroundAnimationToPicture mantaTextures mantaAnimation

  readyPicture :: Canvas ()
  readyPicture = do
    case countdown of
      Nothing -> pure ()
      Just down -> do
        countdownToPicture resources animationState (down - gameTicks)

  shootTargetPicture :: Canvas ()
  shootTargetPicture = do
    case fmap l2d lastShot of
      Nothing -> pure ()
      Just (x, y) -> do
        Canvas.save ()
        Canvas.translate (toDouble (blockSize * x), (blockSize * y))
        Canvas.restore ()

-- circle_ []

renderBackground :: Size -> GameImage -> Canvas ()
renderBackground (w, h) img = do
  Canvas.save ()
  Canvas.drawImage' (image img, 0, 0, w, h)
  Canvas.restore ()

inGamePlayerColor :: CSS.Color
inGamePlayerColor = CSS.red

solidToPicture :: Size -> (a -> GameImage) -> Solid a -> Canvas ()
solidToPicture (w, h) getImage solid = do
  Canvas.save ()
  let
    (x, y) =
      solidCenter solid
        & bimap (+ (w / 2)) (+ (h / 2))
        & invertHeight' h

  Canvas.drawImage' (image $ getImage (solidContent solid), x, y, toDouble blockSize, toDouble blockSize)
  Canvas.restore ()

inGamePlayerToPicture :: Size -> Resources -> AnimationState -> InGamePlayer -> Canvas ()
inGamePlayerToPicture (w, h) Resources{..} AnimationState{..} InGamePlayer{..} = do
  let
    directionComponent = case playerLastRunDirection of
      RunLeft -> -1
      RunRight -> 1
    maybeAnimation = lookupPlayerAnimationMap playerDescription playerAnimations
    animation =
      playerToAnimation $ Maybe.fromMaybe defaultPlayerAnimation maybeAnimation

  Canvas.save ()

  let
    (pw, ph) = (toDouble playerWidth *  blockSize, toDouble playerHeight *  blockSize)
    (x, y) = l2d playerLocation
        & bimap (* blockSize) (* blockSize)
        & bimap (+ (w / 2)) (+ ((h / 2) + (ph / (3 / 2) + (blockSize / 2))))
        & invertHeight' h

  Canvas.drawImage' (image $ getAnimationFrame runningPlayerTextures animation, x, y, pw, ph)
  Canvas.restore ()

deadPlayerToPicture :: Resources -> AnimationState -> DeadPlayer -> Canvas ()
deadPlayerToPicture Resources{..} AnimationState{..} DeadPlayer{..} = do
  let
    maybeAnimation = lookupPlayerAnimationMap deadPlayerDescription playerAnimations
    (x, y) = l2d $ case maybeAnimation of
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

projectileToPicture :: Resources -> AnimationState -> Projectile -> Canvas ()
projectileToPicture Resources{..} AnimationState{..} p = do
  Canvas.save ()
  Canvas.translate ((x * blockSize), (y * blockSize))
  Canvas.drawImage (image projectileTexture, 0, 0)
  Canvas.restore ()
 where
  (x, y) = l2d $ projectileLocation p

countdownToPicture :: Resources -> AnimationState -> Integer -> Canvas ()
countdownToPicture Resources{..} AnimationState{..} tick = do
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
  (x, y) = l2d location

getAnimationFrame :: Vector GameImage -> Animation -> GameImage
getAnimationFrame imgs a =
  imgs Vector.! (animationPictures a `rem` Vector.length imgs)

displayText :: Canvas ()
displayText = Canvas.translate (0, 100)

data PlayerState = Alive | Disconnected | Dead deriving (Eq, Show, Enum, Bounded)
