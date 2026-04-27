
module WaterWars.Client.Render.State (
  Animation (..),
  World (..),
  WorldSTM (..),
  AnimationState (..),
  WorldInfo (..),
  PlayerAnimation (..),
  PlayerAnimationMap (..),
  lookupPlayerAnimationMap,
  ServerUpdate (..),
  emptyWorld,
  newAnimationState,
  setTerrain,
  module WaterWars.Client.Resources.Resources,
)
where

import Data.Array.IArray

import WaterWars.Client.Render.Config
import WaterWars.Client.Render.Terrain.Solid
import WaterWars.Client.Resources.Block
import WaterWars.Client.Resources.Resources

import WaterWars.Core.Game
import qualified WaterWars.Core.Game as CoreState

import Control.Concurrent.STM.TVar (TVar)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import GHC.Generics (Generic)
import WaterWars.Client.Render.Animation

newtype WorldSTM = WorldSTM (TVar World)

data World = World
  { worldInfo :: WorldInfo
  , lastGameUpdate :: ServerUpdate
  }
  deriving (Generic, Eq)

data AnimationState = AnimationState
  { defaultPlayerAnimation :: PlayerAnimation
  , newPlayerIdleAnimation :: PlayerAnimation
  , newPlayerRunnningAnimation :: PlayerAnimation
  , newPlayerDeathAnimation :: PlayerAnimation
  , playerAnimations :: PlayerAnimationMap
  , connectingAnimation :: Animation
  , mantaAnimation :: BackgroundAnimation
  , solids :: [Solid BlockContent]
  , decorations :: [Solid Decoration]
  }
  deriving (Generic, Eq)

newtype PlayerAnimationMap = PlayerAnimationMap {getPlayerAnimationMap :: Map Player PlayerAnimation}
  deriving (Generic, Eq)

lookupPlayerAnimationMap :: Player -> PlayerAnimationMap -> Maybe PlayerAnimation
lookupPlayerAnimationMap val pm = Map.lookup val (getPlayerAnimationMap pm)

newtype ServerUpdate = ServerUpdate
  { gameStateUpdate :: CoreState.GameState
  }
  deriving (Eq, Show, Generic)

data WorldInfo = WorldInfo
  { jump :: Bool
  , walkLeft :: Bool
  , walkRight :: Bool
  , shoot :: Maybe Location
  , lastShot :: Maybe Location
  , duck :: Bool
  , exitGame :: Bool
  , readyUp :: Bool
  , -- TODO: Everything beneath should be refactored into another datatype
    countdown :: Maybe Integer
  , gameRunning :: Bool
  , localPlayer :: Maybe Player
  , winnerPlayer :: Maybe Player
  , projectiles :: [CoreState.Projectile]
  }
  deriving (Show, Generic, Eq)

newAnimationState :: AnimationState
newAnimationState =
  AnimationState
    { playerAnimations = PlayerAnimationMap Map.empty
    , defaultPlayerAnimation =
        PlayerIdleAnimation
          Animation
            { countDownTilNext = 30
            , countDownMax = 30
            , animationPictures = 0
            -- , animationPictures = [idlePlayerTexture]
            }
    , newPlayerIdleAnimation =
        PlayerIdleAnimation
          Animation
            { countDownTilNext = 30
            , countDownMax = 30
            , animationPictures = 0
            -- , animationPictures = [idlePlayerTexture]
            }
    , newPlayerRunnningAnimation =
        PlayerIdleAnimation
          Animation
            { countDownTilNext = 5
            , countDownMax = 5
            , animationPictures = 0
            -- , animationPictures = runningPlayerTextures
            }
    , newPlayerDeathAnimation =
        PlayerDeathAnimation
          BackgroundAnimation
            { animation =
                Animation
                  { countDownTilNext = 9
                  , countDownMax = 9
                  , animationPictures = 0
                  -- , animationPictures =
                  --     (take 2 playerDeathTextures)
                  --       ++ (drop 2 playerDeathTextures)
                  }
            , location = Location (0, 0) -- default location
            -- , updateOperation = deadPlayerUpdateOperation
            , direction = RightDir
            }
    , mantaAnimation =
        BackgroundAnimation
          { animation =
              Animation
                { countDownTilNext = 30
                , countDownMax = 30
                , animationPictures = 0
                -- , animationPictures = mantaTextures
                }
          , location = Location (0, 0)
          -- , updateOperation = mantaUpdateOperation
          , direction = RightDir
          }
    , solids = []
    , decorations = []
    , connectingAnimation =
        Animation
          { countDownTilNext = 60
          , countDownMax = 60
          , animationPictures = 0
          -- , animationPictures = connectingTextures
          }
    }

emptyWorld :: World
emptyWorld =
  World
    { worldInfo =
        WorldInfo
          { jump = False
          , walkLeft = False
          , walkRight = False
          , duck = False
          , shoot = Nothing
          , lastShot = Nothing
          , exitGame = False
          , readyUp = False
          , countdown = Nothing
          , gameRunning = False
          , localPlayer = Nothing
          , winnerPlayer = Nothing
          , projectiles = []
          }
    , -- , networkInfo    = Nothing
      lastGameUpdate =
        ServerUpdate
          { gameStateUpdate =
              GameState
                { inGamePlayers = InGamePlayers []
                , gameDeadPlayers = DeadPlayers []
                , gameProjectiles = Projectiles []
                , gameTicks = 0
                }
          }
    }

setTerrain :: CoreState.TerrainDecoration -> CoreState.Terrain -> AnimationState -> AnimationState
setTerrain decoration terrain animationState =
  animationState
    { solids = blockPositions terrainArray
    , decorations =
        decorationPositions
          (terrainDecorationArray decoration)
    }
 where
  terrainArray = CoreState.terrainBlocks terrain

  blockPositions ::
    Array BlockLocation Block -> [Solid BlockContent]
  blockPositions locationMap =
    Maybe.mapMaybe
      ( \(loc, block) -> case block of
          NoBlock -> Nothing
          SolidBlock content ->
            Just $ blockLocationToSolid blockSize loc content
      )
      (assocs locationMap)
  decorationPositions ::
    Array BlockLocation [Decoration] -> [Solid Decoration]
  decorationPositions locationMap =
    concatMap
      ( \(loc, deco) -> do
          decorationElement <- deco
          return $ blockLocationToSolid blockSize loc decorationElement
      )
      (assocs locationMap)

blockLocationToSolid :: Float -> BlockLocation -> a -> Solid a
blockLocationToSolid size (BlockLocation (x, y)) a =
  Solid
    { solidWidth = size
    , solidHeight = size
    , solidCenter = (fromIntegral x * size, fromIntegral y * size)
    , solidContent = a
    }

mantaUpdateOperation :: BackgroundAnimation -> BackgroundAnimation
mantaUpdateOperation ba@BackgroundAnimation{..} =
  ba
    { location = Location (newX, newY)
    , direction = dir
    }
 where
  Location (x, _) = location
  dir
    | (direction == RightDir) && (x >= fieldWidth + 60) = LeftDir
    | (direction == LeftDir) && (x <= -fieldWidth - 60) = RightDir
    | otherwise = direction
  newX = case dir of
    RightDir -> x + 0.5
    LeftDir -> x - 0.5
  newY = 10 * sin (x / 15)

