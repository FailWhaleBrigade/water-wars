module WaterWars.Client.Render.State
    ( Animation(..)
    , World(..)
    , WorldSTM(..)
    , RenderInfo(..)
    , WorldInfo(..)
    , PlayerAnimation(..)
    , ServerUpdate(..)
    , initializeState
    , setTerrain
    , module WaterWars.Client.Resources.Resources
    )
where

import           ClassyPrelude
import           Graphics.Gloss
import           Data.Array.IArray
import           Data.Maybe (fromJust)

import           Data.List                                ( cycle )

import           WaterWars.Client.Render.Terrain.Solid
import           WaterWars.Client.Render.Config
import           WaterWars.Client.Resources.Block
import           WaterWars.Client.Resources.Resources

import qualified WaterWars.Client.Network.State
                                               as NetworkState

import qualified WaterWars.Core.Game     as CoreState
import           WaterWars.Core.Game

import           WaterWars.Client.Render.Animation

newtype WorldSTM = WorldSTM (TVar World)

data World = World
    { renderInfo :: RenderInfo
    , worldInfo :: WorldInfo
    , lastGameUpdate :: ServerUpdate
    , networkInfo :: Maybe NetworkState.NetworkInfo
    }

data RenderInfo = RenderInfo
    { resources :: Resources
    , defaultPlayerAnimation :: PlayerAnimation
    , newPlayerIdleAnimation :: PlayerAnimation
    , newPlayerRunnningAnimation :: PlayerAnimation
    , newPlayerDeathAnimation :: PlayerAnimation
    , playerAnimations :: Map Player PlayerAnimation
    , connectingAnimation :: Animation
    , mantaAnimation :: BackgroundAnimation
    , solids :: Seq Solid
    , decorations :: Seq Solid
    }

newtype ServerUpdate = ServerUpdate
    { gameStateUpdate :: CoreState.GameState
    } deriving (Eq, Show)

data WorldInfo = WorldInfo
    { jump      :: Bool
    , walkLeft  :: Bool
    , walkRight :: Bool
    , shoot     :: Maybe Location
    , lastShot  :: Maybe Location
    , duck      :: Bool
    , exitGame  :: Bool
    , readyUp   :: Bool
    -- TODO: Everything beneath should be refactored into another datatype
    , countdown :: Maybe Integer
    , gameRunning :: Bool
    , localPlayer :: Maybe Player
    , projectiles  :: Seq CoreState.Projectile
    } deriving Show

initializeState :: Resources -> IO WorldSTM
initializeState resources@Resources {..} = WorldSTM <$> newTVarIO World
    { renderInfo     = RenderInfo
        { resources                  = resources
        , playerAnimations           = mapFromList []
        , defaultPlayerAnimation     = PlayerIdleAnimation Animation
            { countDownTilNext  = 30
            , countDownMax      = 30
            , animationPictures = repeat idlePlayerTexture
            }
        , newPlayerIdleAnimation     = PlayerIdleAnimation Animation
            { countDownTilNext  = 30
            , countDownMax      = 30
            , animationPictures = repeat idlePlayerTexture
            }
        , newPlayerRunnningAnimation = PlayerIdleAnimation Animation
            { countDownTilNext  = 5
            , countDownMax      = 5
            , animationPictures = cycle runningPlayerTextures
            }
        , newPlayerDeathAnimation    = PlayerDeathAnimation BackgroundAnimation
            { animation       = Animation
                { countDownTilNext  = 9
                , countDownMax      = 9
                , animationPictures = (take 2 playerDeathTextures)
                    ++ (cycle (drop 2 playerDeathTextures))
                }
            , location        = Location (0, 0) -- default location
            , updateOperation = deadPlayerUpdateOperation
            , direction       = RightDir
            }
        , mantaAnimation             = BackgroundAnimation
            { animation       = Animation
                { countDownTilNext  = 30
                , countDownMax      = 30
                , animationPictures = cycle mantaTextures
                }
            , location        = Location (0, 0)
            , updateOperation = mantaUpdateOperation
            , direction       = RightDir
            }
        , solids                     = empty
        , decorations                = empty
        , connectingAnimation        = Animation
            { countDownTilNext  = 60
            , countDownMax      = 60
            , animationPictures = cycle connectingTextures
            }
        }
    , worldInfo      = WorldInfo
        { jump        = False
        , walkLeft    = False
        , walkRight   = False
        , duck        = False
        , shoot       = Nothing
        , lastShot    = Nothing
        , exitGame    = False
        , readyUp     = False
        , countdown   = Nothing
        , gameRunning = False
        , localPlayer = Nothing
        , projectiles = empty
        }
    , networkInfo    = Nothing
    , lastGameUpdate = ServerUpdate
        { gameStateUpdate = GameState
            { inGamePlayers   = InGamePlayers empty
            , gameDeadPlayers = DeadPlayers empty
            , gameProjectiles = Projectiles empty
            , gameTicks       = 0
            }
        }
    }

setTerrain :: CoreState.TerrainDecoration -> CoreState.Terrain -> World -> World
setTerrain decoration terrain World {..} = World
    { renderInfo = renderInfo
        { solids      = fromList (blockPositions terrainArray blockMap')
        , decorations = fromList
            (decorationPositions (terrainDecorationArray decoration)
                                 decorationMap'
            )
        }
    , ..
    }
  where
    blockMap'      = blockMap $ resources renderInfo
    decorationMap' = decorationMap $ resources renderInfo
    terrainArray   = CoreState.terrainBlocks terrain

    blockPositions
        :: Array BlockLocation Block -> Map BlockContent Picture -> [Solid]
    blockPositions locationMap pictureMap = mapMaybe
        (\(loc, block) -> case block of
            NoBlock -> Nothing
            SolidBlock content ->
                blockLocationToSolid blockSize loc <$> lookup content pictureMap
        )
        (assocs locationMap)
    decorationPositions
        :: Array BlockLocation [Decoration] -> Map Decoration Picture -> [Solid]
    decorationPositions locationMap pictureMap = concatMap
        (\(loc, deco) -> do
            decorationElement <- deco
            let picture = lookup decorationElement pictureMap
            guard (isJust picture)
            return $ blockLocationToSolid blockSize loc (fromJust picture)
        )
        (assocs locationMap)

blockLocationToSolid :: Float -> BlockLocation -> Picture -> Solid
blockLocationToSolid size (BlockLocation (x, y)) picture = Solid
    { solidWidth   = size
    , solidHeight  = size
    , solidCenter  = (fromIntegral x * size, fromIntegral y * size)
    , solidTexture = picture
    }

mantaUpdateOperation :: BackgroundAnimation -> BackgroundAnimation
mantaUpdateOperation ba@BackgroundAnimation {..} = ba
    { location  = Location (newX, newY)
    , direction = dir
    }
  where
    Location (x, _) = location
    dir | (direction == RightDir) && (x >= fieldWidth + 60) = LeftDir
        | (direction == LeftDir) && (x <= -fieldWidth - 60) = RightDir
        | otherwise = direction
    newX = case dir of
        RightDir -> x + 0.5
        LeftDir  -> x - 0.5
    newY = 10 * sin (x / 15)

deadPlayerUpdateOperation :: BackgroundAnimation -> BackgroundAnimation
deadPlayerUpdateOperation BackgroundAnimation {..} = BackgroundAnimation
    { location = Location (x, newY)
    , ..
    }
  where
    Location (x, y) = location
    newY            = y + 0.05




