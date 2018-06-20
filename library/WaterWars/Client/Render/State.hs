module WaterWars.Client.Render.State
    ( Animation(..)
    , World(..)
    , WorldSTM(..)
    , RenderInfo(..)
    , WorldInfo(..)
    , PlayerAnimation(..)
    , initializeState
    , setTerrain
    )
where

import           ClassyPrelude
import           Graphics.Gloss
import           Data.Array.IArray

import           Data.List                                ( cycle )

import           WaterWars.Client.Render.Terrain.Solid
import           WaterWars.Client.Render.Config
import           WaterWars.Client.Resources.Block

import qualified WaterWars.Client.Network.State
                                               as NetworkState

import qualified WaterWars.Core.Game.State     as CoreState
import qualified WaterWars.Core.Game.Map       as CoreState

import           WaterWars.Core.Game
import           WaterWars.Client.Render.Animation

newtype WorldSTM = WorldSTM (TVar World)

data World = World
    { renderInfo :: RenderInfo
    , worldInfo :: WorldInfo
    , networkInfo :: Maybe NetworkState.NetworkInfo
    }

data RenderInfo = RenderInfo
    -- TODO: more render information, e.g. Player textures, animation textures, ...
    { blockMap :: BlockMap
    , backgroundTexture :: Picture
    , projectileTexture :: Picture
    , playerRunningTextures :: [Picture]
    , playerIdleTextures :: [Picture]
    , defaultPlayerAnimation :: PlayerAnimation
    , newPlayerIdleAnimation :: PlayerAnimation
    , newPlayerRunnningAnimation :: PlayerAnimation
    , playerAnimations :: Map Player PlayerAnimation
    , solids :: Seq Solid
    , mantaAnimation :: BackgroundAnimation
    }

data WorldInfo = WorldInfo
    { jump      :: Bool
    , walkLeft  :: Bool
    , walkRight :: Bool
    , shoot     :: Maybe Location
    , duck      :: Bool
    , exitGame  :: Bool
    , readyUp   :: Bool
    , countdown :: Maybe Int
    , gameRunning :: Bool
    , player    :: Maybe CoreState.InGamePlayer
    , otherPlayers :: Seq CoreState.InGamePlayer
    , projectiles  :: Seq CoreState.Projectile
    } deriving Show

initializeState
    :: Picture
    -> Picture
    -> Picture
    -> [Picture]
    -> [Picture]
    -> BlockMap
    -> IO WorldSTM
initializeState bmpBg bmpPrj playerTex playerRunningTexs bmpsMan blockMap' =
    WorldSTM <$> newTVarIO World
        { renderInfo  = RenderInfo
            { blockMap                   = blockMap'
            , backgroundTexture          = bmpBg
            , projectileTexture          = scale 0.2 0.2 bmpPrj
            , playerRunningTextures      = playerRunningTexs
            , playerIdleTextures         = singleton playerTex
            , playerAnimations           = mapFromList []
            , defaultPlayerAnimation     = PlayerIdleAnimation Animation
                { countDownTilNext  = 30
                , countDownMax      = 30
                , animationPictures = repeat playerTex
                }
            , newPlayerIdleAnimation     = PlayerIdleAnimation Animation
                { countDownTilNext  = 30
                , countDownMax      = 30
                , animationPictures = repeat playerTex
                }
            , newPlayerRunnningAnimation = PlayerIdleAnimation Animation
                { countDownTilNext  = 5
                , countDownMax      = 5
                , animationPictures = cycle playerRunningTexs
                }
            , mantaAnimation             = BackgroundAnimation
                { animation       = Animation
                    { countDownTilNext  = 30
                    , countDownMax      = 30
                    , animationPictures = cycle bmpsMan
                    }
                , location        = Location (0, 0)
                , updateOperation = mantaUpdateOperation
                , direction       = RightDir
                }
            , solids                     = empty
            }
        , worldInfo   = WorldInfo
            { jump         = False
            , walkLeft     = False
            , walkRight    = False
            , duck         = False
            , shoot        = Nothing
            , exitGame     = False
            , readyUp      = False
            , countdown    = Nothing
            , gameRunning  = False
            , player       = Nothing
            , otherPlayers = empty
            , projectiles  = empty
            }
        , networkInfo = Nothing
        }

setTerrain :: BlockMap -> CoreState.Terrain -> World -> World
setTerrain blockMap terrain World {..} = World
    { renderInfo = renderInfo { solids = fromList blockPositions }
    , ..
    }
  where
    terrainArray = CoreState.terrainBlocks terrain
    (BlockLocation (lowerX, upperX), BlockLocation (lowerY, upperY)) =
        bounds terrainArray
    mapWidth      = fromIntegral (upperX - lowerX) * blockSize
    mapHeight     = fromIntegral (upperY - lowerY) * blockSize
    mapWidthHalf  = mapWidth / 2
    mapHeightHalf = mapHeight / 2

    blockPositions :: [Solid]
    blockPositions = mapMaybe
        (\(loc, block) -> case block of
            NoBlock -> Nothing
            SolidBlock content ->
                blockLocationToSolid mapWidthHalf mapHeightHalf blockSize loc
                    <$> lookup content blockMap
        )
        (assocs terrainArray)

blockLocationToSolid
    :: Float -> Float -> Float -> BlockLocation -> Picture -> Solid
blockLocationToSolid mapWidthHalf mapHeightHalf size (BlockLocation (x, y)) picture
    = Solid
        { solidWidth   = size
        , solidHeight  = size
        , solidCenter  = ( fromIntegral x * size - mapWidthHalf
                         , fromIntegral y * size - mapHeightHalf
                         )
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
