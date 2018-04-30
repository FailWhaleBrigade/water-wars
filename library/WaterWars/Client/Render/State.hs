module WaterWars.Client.Render.State
    ( module WaterWars.Core.GameState
    , Animation(..)
    , World(..)
    , WorldSTM(..)
    , RenderInfo(..)
    , WorldInfo(..)
    , initializeState
    , setTerrain
    ) where

import ClassyPrelude
import Graphics.Gloss
import Data.Array.IArray

import Data.List (cycle)

import WaterWars.Client.Render.Terrain.Solid
import WaterWars.Client.Render.Config

import WaterWars.Client.Resources.Block

import qualified WaterWars.Client.Network.State as NetworkState

import qualified WaterWars.Core.GameState as CoreState
import qualified WaterWars.Core.GameMap as CoreState
import qualified WaterWars.Core.DefaultGame as DefaultGame

import WaterWars.Core.GameState
import WaterWars.Core.GameMap

newtype WorldSTM = WorldSTM (TVar World)

data Animation = Animation
    { location :: Location
    , countDownTilNext :: Integer
    , countDownMax :: Integer
    , animationPictures :: [Picture]
    , picInd :: Int
    } deriving Show

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
    , solids :: Seq Solid
    , animation :: Animation
    } deriving Show

data WorldInfo = WorldInfo
    { jump      :: Bool
    , walkLeft  :: Bool
    , walkRight :: Bool
    , shoot     :: Bool
    , duck      :: Bool
    , exitGame  :: Bool
    , player    :: CoreState.InGamePlayer
    , otherPlayers :: Seq CoreState.InGamePlayer
    , projectiles  :: Seq CoreState.Projectile
    } deriving Show

initializeState :: Picture -> Picture -> [Picture] -> BlockMap -> IO WorldSTM
initializeState bmpBg bmpPrj bmpsMan blockMap' = WorldSTM <$> newTVarIO World
    { renderInfo  = RenderInfo
        { blockMap          = blockMap'
        , backgroundTexture = bmpBg
        , projectileTexture = bmpPrj
        , animation         = Animation
            { location          = Location (100, -100)
            , countDownTilNext  = 30
            , countDownMax      = 30
            , animationPictures = cycle bmpsMan
            , picInd            = 0
            }
        , solids            = empty
        }
    , worldInfo   = WorldInfo
        { jump         = False
        , walkLeft     = False
        , walkRight    = False
        , duck         = False
        , shoot        = False
        , exitGame     = False
        , player       = CoreState.InGamePlayer
            { CoreState.playerDescription   = CoreState.Player "Player One"
            , CoreState.playerLocation      = CoreState.Location (0, 0)
            , CoreState.playerMaxHealth     = 100
            , CoreState.playerHealth        = 100
            , CoreState.playerViewDirection = CoreState.Angle 0.0
            , CoreState.playerVelocity      = CoreState.VelocityVector 0.0 0.0
            }
        , otherPlayers = empty
        , projectiles  = fromList
            [DefaultGame.defaultProjectile, DefaultGame.defaultProjectile]
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
