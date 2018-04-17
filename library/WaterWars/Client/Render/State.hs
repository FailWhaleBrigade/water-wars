module WaterWars.Client.Render.State
    ( module WaterWars.Core.GameState
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

import WaterWars.Client.Render.Terrain.Solid
import WaterWars.Client.Render.Config

import WaterWars.Client.Resources.Block

import qualified WaterWars.Client.Network.State as NetworkState

import qualified WaterWars.Core.GameState as CoreState
import WaterWars.Core.GameState

newtype WorldSTM = WorldSTM (TVar World)

data World = World
    { renderInfo  :: RenderInfo
    , worldInfo   :: WorldInfo
    , networkInfo :: Maybe NetworkState.NetworkInfo
    }

data RenderInfo = RenderInfo
    -- TODO: more render information, e.g. Player textures, animation textures, ...
    { blockMap          :: BlockMap
    , backgroundTexture :: Picture
    , solids            :: Seq Solid
    } deriving Show

data WorldInfo = WorldInfo
    { jump      :: Bool
    , walkLeft  :: Bool
    , walkRight :: Bool
    , shoot     :: Bool
    , duck      :: Bool
    , exitGame  :: Bool
    , player    :: CoreState.InGamePlayer -- TODO: should use Player from WaterWars.Core.GameState
    , otherPlayers :: Seq CoreState.InGamePlayer
    , projectiles :: CoreState.Projectiles
    } deriving Show

initializeState :: Picture -> BlockMap -> IO WorldSTM
initializeState bmp blockMap' = WorldSTM <$> newTVarIO World
    { renderInfo  = RenderInfo
        { blockMap          = blockMap'
        , backgroundTexture = bmp
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
            { CoreState.playerDescription   = CoreState.Player "unknown"
            , CoreState.playerLocation      = CoreState.Location (0, 0)
            , CoreState.playerMaxHealth     = 100
            , CoreState.playerHealth        = 100
            , CoreState.playerViewDirection = CoreState.Angle 0.0
            , CoreState.playerVelocity = CoreState.VelocityVector 0.0 0.0
            }
        , otherPlayers = empty
        , projectiles  = CoreState.Projectiles empty
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
