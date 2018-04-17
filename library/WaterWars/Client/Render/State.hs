module WaterWars.Client.Render.State where

import ClassyPrelude
import Graphics.Gloss
import Data.Array.IArray

import WaterWars.Core.GameState (Terrain(..), BlockLocation(..))

import WaterWars.Client.Render.Terrain.Solid
import WaterWars.Client.Render.Config
import WaterWars.Client.Resources.Block

import qualified WaterWars.Client.Network.State as NetworkState

import qualified WaterWars.Core.GameState as CoreState

type Radius = Float

type Position = (Float, Float)

newtype WorldSTM = WorldSTM (TVar World)

data World = World
    { renderInfo  :: RenderInfo
    , worldInfo   :: WorldInfo
    , networkInfo :: Maybe NetworkState.NetworkInfo
    }

data RenderInfo = RenderInfo
    { blockMap          :: BlockMap
    , backgroundTexture :: Picture
    , solids            :: Seq Solid
    } deriving Show

data WorldInfo = WorldInfo
    { jump      :: Bool
    , walkLeft  :: Bool
    , walkRight :: Bool
    , shoot     :: Bool
    , exitGame  :: Bool
    , player    :: Player
    , otherPlayers :: Seq Player
    } deriving Show

data Player = Player
    { playerLoc :: (Float, Float)
    , playerVel :: (Float, Float)
    } deriving (Eq, Show, Read)

initializeState :: Picture -> BlockMap -> IO WorldSTM
initializeState bmp blockMap' = WorldSTM <$> newTVarIO World
    { renderInfo  = RenderInfo
        { blockMap          = blockMap'
        , backgroundTexture = bmp
        , solids            = empty
        }
    , worldInfo   = WorldInfo
        { jump      = False
        , walkLeft  = False
        , walkRight = False
        , shoot     = False
        , exitGame  = False
        , player    = Player (0,0) (0,0)
        , otherPlayers = empty
        }
    , networkInfo = Nothing
    }

setTerrain :: BlockMap -> Terrain -> World -> World
setTerrain blockMap terrain World {..} = World { renderInfo = renderInfo { solids = fromList blockPositions }, .. }
  where
    terrainArray = terrainBlocks terrain
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
        , solidCenter  = ( (fromIntegral x) * size - mapWidthHalf
                         , (fromIntegral y) * size - mapHeightHalf
                         )
        , solidTexture = picture
        }


