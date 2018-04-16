module WaterWars.Client.Render.State where

import ClassyPrelude
import Graphics.Gloss
import Data.Array.IArray

import WaterWars.Core.GameState (Terrain(..), BlockLocation(..))

import WaterWars.Client.Render.Entity.Solid
import WaterWars.Client.Render.Config
import WaterWars.Client.Resources.Block

type Radius = Float

type Position = (Float, Float)

newtype WorldSTM = WorldSTM (TVar World)

data World = World
    { player            :: Player
    , otherPlayers      :: Seq Player
    , backgroundTexture :: Picture
    , blockMap :: BlockMap
    , solids :: Seq Solid
    } deriving Show

data Player = Player
    { playerLoc :: (Float, Float)
    , playerVel :: (Float, Float)
    } deriving (Eq, Show, Read)

initializeState :: Picture -> BlockMap -> IO WorldSTM
initializeState bmp blockMap' = WorldSTM <$> newTVarIO World
    { player            = Player (0, -50) (0, 0)
    , otherPlayers      = empty
    , backgroundTexture = bmp
    , solids            = empty 
    , blockMap          = blockMap'
    }

setTerrain :: BlockMap -> Terrain -> World -> World
setTerrain blockMap terrain world = world { solids = (solids world) ++ fromList blockPositions }
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
            (\(loc, block) -> 
                    blockLocationToSolid mapWidthHalf mapHeightHalf blockSize loc
                        <$> lookup block blockMap
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
