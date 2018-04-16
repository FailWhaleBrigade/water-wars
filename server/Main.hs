module Main where

import ClassyPrelude
import Network.WebSockets

import Control.Concurrent

import Data.Array.IArray
import Data.List (transpose)

import WaterWars.Core.DefaultGame
import WaterWars.Core.GameState
import WaterWars.Core.Entity.Block

main :: IO ()
main = do
  runServer "localhost" 1234 $ \conn -> do
    connHandle <- acceptRequest conn
    putStrLn "Client connected"
    sendTextData connHandle $ tshow (Map defaultGameMap)
    threadDelay (3 * 1000000)
    sendTextData connHandle $ tshow (Map aGameMap)
    return ()
  return ()

aGameMap :: GameMap
aGameMap = GameMap
  { gameTerrain = terrain
  , gamePlayers = singleton defaultPlayer
  }

terrain :: Terrain
terrain = Terrain
  { terrainBlocks = listArray (BlockLocation (-8, -8), BlockLocation (8, 8))
                              (concat $ transpose $ reverse
                              [ [TopLeftCorner] ++ replicate 15 Ceil ++ [TopRightCorner]
                              , [LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, RightWall]
                              , [LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, RightWall]
                              , [LeftWall, Middle,  Middle,  Middle,  NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, RightWall]
                              , [LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, RightWall]
                              , [LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, RightWall]
                              , [LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, BottomLeftCorner, Floor, BottomRightCorner, NoBlock, NoBlock, NoBlock, NoBlock, RightWall]
                              , [LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, RightWall]
                              , [LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, RightWall]
                              , [LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, RightWall]
                              , [LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, RightWall]
                              , [LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, RightWall]
                              , [LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, RightWall]
                              , [LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, RightWall]
                              , [LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, RightWall]
                              , [LeftWall, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, NoBlock, RightWall]
                              , [BottomLeftCorner] ++ replicate 15 Floor ++ [BottomRightCorner]
                              ] 
                              )
  , terrainBackground = "default"
  }
  