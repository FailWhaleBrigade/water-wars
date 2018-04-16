module Main where

import ClassyPrelude
import Network

import Control.Concurrent

import Data.Array.IArray
import Data.List (transpose)

import WaterWars.Core.DefaultGame
import WaterWars.Core.GameState
import WaterWars.Core.Entity.Block

main :: IO ()
main = do
  socket <- listenOn $ PortNumber 1234
  (connHandle, clientName, clientPort) <- accept socket
  putStrLn $ tshow clientName ++ " " ++ tshow clientPort
  hPutText connHandle . tshow $ Map defaultGameMap
  threadDelay (3 * 1000000)
  hPutText connHandle . tshow $ Map aGameMap
  hClose connHandle
  sClose socket
  return ()

hPutText :: Handle -> Text -> IO ()
hPutText h = hPut h . encodeUtf8

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
  