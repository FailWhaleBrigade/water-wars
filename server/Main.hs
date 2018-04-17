module Main where

import ClassyPrelude
import Network.WebSockets

import WaterWars.Core.DefaultGame
import WaterWars.Core.GameState

main :: IO ()
main = do
  runServer "localhost" 1234 $ \conn -> do
    connHandle <- acceptRequest conn
    putStrLn "Client connected"
    sendTextData connHandle $ tshow (Map defaultGameMap)
    return ()
  return ()