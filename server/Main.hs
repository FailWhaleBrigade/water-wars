module Main where

import ClassyPrelude
import Network
import WaterWars.Core.DefaultGame
import WaterWars.Core.GameState

main :: IO ()
main = do
  socket <- listenOn $ PortNumber 1234
  (connHandle, clientName, clientPort) <- accept socket
  putStrLn $ tshow clientName ++ " " ++ tshow clientPort
  hPutText connHandle . tshow $ Map defaultGameMap
  hClose connHandle
  sClose socket
  return ()

hPutText :: Handle -> Text -> IO ()
hPutText h = hPut h . encodeUtf8
