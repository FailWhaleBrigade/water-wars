{-# LANGUAGE TypeOperators #-}
module WaterWars.Server.Action.Util where

import           ClassyPrelude           hiding ( Reader
                                                , ask
                                                )
import           Effectful
import           Effectful.Reader.Static as Reader
import           WaterWars.Network.Protocol
import           WaterWars.Core.Game

import           WaterWars.Server.Env
import           WaterWars.Server.Events
import           WaterWars.Server.ConnectionMgnt


broadcastMessage
    :: (IOE :> r, Reader Env :> r)
    => ServerMessage
    -> Eff r ()
broadcastMessage serverMessage = do
    session <- Reader.asks (connectionMap . networkEnv)
    forM_ (session :: Map Player Connection)
        $ \conn -> atomically $ writeTQueue (readChannel conn) serverMessage
