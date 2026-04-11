{-# LANGUAGE TypeOperators #-}
module WaterWars.Server.Action.Util where

import           Effectful
import           Effectful.Reader.Static as Reader
import           WaterWars.Network.Protocol
import           WaterWars.Core.Game

import           WaterWars.Server.Env
import           WaterWars.Server.Events
import           WaterWars.Server.ConnectionMgnt
import Data.Map.Strict (Map)
import Control.Concurrent.STM
import Data.Foldable (forM_)


broadcastMessage
    :: (IOE :> r, Reader Env :> r)
    => ServerMessage
    -> Eff r ()
broadcastMessage serverMessage = do
    session <- Reader.asks (connectionMap . networkEnv)
    forM_ (session :: Map Player Connection)
        $ \conn -> liftIO $ atomically $ writeTQueue (readChannel conn) serverMessage
