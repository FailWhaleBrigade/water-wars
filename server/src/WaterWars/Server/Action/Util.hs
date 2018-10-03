module WaterWars.Server.Action.Util where

import           ClassyPrelude           hiding ( Reader
                                                , ask
                                                )
import           Control.Eff
import           Control.Eff.Reader.Strict
import           Control.Eff.Lift
import           WaterWars.Network.Protocol
import           WaterWars.Server.Env
import           WaterWars.Server.ConnectionMgnt


broadcastMessage
    :: (Member (Reader Env) r, MonadIO m, Lifted m r)
    => ServerMessage
    -> Eff r ()
broadcastMessage serverMessage = do
    sessionMap' <- reader (connectionMapTvar . networkEnv)
    session     <- readTVarIO sessionMap'
    forM_ (session :: Map Text ClientConnection)
        $ \conn -> atomically $ writeTQueue (readChannel conn) serverMessage
