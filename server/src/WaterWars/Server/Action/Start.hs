module WaterWars.Server.Action.Start where

import           ClassyPrelude           hiding ( Reader
                                                , ask
                                                )
import           Control.Eff
import           Control.Eff.Reader.Strict
import           Control.Eff.Lift
import           Control.Eff.Log
import qualified Control.Eff.Log               as EffLog
import           WaterWars.Core.Game
import           WaterWars.Network.Protocol
import           WaterWars.Server.Env
import           WaterWars.Server.Action.Util


startGameCallback
    :: (Member (Log Text) r, Member (Reader Env) r, MonadIO m, Lifted m r)
    => Eff r ()
startGameCallback = do
    ServerEnv {..} <- reader serverEnv
    let gameTick = gameTicks . gameState $ gameLoop
    EffLog.logE $ "Send the Game start message: " ++ tshow gameTick

    broadcastMessage GameStartMessage

