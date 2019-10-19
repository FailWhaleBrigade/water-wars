module WaterWars.Server.Action.Start where

import           ClassyPrelude           hiding ( Reader
                                                , ask
                                                )
import           Control.Eff
import           Control.Eff.Reader.Strict
import           Control.Eff.Lift
import           WaterWars.Core.Game
import           WaterWars.Network.Protocol
import           WaterWars.Server.Env
import           WaterWars.Server.Action.Util


startGame
    :: (Member (Reader Env) r, MonadIO m, Lifted m r)
    => Eff r ()
startGame = do
    ServerEnv {..} <- reader serverEnv
    let gameTick = gameTicks . gameState $ gameLoop

    broadcastMessage GameStartMessage
