module WaterWars.Server.Action.Start where

import           ClassyPrelude           hiding ( Reader
                                                , asks
                                                )
import           Effectful
import           Effectful.Reader.Static as Reader
import           Effectful.Log
import           WaterWars.Core.Game
import           WaterWars.Network.Protocol
import           WaterWars.Server.Env
import           WaterWars.Server.Action.Util


startGame
    :: (Log :> r, Reader Env :> r, IOE :> r)
    => Eff r ()
startGame = do
    ServerEnv {..} <- Reader.asks serverEnv
    let gameTick = gameTicks . gameState $ gameLoop
    logTrace_ $ "Send the Game start message: " ++ tshow gameTick

    broadcastMessage GameStartMessage

