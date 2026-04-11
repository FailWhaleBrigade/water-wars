module WaterWars.Server.Action.Start where

import           Effectful
import           Effectful.Reader.Static as Reader
import           Effectful.Log
import           WaterWars.Core.Game
import           WaterWars.Network.Protocol
import           WaterWars.Server.Env
import           WaterWars.Server.Action.Util
import qualified Data.Text as Text


startGame
    :: (Log :> r, Reader Env :> r, IOE :> r)
    => Eff r ()
startGame = do
    ServerEnv {..} <- Reader.asks serverEnv
    let gameTick = gameTicks . gameState $ gameLoop
    logTrace_ $ "Send the Game start message: " <> Text.show gameTick

    broadcastMessage GameStartMessage

