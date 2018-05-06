module WaterWars.Server.EventLoop where

import ClassyPrelude

import System.Log.Logger

import WaterWars.Network.Protocol

import WaterWars.Core.GameState
import WaterWars.Server.ConnectionMgnt

eventLoop :: MonadIO m => TChan (ClientMessage, Player) -> TMVar PlayerActions -> m ()
eventLoop broadcastChan tmvar = forever $ do 
    (clientMsg, player) <- atomically $ readTChan broadcastChan
    liftIO $ debugM "Server.Connection" ("Successfully read a message from: " ++ show player)
    case clientMsg of 
        LoginMessage login -> undefined
        LogoutMessage logout -> undefined
        GameSetupMessage gameSetup -> undefined
        PlayerActionMessage playerAction -> atomically $ do
            PlayerActions {..} <- takeTMVar tmvar
            let getPlayerActions' = insertWith (++) player (getAction playerAction) getPlayerActions 
            putTMVar tmvar (PlayerActions getPlayerActions')
    return ()