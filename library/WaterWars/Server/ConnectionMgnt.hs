module WaterWars.Server.ConnectionMgnt where

import ClassyPrelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Error.Class
import System.Log.Logger


import Network.WebSockets

import WaterWars.Core.GameState
import WaterWars.Network.Protocol

-- TODO: move this data-definition to connections-management
data Connections = Connections
    { players :: Map Text PlayerSession
    , gameSetup :: Maybe GameSetup
    }


data PlayerSession = PlayerSession
    { name :: Text
    , conn :: Connection
    }

broadcastGameState :: MonadIO m => Connections -> GameState -> m ()
broadcastGameState Connections {..} state = forM_ players $ \player -> do
    res <- runExceptT $ sendGameState state player
    case res of
        Left msg ->
            liftIO
                .  warningM "Server.Connection"
                $  "Could not new GameState: "
                ++ show (name player)
                ++ ": "
                ++ msg
        Right () -> return ()

sendGameState
    :: (MonadIO m, MonadError String m) => GameState -> PlayerSession -> m ()
sendGameState state PlayerSession {..} =
    liftIO $ sendTextData conn (tshow state)
