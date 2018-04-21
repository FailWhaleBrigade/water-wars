module WaterWars.Server.ConnectionMgnt where

import ClassyPrelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Error.Class
import System.Log.Logger

import Network.WebSockets

import WaterWars.Network.Protocol

import WaterWars.Core.GameState
import WaterWars.Core.GameAction

import WaterWars.Server.Config

data ServerState = ServerState
    { connections :: Connections
    , gameMap     :: GameMap
    , gameState   :: GameState
    , actions     :: Map Player Action
    }

data Connections = Connections
    { players :: Map Player Connection
    , gameSetup :: Maybe GameSetup
    }

broadcastGameState :: MonadIO m => Connections -> GameState -> m ()
broadcastGameState Connections {..} state = forM_ players $ \conn -> do
    res <- runExceptT $ sendGameState state conn
    case res of
        Left msg ->
            liftIO
                .  warningM networkLoggerName
                $  "Could not send GameState: "
                ++ msg
        Right () -> return ()

sendGameState
    :: (MonadIO m, MonadError String m) => GameState -> Connection -> m ()
sendGameState state conn =
    liftIO $ sendTextData conn (tshow state)
