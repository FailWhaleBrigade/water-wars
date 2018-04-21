module WaterWars.Server.ConnectionMgnt where

import ClassyPrelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Error.Class
import System.Log.Logger

import Network.WebSockets

import WaterWars.Core.GameState
import WaterWars.Network.Protocol

import WaterWars.Server.Config

data Connections = Connections
    { players :: Map Text PlayerSession
    , gameSetup :: Maybe GameSetup
    } deriving (Show, Eq)


data PlayerSession = PlayerSession
    { name :: Text
    , conn :: Connection
    } 

instance Eq PlayerSession where
    (==) p1 p2 = name p1 == name p2

instance Show PlayerSession where
    show = show . name

broadcastGameState :: MonadIO m => Connections -> GameState -> m ()
broadcastGameState Connections {..} state = forM_ players $ \player -> do
    res <- runExceptT $ sendGameState state player
    case res of
        Left msg ->
            liftIO
                .  warningM networkLoggerName
                $  "Could not new GameState: "
                ++ show (name player)
                ++ ": "
                ++ msg
        Right () -> return ()

sendGameState
    :: (MonadIO m, MonadError String m) => GameState -> PlayerSession -> m ()
sendGameState state PlayerSession {..} =
    liftIO $ sendTextData conn (tshow state)
