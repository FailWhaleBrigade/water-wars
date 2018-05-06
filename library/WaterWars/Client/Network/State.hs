{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
module WaterWars.Client.Network.State where

import ClassyPrelude

import qualified Network.WebSockets as WS
import WaterWars.Network.Connection
import WaterWars.Network.Protocol as Protocol

data NetworkConfig = NetworkConfig
    { portId   :: Int
    , hostName :: String
    } deriving (Show, Eq)

data NetworkInfo = NetworkInfo
    { networkId     :: Text
    , networkConfig :: NetworkConfig
    } deriving (Eq, Show)


newtype Connection = Connection 
    { connection :: WS.Connection
    }

instance NetworkConnection Connection where
    type SendType Connection = ClientMessage
    type ReceiveType Connection = ServerMessage
    send :: MonadIO m => Connection -> ClientMessage -> m ()
    send conn toSend = do
        let msg = serialize toSend
        liftIO $ WS.sendTextData (connection conn) msg

    receive :: MonadIO m => Connection -> m (Either Text ServerMessage)
    receive conn = do
        msg <- liftIO $ WS.receiveData (connection conn)
        case deserialize msg of
            Nothing -> return $ Left msg
            Just action -> return $ Right action

newConnection :: WS.Connection -> Connection
newConnection = Connection