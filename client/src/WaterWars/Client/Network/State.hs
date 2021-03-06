{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
module WaterWars.Client.Network.State where

import           ClassyPrelude

import qualified Network.WebSockets            as WS
import           WaterWars.Network.Protocol    as Protocol

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

send :: MonadIO m => Connection -> ClientMessage -> m ()
send conn toSend = do
    let msg = serialize toSend
    liftIO $ WS.sendTextData (connection conn) msg

receive :: MonadIO m => Connection -> m (Either String ServerMessage)
receive conn = do
    msg <- liftIO $ WS.receiveData (connection conn)
    return $ deserialize msg

newConnection :: WS.Connection -> Connection
newConnection = Connection
