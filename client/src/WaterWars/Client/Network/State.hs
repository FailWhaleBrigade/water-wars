{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
module WaterWars.Client.Network.State where


-- import Data.Text

-- -- import qualified Network.WebSockets            as WS
-- import           WaterWars.Network.Protocol    as Protocol
-- import Control.Monad.IO.Class

-- data NetworkConfig = NetworkConfig
--     { portId   :: Int
--     , hostName :: String
--     } deriving (Show, Eq)

-- data NetworkInfo = NetworkInfo
--     { networkId     :: Text
--     , networkConfig :: NetworkConfig
--     } deriving (Eq, Show)


-- newtype Connection = Connection
--     { connection :: ()
--     }

-- send :: MonadIO m => Connection -> ClientMessage -> m ()
-- send conn toSend = do
--     undefined

-- receive :: MonadIO m => Connection -> m (Either String ServerMessage)
-- receive conn = do
--     msg <- liftIO $ WS.receiveData (connection conn)
--     return $ deserialize msg

-- newConnection :: () -> Connection
-- newConnection = Connection
