{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds #-}

module WaterWars.Network.Connection where

import ClassyPrelude

class NetworkConnection c where
    type SendType c :: * -- Type that is sent as a message
    type ReceiveType c :: * -- Type that is read from a message
    send :: (MonadIO m, Serializable (SendType c)) => c -> SendType c -> m ()
    receive :: (MonadIO m, Deserializable (ReceiveType c)) => c -> m (Either Text (ReceiveType c))

class Serializable c where
    serialize :: c -> Text

class Deserializable c where
    deserialize :: Text -> Maybe c

class NetworkConnections c where
    type WriteType c :: *
    broadcast :: MonadIO m => c -> WriteType c -> m ()
