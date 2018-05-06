{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds #-}

module WaterWars.Network.Connection where

import ClassyPrelude

class NetworkConnection c where
    type SendType c :: * -- ^Type that is sent as a message
    type ReceiveType c :: * -- ^Type that is read from a message
    send :: (MonadIO m, Serializable (SendType c)) => c -> SendType c -> m ()
    receive :: (MonadIO m, Deserializable (ReceiveType c)) => c -> m (Either Text (ReceiveType c))

class Serializable c where
    serialize :: c -> Text

class Deserializable c where
    deserialize :: Text -> Maybe c

class NetworkConnections c where
    type WriteType c :: *
    type ReadType c :: *
    broadcast :: MonadIO m => c -> WriteType c -> m ()

-- Generilitation over TChan
class IPC c where
    type Identifier c :: * -- ^Uniquely identifies a connection
    type WriteTo c :: * -- ^Type to write to the server loop
    type ReadFrom c :: * -- ^Type that is received from the server loop

    writeTo :: MonadIO m => c -> WriteTo c -> m ()
    readFrom :: MonadIO m => c -> m (ReadFrom c)

-- |Type class that combines a network communication with a IPC connection
-- Makes sure that the types fit.
-- combines IPC and NetworkConnection.
type CanCommunicate c = (IPC c
    , NetworkConnection c
    , WriteTo c ~ ReceiveType c
    , Deserializable (ReceiveType c)
    , ReadFrom c ~ SendType c
    , Serializable (SendType c)
    )
