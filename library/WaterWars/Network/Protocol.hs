{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WaterWars.Network.Protocol where

import ClassyPrelude

import Data.Serialize
import WaterWars.Network.Connection
import WaterWars.Core.Game

-- |Datatype to login to a game server.
-- So far, only a reconnect options is supported.
newtype Login = Login
    { sessionId :: Maybe Text
    } deriving (Show, Read, Eq, Generic)
    deriving anyclass Serialize

-- |Response to a Login request.
-- Either fails with an error message or succeeds with the session id
data LoginResponse = LoginResponse
    { successSessionId :: Text
    , successPlayer    :: InGamePlayer
    } deriving (Show, Read, Eq, Generic)
    deriving anyclass Serialize

-- |Player action that can be sent to a server.
newtype PlayerAction = PlayerAction
    { getAction :: Action
    }
    deriving (Show, Read, Eq, Generic)
    deriving anyclass Serialize

-- |Sets up a game for a single round.
data GameSetup = GameSetup
    { numberOfPlayers :: Int
    , terrainMap :: Text
    } deriving (Show, Read, Eq, Ord, Generic)
    deriving anyclass Serialize

-- |Response record for a GameSetup request.
-- May fail if the game has already been set up, or the GameSetup request was invalid.
newtype GameSetupResponse = GameSetupResponse
    { getSetupResponse :: Either SetupError Bool
    } deriving (Show, Read, Eq, Generic)
    deriving anyclass Serialize

    -- |Signals that an error has happened during game initialization
data SetupError
    = InvalidAmountOfPlayers
    | UnknownMap
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
    deriving anyclass Serialize


-- |If players are logging out, for completeness, not neccessarily used.
data Logout = Logout
    deriving (Show, Read, Eq, Generic)
    deriving anyclass Serialize

-- |Signals the server that a client is ready
data ClientReady = ClientReady
    deriving (Show, Read, Eq, Generic)
    deriving anyclass Serialize

-- |Inform client that the game is about to start at game tick n
newtype GameStart = GameStart Integer
    deriving (Show, Read, Eq, Generic)
    deriving anyclass Serialize

data ServerMessage
    = GameSetupResponseMessage GameSetupResponse
    | LoginResponseMessage LoginResponse
    | GameMapMessage GameMap
    | GameStateMessage GameState GameEvents
    | GameWillStartMessage GameStart
    | GameStartMessage
    | ResetGameMessage 
    deriving (Show, Eq, Read, Generic)
    deriving anyclass Serialize

data ClientMessage
    = LoginMessage Login
    | LogoutMessage Logout
    | GameSetupMessage GameSetup
    | PlayerActionMessage PlayerAction
    | ClientReadyMessage ClientReady
    deriving (Show, Eq, Read, Generic)
    deriving anyclass Serialize

instance Serialize Text where
    put = put . encodeUtf8
    get = decodeUtf8 <$> get

instance Serialize InGamePlayer where
instance Serialize VelocityVector where
instance Serialize RunDirection where
instance Serialize Location where
instance Serialize GameMap where
instance Serialize Terrain where
instance Serialize TerrainDecoration where
instance Serialize Decoration where
instance Serialize BlockLocation where
instance Serialize Block where
instance Serialize BlockContent where
instance Serialize Player where
instance Serialize InGamePlayers where
instance Serialize GameState where
instance Serialize Projectiles
instance Serialize Projectile
instance Serialize DeadPlayers where
instance Serialize DeadPlayer where
instance Serialize GameEvents
instance Serialize GameEvent

instance Serialize Action where
instance Serialize RunAction where
instance Serialize JumpAction where
instance Serialize ShootAction where
instance Serialize Angle where


instance Serializable ClientMessage where
    serialize = runPut . put
instance Deserializable ClientMessage where
    deserialize = runGet get

instance Serializable ServerMessage where
    serialize = runPut . put
instance Deserializable ServerMessage where
    deserialize = runGet get
