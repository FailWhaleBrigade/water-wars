{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
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

-- |Response to a Login request.
-- Either fails with an error message or succeeds with the session id
data LoginResponse = LoginResponse
    { successSessionId :: Text
    , successPlayer    :: InGamePlayer
    } deriving (Show, Read, Eq, Generic)

-- |Player action that can be sent to a server.
newtype PlayerAction = PlayerAction
    { getAction :: Action
    }
    deriving (Show, Read, Eq, Generic)

-- |Sets up a game for a single round.
data GameSetup = GameSetup
    { numberOfPlayers :: Int
    , terrainMap :: Text
    } deriving (Show, Read, Eq, Ord, Generic)

-- |Response record for a GameSetup request.
-- May fail if the game has already been set up, or the GameSetup request was invalid.
newtype GameSetupResponse = GameSetupResponse
    { getSetupResponse :: Either SetupError Bool
    } deriving (Show, Read, Eq, Generic)

    -- |Signals that an error has happened during game initialization
data SetupError
    = InvalidAmountOfPlayers
    | UnknownMap
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance Serialize SetupError where

-- |If players are logging out, for completeness, not neccessarily used.
data Logout = Logout deriving (Show, Read, Eq, Generic)

-- |Signals the server that a client is ready
data ClientReady = ClientReady deriving (Show, Read, Eq, Generic)

-- |Inform client that the game is about to start at game tick n
newtype GameStart = GameStart Integer deriving (Show, Read, Eq, Generic)

data ServerMessage
    = GameSetupResponseMessage GameSetupResponse
    | LoginResponseMessage LoginResponse
    | GameMapMessage GameMap
    | GameStateMessage GameState GameEvents
    | GameWillStartMessage GameStart
    | GameStartMessage
    deriving (Show, Eq, Read, Generic)

data ClientMessage
    = LoginMessage Login
    | LogoutMessage Logout
    | GameSetupMessage GameSetup
    | PlayerActionMessage PlayerAction
    | ClientReadyMessage ClientReady
    deriving (Show, Eq, Read, Generic)


instance Serialize GameSetupResponse where
instance Serialize Logout where
instance Serialize ClientReady where
instance Serialize Text where
    put txt = put $ encodeUtf8 txt
    get     = decodeUtf8 <$> get

instance Serialize Login where
instance Serialize LoginResponse where
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
instance Serialize GameSetup where
instance Serialize GameState where
instance Serialize Projectiles 
instance Serialize Projectile
instance Serialize DeadPlayers where
instance Serialize DeadPlayer where
instance Serialize GameEvents
instance Serialize GameEvent
instance Serialize GameStart where
instance Serialize ServerMessage where

instance Serialize ClientMessage where
instance Serialize PlayerAction where 
instance Serialize Action where 
instance Serialize RunAction where 
instance Serialize JumpAction where 
instance Serialize ShootAction where 
instance Serialize Angle where 
    

instance Serializable ClientMessage where
    serialize = runPut . put
instance Deserializable ClientMessage where
    deserialize bs = runGet get bs

instance Serializable ServerMessage where
    serialize = runPut . put
instance Deserializable ServerMessage where
    deserialize bs = runGet get bs
    
