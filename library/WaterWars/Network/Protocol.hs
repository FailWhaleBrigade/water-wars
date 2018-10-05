{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WaterWars.Network.Protocol where

import           ClassyPrelude

import           Data.Serialize
import           WaterWars.Core.Game

-- |Datatype to login to a game server.
-- So far, only a reconnect options is supported.
newtype Login = Login
    { sessionId :: Maybe Player
    } deriving (Show, Read, Eq, Generic)
    deriving anyclass Serialize

-- |Response to a Login request.
-- Either fails with an error message or succeeds with the session id
data LoginResponse = LoginResponse
    { successSessionId :: Player
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
    | StopGameWithWinner Player
    | StopGame 
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

instance Serialize InGamePlayer
instance Serialize VelocityVector
instance Serialize RunDirection
instance Serialize Location
instance Serialize GameMap
instance Serialize Terrain
instance Serialize TerrainDecoration
instance Serialize Decoration
instance Serialize BlockLocation
instance Serialize Block
instance Serialize BlockContent
instance Serialize Player
instance Serialize InGamePlayers
instance Serialize GameState
instance Serialize Projectiles
instance Serialize Projectile
instance Serialize DeadPlayers
instance Serialize DeadPlayer
instance Serialize GameEvents
instance Serialize GameEvent

instance Serialize Action
instance Serialize RunAction
instance Serialize JumpAction
instance Serialize ShootAction
instance Serialize Angle

class Serializable c where
    serialize :: c -> ByteString
    deserialize :: ByteString -> Either String c

instance Serializable ClientMessage where
    serialize = runPut . put
    deserialize = runGet get

instance Serializable ServerMessage where
    serialize = runPut . put
    deserialize = runGet get
