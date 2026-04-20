{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WaterWars.Network.Protocol where

import           Data.Serialize
import           WaterWars.Core.Game
import GHC.Generics
import Data.ByteString
import Data.Text (Text)
import Data.Text.Encoding
import Data.Aeson
import qualified Data.Array as Arr
import qualified Data.Aeson.Encoding as Encoding
import Data.Aeson.Types

-- |Datatype to login to a game server.
-- So far, only a reconnect options is supported.
newtype Login = Login
    { sessionId :: Maybe Player
    } deriving (Show, Read, Eq, Generic)
    deriving anyclass Serialize
    deriving anyclass (ToJSON, FromJSON)

-- |Response to a Login request.
-- Either fails with an error message or succeeds with the session id
data LoginResponse = LoginResponse
    { successSessionId :: Player
    , successPlayer    :: InGamePlayer
    } deriving (Show, Read, Eq, Generic)
    deriving anyclass Serialize
    deriving anyclass (ToJSON, FromJSON)

-- |Player action that can be sent to a server.
newtype PlayerAction = PlayerAction
    { getAction :: Action
    }
    deriving (Show, Read, Eq, Generic)
    deriving anyclass Serialize
    deriving anyclass (ToJSON, FromJSON)

-- |Sets up a game for a single round.
data GameSetup = GameSetup
    { numberOfPlayers :: Int
    , terrainMap :: Text
    } deriving (Show, Read, Eq, Ord, Generic)
    deriving anyclass Serialize
    deriving anyclass (ToJSON, FromJSON)

-- |Response record for a GameSetup request.
-- May fail if the game has already been set up, or the GameSetup request was invalid.
newtype GameSetupResponse = GameSetupResponse
    { getSetupResponse :: Either SetupError Bool
    } deriving (Show, Read, Eq, Generic)
    deriving anyclass Serialize
    deriving anyclass (ToJSON, FromJSON)

-- |Signals that an error has happened during game initialization
data SetupError
    = InvalidAmountOfPlayers
    | UnknownMap
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
    deriving anyclass Serialize
    deriving anyclass (ToJSON, FromJSON)


-- |If players are logging out, for completeness, not neccessarily used.
data Logout = Logout
    deriving (Show, Read, Eq, Generic)
    deriving anyclass Serialize
    deriving anyclass (ToJSON, FromJSON)

-- |Signals the server that a client is ready
data ClientReady = ClientReady
    deriving (Show, Read, Eq, Generic)
    deriving anyclass Serialize
    deriving anyclass (ToJSON, FromJSON)

-- |Inform client that the game is about to start at game tick n
newtype GameStart = GameStart Integer
    deriving (Show, Read, Eq, Generic)
    deriving anyclass Serialize
    deriving anyclass (ToJSON, FromJSON)

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
    deriving anyclass (ToJSON, FromJSON)

data ClientMessage
    = LoginMessage Login
    | LogoutMessage Logout
    | GameSetupMessage GameSetup
    | PlayerActionMessage PlayerAction
    | ClientReadyMessage ClientReady
    deriving (Show, Eq, Read, Generic)
    deriving anyclass Serialize
    deriving anyclass (ToJSON, FromJSON)

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

instance ToJSON InGamePlayer
instance ToJSON VelocityVector
instance ToJSON RunDirection
instance ToJSON Location
instance ToJSON GameMap
instance ToJSON Terrain  where
    toJSON (Terrain m) = blockArrayToJSON m
    toEncoding (Terrain m) = blockArrayToEncoding m
instance ToJSON TerrainDecoration where
    toJSON (TerrainDecoration m) = blockArrayToJSON m
    toEncoding (TerrainDecoration m) = blockArrayToEncoding m
instance ToJSON Decoration
instance ToJSON BlockLocation
instance ToJSON Block
instance ToJSON BlockContent
instance ToJSON Player
instance ToJSON InGamePlayers
instance ToJSON GameState
instance ToJSON Projectiles
instance ToJSON Projectile
instance ToJSON DeadPlayers
instance ToJSON DeadPlayer
instance ToJSON GameEvents
instance ToJSON GameEvent
instance ToJSON Action
instance ToJSON RunAction
instance ToJSON JumpAction
instance ToJSON ShootAction
instance ToJSON Angle

instance FromJSON InGamePlayer
instance FromJSON VelocityVector
instance FromJSON RunDirection
instance FromJSON Location
instance FromJSON GameMap
instance FromJSON Terrain where
    parseJSON v = Terrain <$> blockArrayParseJSON v
instance FromJSON TerrainDecoration where
    parseJSON v = TerrainDecoration <$> blockArrayParseJSON v
instance FromJSON Decoration
instance FromJSON BlockLocation
instance FromJSON Block
instance FromJSON BlockContent
instance FromJSON Player
instance FromJSON InGamePlayers
instance FromJSON GameState
instance FromJSON Projectiles
instance FromJSON Projectile
instance FromJSON DeadPlayers
instance FromJSON DeadPlayer
instance FromJSON GameEvents
instance FromJSON GameEvent
instance FromJSON Action
instance FromJSON RunAction
instance FromJSON JumpAction
instance FromJSON ShootAction
instance FromJSON Angle

blockArrayToEncoding :: ToJSON a => Arr.Array BlockLocation a -> Encoding
blockArrayToEncoding arr =
    Encoding.pairs $
           Encoding.pair "bounds" (Encoding.list Encoding.int [x0, y0, x1, y1])
        <> Encoding.pair "elems" (Encoding.list toEncoding (Arr.elems arr))

    where
        (BlockLocation (x0, y0), BlockLocation (x1, y1)) = Arr.bounds arr

blockArrayToJSON :: ToJSON a => Arr.Array BlockLocation a -> Value
blockArrayToJSON arr =
    object
        [ "bounds" .=  [x0, y0, x1, y1]
        , "elems" .= Arr.elems arr
        ]
    where
        (BlockLocation (x0, y0), BlockLocation (x1, y1)) = Arr.bounds arr

blockArrayParseJSON :: FromJSON a => Value -> Parser (Arr.Array BlockLocation a)
blockArrayParseJSON = withObject "BlockArray" $ \o -> do
    [x0, y0, x1, y1] <- o .: "bounds"
    es <- o .: "elems"
    pure $ Arr.listArray (BlockLocation (x0, y0), BlockLocation (x1, y1)) es

class Serializable c where
    serialize :: c -> ByteString
    deserialize :: ByteString -> Either String c

instance Serializable ClientMessage where
    serialize = runPut . put
    deserialize = runGet get

instance Serializable ServerMessage where
    serialize = runPut . put
    deserialize = runGet get
