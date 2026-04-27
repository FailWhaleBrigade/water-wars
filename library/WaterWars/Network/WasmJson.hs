{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BangPatterns #-}

module WaterWars.Network.WasmJson where

import Control.Applicative ((<|>))
import qualified Data.Array as Arr
import Miso (MisoString)
import Miso.JSON
import WaterWars.Core.Game
import WaterWars.Network.Protocol

instance ToJSON Login
instance FromJSON Login

instance ToJSON LoginResponse
instance FromJSON LoginResponse

instance ToJSON PlayerAction
instance FromJSON PlayerAction

instance ToJSON GameSetup
instance FromJSON GameSetup

instance ToJSON GameSetupResponse where
  toJSON (GameSetupResponse e) = object ["getSetupResponse" .= toJSON e]
instance FromJSON GameSetupResponse where
  parseJSON = withObject "GameSetupResponse" $ \o ->
    GameSetupResponse <$> o .: "getSetupResponse"

instance (ToJSON a) => ToJSON (Either SetupError a) where
  toJSON (Left e) = object ["Left" .= toJSON e]
  toJSON (Right a) = object ["Right" .= toJSON a]
instance (FromJSON a) => FromJSON (Either SetupError a) where
  parseJSON = withObject "Either" $ \o -> do
    ( do
        err <- o .: "Left"
        pure $ Left err
      )
      <|> ( do
              a <- o .: "Right"
              pure $ Right a
          )
instance ToJSON SetupError where
  toJSON InvalidAmountOfPlayers = String "InvalidAmountOfPlayers"
  toJSON UnknownMap = String "UnknownMap"
instance FromJSON SetupError where
  parseJSON = withText "SetupError" $ \case
    "InvalidAmountOfPlayers" -> pure InvalidAmountOfPlayers
    "UnknownMap" -> pure UnknownMap
    other -> fail $ "Unknown SetupError: " ++ show other

instance ToJSON Logout where
  toJSON Logout = Array []
instance FromJSON Logout where
  parseJSON = withArray "Logout" $ \ arr -> do
    let ![] = arr
    pure Logout

instance ToJSON ClientReady where
  toJSON ClientReady = Array []
instance FromJSON ClientReady where
  parseJSON = withArray "ClientReady" $ \ arr -> do
    let ![] = arr
    pure ClientReady

instance ToJSON GameStart where
  toJSON (GameStart s) = toJSON s
instance FromJSON GameStart where
  parseJSON o = GameStart <$> parseJSON o

instance ToJSON ServerMessage where
  toJSON (GameSetupResponseMessage r) = object ["tag" .= String "GameSetupResponseMessage", "contents" .= toJSON r]
  toJSON (LoginResponseMessage r) = object ["tag" .= String "LoginResponseMessage", "contents" .= toJSON r]
  toJSON (GameMapMessage m) = object ["tag" .= String "GameMapMessage", "contents" .= toJSON m]
  toJSON (GameStateMessage s e) = object ["tag" .= String "GameStateMessage", "contents" .= toJSON (s, e)]
  toJSON (GameWillStartMessage s) = object ["tag" .= String "GameWillStartMessage", "contents" .= toJSON s]
  toJSON GameStartMessage = object ["tag" .= String "GameStartMessage"]
  toJSON ResetGameMessage = object ["tag" .= String "ResetGameMessage"]
  toJSON (StopGameWithWinner p) = object ["tag" .= String "StopGameWithWinner", "contents" .= toJSON p]
  toJSON StopGame = object ["tag" .= String "StopGame"]
instance FromJSON ServerMessage where
  parseJSON = withObject "ServerMessage" $ \o -> do
    tag :: MisoString <- o .: "tag"
    case tag of
      "GameSetupResponseMessage" -> GameSetupResponseMessage <$> o .: "contents"
      "LoginResponseMessage" -> LoginResponseMessage <$> o .: "contents"
      "GameMapMessage" -> GameMapMessage <$> o .: "contents"
      "GameStateMessage" -> uncurry GameStateMessage <$> o .: "contents"
      "GameWillStartMessage" -> GameWillStartMessage <$> o .: "contents"
      "GameStartMessage" -> pure GameStartMessage
      "ResetGameMessage" -> pure ResetGameMessage
      "StopGameWithWinner" -> StopGameWithWinner <$> o .: "contents"
      "StopGame" -> pure StopGame
      other -> fail $ "Unknown ServerMessage tag: " ++ show other

instance ToJSON ClientMessage where
  toJSON (LoginMessage m) = object ["tag" .= String "LoginMessage", "contents" .= toJSON m]
  toJSON (LogoutMessage m) = object ["tag" .= String "LogoutMessage", "contents" .= toJSON m]
  toJSON (GameSetupMessage m) = object ["tag" .= String "GameSetupMessage", "contents" .= toJSON m]
  toJSON (PlayerActionMessage m) = object ["tag" .= String "PlayerActionMessage", "contents" .= toJSON m]
  toJSON (ClientReadyMessage m) = object ["tag" .= String "ClientReadyMessage", "contents" .= toJSON m]
instance FromJSON ClientMessage where
  parseJSON = withObject "ClientMessage" $ \o -> do
    tag :: MisoString <- o .: "tag"
    case tag of
      "LoginMessage" -> LoginMessage <$> o .: "contents"
      "LogoutMessage" -> LogoutMessage <$> o .: "contents"
      "GameSetupMessage" -> GameSetupMessage <$> o .: "contents"
      "PlayerActionMessage" -> PlayerActionMessage <$> o .: "contents"
      "ClientReadyMessage" -> ClientReadyMessage <$> o .: "contents"
      other -> fail $ "Unknown ClientMessage tag: " ++ show other

instance ToJSON InGamePlayer
instance FromJSON InGamePlayer

instance ToJSON VelocityVector
instance FromJSON VelocityVector

instance ToJSON RunDirection where
  toJSON RunLeft = String "RunLeft"
  toJSON RunRight = String "RunRight"
instance FromJSON RunDirection where
  parseJSON = withText "RunDirection" $ \case
    "RunLeft" -> pure RunLeft
    "RunRight" -> pure RunRight
    other -> fail $ "Unknown RunDirection: " ++ show other

instance ToJSON Location where
  toJSON (Location (x, y)) = Array [toJSON x, toJSON y]

instance FromJSON Location where
  parseJSON = withArray "Location" $ \arr -> do
    let
      [x', y'] = arr
    x <- parseJSON x'
    y <- parseJSON y'
    pure $ Location (x, y)


instance ToJSON GameMap
instance FromJSON GameMap

instance ToJSON Terrain where
  toJSON (Terrain m) = blockArrayToJSON m
instance FromJSON Terrain where
  parseJSON v = Terrain <$> blockArrayParseJSON parseJSON v

instance ToJSON TerrainDecoration where
  toJSON (TerrainDecoration m) = blockArrayToJSON m
instance FromJSON TerrainDecoration where
  parseJSON v = TerrainDecoration <$> blockArrayParseJSON parseJSON v

instance ToJSON Decoration where
  toJSON Algea = String "Algea"
  toJSON Coral = String "Coral"
  toJSON Snail = String "Snail"
  toJSON Umbrella = String "Umbrella"
instance FromJSON Decoration where
  parseJSON = withText "Decoration" $ \case
    "Algea" -> pure Algea
    "Coral" -> pure Coral
    "Snail" -> pure Snail
    "Umbrella" -> pure Umbrella
    other -> fail $ "Unknown Decoration: " ++ show other

instance ToJSON BlockLocation where
  toJSON (BlockLocation (x, y)) = Array [toJSON x, toJSON y]

instance FromJSON BlockLocation where
  parseJSON = withArray "BlockLocation" $ \arr -> do
    let
      [x', y'] = arr
    x <- parseJSON x'
    y <- parseJSON y'
    pure $ BlockLocation (x, y)

instance ToJSON Block where
  toJSON (SolidBlock content) = object ["tag" .= String "SolidBlock", "contents" .= toJSON content]
  toJSON NoBlock = object ["tag" .= String "NoBlock"]
instance FromJSON Block where
  parseJSON = withObject "Block" $ \o -> do
    tag :: MisoString <- o .: "tag"
    case tag of
      "SolidBlock" -> SolidBlock <$> o .: "contents"
      "NoBlock" -> pure NoBlock
      other -> fail $ "Unknown Block tag: " ++ show other

instance ToJSON BlockContent where
  toJSON Floor = String "Floor"
  toJSON EndLeft = String "EndLeft"
  toJSON EndRight = String "EndRight"
  toJSON BottomLeftCorner = String "BottomLeftCorner"
  toJSON BottomRightCorner = String "BottomRightCorner"
  toJSON TopRightCorner = String "TopRightCorner"
  toJSON TopLeftCorner = String "TopLeftCorner"
  toJSON LeftWall = String "LeftWall"
  toJSON RightWall = String "RightWall"
  toJSON Middle = String "Middle"
  toJSON Ceil = String "Ceil"
instance FromJSON BlockContent where
  parseJSON = withText "BlockContent" $ \case
    "Floor" -> pure Floor
    "EndLeft" -> pure EndLeft
    "EndRight" -> pure EndRight
    "BottomLeftCorner" -> pure BottomLeftCorner
    "BottomRightCorner" -> pure BottomRightCorner
    "TopRightCorner" -> pure TopRightCorner
    "TopLeftCorner" -> pure TopLeftCorner
    "LeftWall" -> pure LeftWall
    "RightWall" -> pure RightWall
    "Middle" -> pure Middle
    "Ceil" -> pure Ceil
    other -> fail $ "Unknown BlockContent: " ++ show other

instance ToJSON Player
instance FromJSON Player

instance ToJSON InGamePlayers
instance FromJSON InGamePlayers

instance ToJSON GameState
instance FromJSON GameState

instance ToJSON Projectiles
instance FromJSON Projectiles

instance ToJSON Projectile
instance FromJSON Projectile

instance ToJSON DeadPlayers
instance FromJSON DeadPlayers

instance ToJSON DeadPlayer
instance FromJSON DeadPlayer

instance ToJSON GameEvents
instance FromJSON GameEvents

instance ToJSON GameEvent where
  toJSON = \ case
    ShotProjectile p -> toJSON p

instance FromJSON GameEvent where
  parseJSON o =
    ShotProjectile <$> parseJSON o

instance ToJSON Action
instance FromJSON Action

instance ToJSON RunAction where
  toJSON = \ case
    RunAction p -> toJSON p
instance FromJSON RunAction where
  parseJSON o =
    RunAction <$> parseJSON o

instance ToJSON JumpAction where
  toJSON JumpAction = Array []

instance FromJSON JumpAction where
  parseJSON = withArray "JumpAction" $ \ arr -> do
    let ![] = arr
    pure JumpAction

instance ToJSON ShootAction where
  toJSON (ShootAction angle) = toJSON angle
instance FromJSON ShootAction where
  parseJSON o = ShootAction <$> parseJSON o

instance ToJSON Angle where
  toJSON (Angle angle) = toJSON angle
instance FromJSON Angle where
  parseJSON o = Angle <$> parseJSON o

blockArrayToJSON :: (ToJSON a) => Arr.Array BlockLocation a -> Value
blockArrayToJSON arr =
  object
    [ "bounds" .= [x0, y0, x1, y1]
    , "elems" .= fmap toJSON (Arr.elems arr)
    ]
 where
  (BlockLocation (x0, y0), BlockLocation (x1, y1)) = Arr.bounds arr

blockArrayParseJSON :: (Value -> Parser b) -> Value -> Parser (Arr.Array BlockLocation b)
blockArrayParseJSON f = withObject "BlockArray" $ \o -> do
  [x0, y0, x1, y1] <- o .: "bounds"
  es :: [Value] <- o .: "elems"
  ms <- traverse f es
  pure $ Arr.listArray (BlockLocation (x0, y0), BlockLocation (x1, y1)) ms
