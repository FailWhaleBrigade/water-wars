{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module WaterWars.Network.ProtocolTest where

import Control.Monad
import qualified Data.Aeson as Aeson
import Data.Array
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Encoding as TextLazy
import qualified Miso
import qualified Miso.JSON as Miso
import Test.Tasty
import Test.Tasty.QuickCheck
import WaterWars.Core.Game
import WaterWars.Network.Json ()
import WaterWars.Network.Protocol
import WaterWars.Network.WasmJson ()

protocolTests :: TestTree
protocolTests =
  testGroup
    "Json encodings"
    [ testGroup
        "server -> client"
        [ testProperty "Login" (roundTripJsonToMisoJson @Login)
        , testProperty "LoginResponse" (roundTripJsonToMisoJson @LoginResponse)
        , testProperty "PlayerAction" (roundTripJsonToMisoJson @PlayerAction)
        , testProperty "GameSetup" (roundTripJsonToMisoJson @GameSetup)
        , testProperty "GameSetupResponse" (roundTripJsonToMisoJson @GameSetupResponse)
        , testProperty "SetupError" (roundTripJsonToMisoJson @SetupError)
        , testProperty "Logout" (roundTripJsonToMisoJson @Logout)
        , testProperty "ClientReady" (roundTripJsonToMisoJson @ClientReady)
        , testProperty "GameStart" (roundTripJsonToMisoJson @GameStart)
        , testProperty "ServerMessage" (roundTripJsonToMisoJson @ServerMessage)
        , testProperty "ClientMessage" (roundTripJsonToMisoJson @ClientMessage)
        , testProperty "InGamePlayer" (roundTripJsonToMisoJson @InGamePlayer)
        , testProperty "VelocityVector" (roundTripJsonToMisoJson @VelocityVector)
        , testProperty "RunDirection" (roundTripJsonToMisoJson @RunDirection)
        , testProperty "Location" (roundTripJsonToMisoJson @Location)
        , testProperty "GameMap" (roundTripJsonToMisoJson @GameMap)
        , testProperty "Terrain" (roundTripJsonToMisoJson @Terrain)
        , testProperty "TerrainDecoration" (roundTripJsonToMisoJson @TerrainDecoration)
        , testProperty "Decoration" (roundTripJsonToMisoJson @Decoration)
        , testProperty "BlockLocation" (roundTripJsonToMisoJson @BlockLocation)
        , testProperty "Block" (roundTripJsonToMisoJson @Block)
        , testProperty "BlockContent" (roundTripJsonToMisoJson @BlockContent)
        , testProperty "Player" (roundTripJsonToMisoJson @Player)
        , testProperty "InGamePlayers" (roundTripJsonToMisoJson @InGamePlayers)
        , testProperty "GameState" (roundTripJsonToMisoJson @GameState)
        , testProperty "Projectiles" (roundTripJsonToMisoJson @Projectiles)
        , testProperty "Projectile" (roundTripJsonToMisoJson @Projectile)
        , testProperty "DeadPlayers" (roundTripJsonToMisoJson @DeadPlayers)
        , testProperty "DeadPlayer" (roundTripJsonToMisoJson @DeadPlayer)
        , testProperty "GameEvents" (roundTripJsonToMisoJson @GameEvents)
        , testProperty "GameEvent" (roundTripJsonToMisoJson @GameEvent)
        , testProperty "Action" (roundTripJsonToMisoJson @Action)
        , testProperty "RunAction" (roundTripJsonToMisoJson @RunAction)
        , testProperty "JumpAction" (roundTripJsonToMisoJson @JumpAction)
        , testProperty "ShootAction" (roundTripJsonToMisoJson @ShootAction)
        , testProperty "Angle" (roundTripJsonToMisoJson @Angle)
        ]
    , testGroup
        "client -> server"
        [ testProperty "Login" (roundTripMisoJsonToJson @Login)
        , testProperty "LoginResponse" (roundTripMisoJsonToJson @LoginResponse)
        , testProperty "PlayerAction" (roundTripMisoJsonToJson @PlayerAction)
        , testProperty "GameSetup" (roundTripMisoJsonToJson @GameSetup)
        , testProperty "GameSetupResponse" (roundTripMisoJsonToJson @GameSetupResponse)
        , testProperty "SetupError" (roundTripMisoJsonToJson @SetupError)
        , testProperty "Logout" (roundTripMisoJsonToJson @Logout)
        , testProperty "ClientReady" (roundTripMisoJsonToJson @ClientReady)
        , testProperty "GameStart" (roundTripMisoJsonToJson @GameStart)
        , testProperty "ServerMessage" (roundTripMisoJsonToJson @ServerMessage)
        , testProperty "ClientMessage" (roundTripMisoJsonToJson @ClientMessage)
        , testProperty "InGamePlayer" (roundTripMisoJsonToJson @InGamePlayer)
        , testProperty "VelocityVector" (roundTripMisoJsonToJson @VelocityVector)
        , testProperty "RunDirection" (roundTripMisoJsonToJson @RunDirection)
        , testProperty "Location" (roundTripMisoJsonToJson @Location)
        , testProperty "GameMap" (roundTripMisoJsonToJson @GameMap)
        , testProperty "Terrain" (roundTripMisoJsonToJson @Terrain)
        , testProperty "TerrainDecoration" (roundTripMisoJsonToJson @TerrainDecoration)
        , testProperty "Decoration" (roundTripMisoJsonToJson @Decoration)
        , testProperty "BlockLocation" (roundTripMisoJsonToJson @BlockLocation)
        , testProperty "Block" (roundTripMisoJsonToJson @Block)
        , testProperty "BlockContent" (roundTripMisoJsonToJson @BlockContent)
        , testProperty "Player" (roundTripMisoJsonToJson @Player)
        , testProperty "InGamePlayers" (roundTripMisoJsonToJson @InGamePlayers)
        , testProperty "GameState" (roundTripMisoJsonToJson @GameState)
        , testProperty "Projectiles" (roundTripMisoJsonToJson @Projectiles)
        , testProperty "Projectile" (roundTripMisoJsonToJson @Projectile)
        , testProperty "DeadPlayers" (roundTripMisoJsonToJson @DeadPlayers)
        , testProperty "DeadPlayer" (roundTripMisoJsonToJson @DeadPlayer)
        , testProperty "GameEvents" (roundTripMisoJsonToJson @GameEvents)
        , testProperty "GameEvent" (roundTripMisoJsonToJson @GameEvent)
        , testProperty "Action" (roundTripMisoJsonToJson @Action)
        , testProperty "RunAction" (roundTripMisoJsonToJson @RunAction)
        , testProperty "JumpAction" (roundTripMisoJsonToJson @JumpAction)
        , testProperty "ShootAction" (roundTripMisoJsonToJson @ShootAction)
        , testProperty "Angle" (roundTripMisoJsonToJson @Angle)
        ]
    ]

roundTripJsonToMisoJson :: (Aeson.ToJSON a, Aeson.FromJSON a, Miso.ToJSON a, Miso.FromJSON a, Eq a, Show a) => a -> Property
roundTripJsonToMisoJson a =
  let
    json = TextLazy.decodeUtf8 $ Aeson.encode a
    misoJson = TextLazy.encodeUtf8 $ Miso.fromMisoString $ Miso.encodePure a
  in
    counterexample ("Miso: " ++ TextLazy.unpack (TextLazy.decodeUtf8 misoJson)) $
      counterexample ("Json: " ++ TextLazy.unpack json) $
        case Miso.eitherDecode $ Miso.toMisoString json of
          Left err -> counterexample (Miso.fromMisoString err) False
          Right val -> val === a

roundTripMisoJsonToJson :: (Aeson.ToJSON a, Aeson.FromJSON a, Miso.ToJSON a, Miso.FromJSON a, Eq a, Show a) => a -> Property
roundTripMisoJsonToJson a =
  let
    json = TextLazy.decodeUtf8 $ Aeson.encode a
    misoJson = TextLazy.encodeUtf8 $ Miso.fromMisoString $ Miso.encodePure a
  in
    counterexample ("Miso: " ++ TextLazy.unpack (TextLazy.decodeUtf8 misoJson)) $
      counterexample ("Json: " ++ TextLazy.unpack json) $
        case Aeson.eitherDecode misoJson of
          Left err -> counterexample err False
          Right val -> val === a

instance Arbitrary Login where
  arbitrary = Login <$> arbitrary

instance Arbitrary LoginResponse where
  arbitrary = LoginResponse <$> arbitrary <*> arbitrary

instance Arbitrary PlayerAction where
  arbitrary = PlayerAction <$> arbitrary

instance Arbitrary GameSetup where
  arbitrary = GameSetup <$> arbitrary <*> (Text.pack <$> listOf arbitraryASCIIChar)

instance Arbitrary GameSetupResponse where
  arbitrary = GameSetupResponse <$> arbitrary

instance Arbitrary SetupError where
  arbitrary =
    elements
      [ InvalidAmountOfPlayers
      , UnknownMap
      ]

instance Arbitrary Logout where
  arbitrary = pure Logout

instance Arbitrary ClientReady where
  arbitrary = pure ClientReady

instance Arbitrary GameStart where
  arbitrary = GameStart <$> arbitrary

instance Arbitrary ServerMessage where
  arbitrary =
    oneof
      [ GameSetupResponseMessage <$> arbitrary
      , LoginResponseMessage <$> arbitrary
      , GameMapMessage <$> arbitrary
      , GameStateMessage <$> arbitrary <*> arbitrary
      , GameWillStartMessage <$> arbitrary
      , pure GameStartMessage
      , pure ResetGameMessage
      , StopGameWithWinner <$> arbitrary
      , pure StopGame
      ]

instance Arbitrary ClientMessage where
  arbitrary =
    oneof
      [ LoginMessage <$> arbitrary
      , LogoutMessage <$> arbitrary
      , GameSetupMessage <$> arbitrary
      , PlayerActionMessage <$> arbitrary
      , ClientReadyMessage <$> arbitrary
      ]

instance Arbitrary InGamePlayer where
  arbitrary =
    InGamePlayer
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary VelocityVector where
  arbitrary = VelocityVector <$> arbitrary <*> arbitrary

instance Arbitrary RunDirection where
  arbitrary =
    arbitraryBoundedEnum

instance Arbitrary Location where
  arbitrary = Location <$> arbitrary

instance Arbitrary GameMap where
  arbitrary = GameMap <$> arbitrary <*> arbitrary

instance Arbitrary Terrain where
  arbitrary = do
    lower@(BlockLocation (x, y)) <- arbitrary
    let
      upper = BlockLocation (x + 6, y + 6)
    es <- replicateM (rangeSize (lower, upper)) arbitrary
    pure $ Terrain (listArray (lower, upper) es)

instance Arbitrary TerrainDecoration where
  arbitrary = do
    lower@(BlockLocation (x, y)) <- arbitrary
    let
      upper = BlockLocation (x + 6, y + 6)
    es <- replicateM (rangeSize (lower, upper)) arbitrary
    pure $ TerrainDecoration (listArray (lower, upper) es)

instance Arbitrary Decoration where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary BlockLocation where
  arbitrary = BlockLocation <$> arbitrary

instance Arbitrary Block where
  arbitrary =
    oneof
      [ pure NoBlock
      , SolidBlock <$> arbitrary
      ]

instance Arbitrary BlockContent where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Player where
  arbitrary = Player <$> (Text.pack <$> listOf arbitraryASCIIChar)

instance Arbitrary InGamePlayers where
  arbitrary = InGamePlayers <$> arbitrary

instance Arbitrary GameState where
  arbitrary =
    GameState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary Projectiles where
  arbitrary = Projectiles <$> arbitrary

instance Arbitrary Projectile where
  arbitrary =
    Projectile
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary DeadPlayers where
  arbitrary = DeadPlayers <$> arbitrary

instance Arbitrary DeadPlayer where
  arbitrary =
    DeadPlayer
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary GameEvents where
  arbitrary = GameEvents <$> arbitrary

instance Arbitrary GameEvent where
  arbitrary = ShotProjectile <$> arbitrary

instance Arbitrary Action where
  arbitrary =
    Action
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary RunAction where
  arbitrary = RunAction <$> arbitrary

instance Arbitrary JumpAction where
  arbitrary = pure JumpAction

instance Arbitrary ShootAction where
  arbitrary = ShootAction <$> arbitrary

instance Arbitrary Angle where
  arbitrary = Angle <$> arbitrary
