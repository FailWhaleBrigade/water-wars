module WaterWars.Client.World where

import WaterWars.Core.Game
import qualified WaterWars.Core.Game as CoreState

import Control.Concurrent.STM.TVar (TVar)
import qualified Data.List as List
import GHC.Generics (Generic)
import WaterWars.Client.Render.State
import WaterWars.Network.Protocol (GameStart (..), LoginResponse (..), PlayerAction (..), ServerMessage (..))
import qualified WaterWars.Network.Protocol as Protocol
import WaterWars.Client.Render.Utils
import Debug.Trace

newtype WorldSTM = WorldSTM (TVar World)

data World = World
  { worldInfo :: WorldInfo
  , lastGameUpdate :: ServerUpdate
  }
  deriving (Generic, Eq)

data WorldInfo = WorldInfo
  { jump :: Bool
  , walkLeft :: Bool
  , walkRight :: Bool
  , shoot :: Maybe RealLocation
  , lastShot :: Maybe RealLocation
  , duck :: Bool
  , exitGame :: Bool
  , readyUp :: Bool
  , -- TODO: Everything beneath should be refactored into another datatype
    countdown :: Maybe Integer
  , gameRunning :: Bool
  , localPlayer :: Maybe Player
  , winnerPlayer :: Maybe Player
  , projectiles :: [CoreState.Projectile]
  }
  deriving (Show, Generic, Eq)

emptyWorld :: World
emptyWorld =
  World
    { worldInfo =
        WorldInfo
          { jump = False
          , walkLeft = False
          , walkRight = False
          , duck = False
          , shoot = Nothing
          , lastShot = Nothing
          , exitGame = False
          , readyUp = False
          , countdown = Nothing
          , gameRunning = False
          , localPlayer = Nothing
          , winnerPlayer = Nothing
          , projectiles = []
          }
    , lastGameUpdate =
        ServerUpdate
          { gameStateUpdate =
              GameState
                { inGamePlayers = InGamePlayers []
                , gameDeadPlayers = DeadPlayers []
                , gameProjectiles = Projectiles []
                , gameTicks = 0
                }
          }
    }

updateWorld :: Protocol.ServerMessage -> AnimationState -> World -> (World, AnimationState, Maybe GameEvents)
updateWorld serverMsg animationState world@World{..} = case serverMsg of
  GameMapMessage gameMap ->
    ( world
    , setTerrain (terrainDecoration gameMap) (gameTerrain gameMap) animationState
    , Nothing
    )
  GameStateMessage gameState@GameState{..} gameEvents ->
    let
      WorldInfo{..} = worldInfo

      newProjectiles :: [Projectile]
      newProjectiles = getProjectiles gameProjectiles

      worldInfo_ = WorldInfo{projectiles = newProjectiles, ..}
      maybeEvents =
        if null $ getGameEvents gameEvents
          then Nothing
          else Just gameEvents
    in
      ( World
          { worldInfo = worldInfo_
          , lastGameUpdate = ServerUpdate gameState
          , ..
          }
      , animationState
      , maybeEvents
      )
  GameSetupResponseMessage _ -> (world, animationState, Nothing)
  LoginResponseMessage loginResponse ->
    let
      WorldInfo{..} = worldInfo
      newPlayer = Just (playerDescription $ successPlayer loginResponse)
      worldInfo_ = WorldInfo{localPlayer = newPlayer, ..}
    in
      ( World{worldInfo = worldInfo_, ..}
      , animationState
      , Nothing
      )
  GameWillStartMessage (GameStart n) ->
    ( world{worldInfo = worldInfo{countdown = Just n}}
    , animationState
    , Nothing
    )
  GameStartMessage ->
    ( world
        { worldInfo = worldInfo{countdown = Nothing, gameRunning = True}
        }
    , animationState
    , Nothing
    )
  ResetGameMessage ->
    -- TODO: this is kind of hacky, we just forget the last game update to avoid the race condition
    -- between deleting all animation and the next gloss update which generates new animation as needed
    ( world
        { -- AnimationState     = AnimationState { playerAnimations = PlayerAnimationMap Map.empty }
          worldInfo = worldInfo{winnerPlayer = Nothing}
        , lastGameUpdate =
            ServerUpdate
              { gameStateUpdate =
                  GameState
                    { inGamePlayers = InGamePlayers []
                    , gameDeadPlayers = DeadPlayers []
                    , gameProjectiles = Projectiles []
                    , gameTicks = 0
                    }
              }
        }
    , animationState
    , Nothing
    )
  StopGame ->
    ( world
        { worldInfo =
            worldInfo
              { countdown = Nothing
              , gameRunning = False
              , winnerPlayer = Nothing
              }
        }
    , animationState
    , Nothing
    )
  StopGameWithWinner winner ->
    ( world
        { worldInfo =
            worldInfo
              { countdown = Nothing
              , gameRunning = False
              , winnerPlayer = Just winner
              }
        }
    , animationState
    , Nothing
    )

extractGameAction :: Size -> World -> (Protocol.PlayerAction, World)
extractGameAction dims world =
  let
    WorldInfo{..} = worldInfo world
    GameState{..} = gameStateUpdate $ lastGameUpdate world
    runCmd
      | walkLeft = Just (RunAction RunLeft)
      | walkRight = Just (RunAction RunRight)
      | otherwise = Nothing
    jmpCmd = if jump then Just JumpAction else Nothing
    shootCmd = toShootCmd dims shoot localPlayer inGamePlayers

    playerAction =
      Action
        { runAction = runCmd
        , jumpAction = jmpCmd
        , shootAction = shootCmd
        }

    newWorld =
      case shoot of
        Just target ->
          world
            { worldInfo =
                (worldInfo world)
                  { shoot = Nothing
                  , lastShot = Just target
                  }
            }
        Nothing -> world

  in
    (PlayerAction{getAction = playerAction}, newWorld)


currentPlayerLocation :: InGamePlayers -> Maybe Player -> Maybe InGamePlayer
currentPlayerLocation allPlayers localPlayer = do
  player <- localPlayer
  List.find
    ((== player) . playerDescription)
    (getInGamePlayers allPlayers)

vector :: Num a => (a, a) -> (a, a) -> (a, a)
vector (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

toShootCmd :: Size -> Maybe RealLocation -> Maybe Player -> InGamePlayers -> Maybe Angle
toShootCmd dims shoot localPlayer inGamePlayers = do
  -- Maybe Shoot
  shootTarget <- shoot
  inGamePlayer <- currentPlayerLocation inGamePlayers localPlayer
  let
    shootLocation = playerHeadLocation inGamePlayer
  return $ calculateAngle shootLocation $ fromRealLoc dims shootTarget

calculateAngle :: Location -> Location -> Angle
calculateAngle (Location a) (Location b) =
  let
    (dx, dy) = vector a b
  in
    Angle (atan2 dy dx)
