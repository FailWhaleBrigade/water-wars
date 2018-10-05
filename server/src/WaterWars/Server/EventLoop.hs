{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module WaterWars.Server.EventLoop where

import           ClassyPrelude           hiding ( ask
                                                , Reader
                                                )

import           Control.Eff
import           Control.Eff.Reader.Strict
import           Control.Eff.Log
import qualified Control.Eff.Log               as EffLog
import           Control.Eff.Lift        hiding ( lift )

import           WaterWars.Network.Protocol
import           WaterWars.Core.Game
import           WaterWars.Server.ConnectionMgnt
import           WaterWars.Server.Env
import           WaterWars.Server.Events
import           WaterWars.Server.Action.Start
import           WaterWars.Server.Action.Restart
import           WaterWars.Server.Action.Util


data Command
    = StartGameCmd
    | StartGameInCmd Integer
    | StopGameCmd
    | ResetGameCmd
    | PauseGameCmd
    | GameOverCmd Player
    | AddPlayerAction Player PlayerAction
    | AddFuture Integer FutureEvent
    | RemoveFuture Integer
    | ReadyUpPlayer Player
    | AddPlayer Player
    | ConnectPlayer Player Connection
    | RemovePlayer Player
    | Broadcast ServerMessage
    | UpdateGameLoop GameState
    deriving (Eq, Show)

runEventLoop
    :: MonadUnliftIO m
    => Logger m Text
    -> TVar Env
    -> TQueue EventMessage
    -> m ()
runEventLoop logger envTvar queue = forever $ do
    env     <- readTVarIO envTvar
    message <- atomically $ readTQueue queue
    let actions = eventLoop message env
    newEnv <- runLift . runLog logger $ handleCmd actions env
    atomically $ modifyTVar' envTvar (const newEnv)

eventLoop :: EventMessage -> Env -> [Command]
eventLoop (EventClientMessage sessionId clientMsg) env = case clientMsg of
    LoginMessage     _      -> [AddPlayer sessionId]

    LogoutMessage    Logout -> [RemovePlayer sessionId]

    GameSetupMessage _      -> []

    PlayerActionMessage playerAction ->
        [AddPlayerAction sessionId playerAction]

    ClientReadyMessage ClientReady ->
        let
            readyPlayers_ :: Int = length (readyPlayers $ gameEnv env)
            connectedPlayers_ :: Int = length (connectionMap $ networkEnv env)
            gameTick = gameTicks . gameState . gameLoop $ serverEnv env
            isAlreadyReady =
                not $ member sessionId (readyPlayers $ gameEnv env)
            cmd = if not isAlreadyReady
                then if readyPlayers_ + 1 >= connectedPlayers_
                    then
                        [ StartGameInCmd 240
                        , AddFuture (gameTick + 240) StartGame
                        , ReadyUpPlayer sessionId
                        ]
                    else [ReadyUpPlayer sessionId]
                else []
        in
            cmd


eventLoop (EventGameLoopMessage gameStateUpdate gameEvents) Env {..}
    = let
          ServerEnv {..} = serverEnv
          GameEnv {..}   = gameEnv
          gameTick       = gameTicks gameStateUpdate
          players = getInGamePlayers $ inGamePlayers gameStateUpdate
          gameOverCmd =
              case
                      ( serverState
                      , length players
                      )
                  of
                      (Running, 0) ->
                          [StopGameCmd, AddFuture (gameTick + 240) ResetGame]
                      (Running, 1) ->
                          [GameOverCmd (playerDescription $ players `indexEx` 0), AddFuture (gameTick + 240) ResetGame]
                      _ -> []
          actionToExecute = case lookup gameTick eventMap of
              Nothing        -> []
              Just ResetGame -> [ResetGameCmd, RemoveFuture gameTick]
              Just StartGame -> [StartGameCmd, RemoveFuture gameTick]
      in
          [ UpdateGameLoop gameStateUpdate
          , Broadcast (GameStateMessage gameStateUpdate gameEvents)
          ]
          ++ gameOverCmd
          ++ actionToExecute

eventLoop (Register uuid conn) _ = [ConnectPlayer uuid conn]

handleCmd
    :: ('[Log Text] <:: r, MonadIO m, Lifted m r)
    => [Command]
    -> Env
    -> Eff r Env
handleCmd cmds env = foldM handleCmd_ env cmds

handleCmd_
    :: ('[Log Text] <:: r, MonadIO m, Lifted m r) => Env -> Command -> Eff r Env
handleCmd_ env@Env {..} cmd = case cmd of
    StartGameCmd -> do
        runReader env startGameCallback
        return env { serverEnv = serverEnv { serverState = Running } }

    StartGameInCmd soon -> do
        let gameTick = gameTicks . gameState $ gameLoop serverEnv
        let msg      = GameWillStartMessage (GameStart (gameTick + soon))
        runReader env (broadcastMessage msg)
        return env

    StopGameCmd  -> return env { serverEnv = serverEnv { serverState = Over } }

    ResetGameCmd -> runReader env restartGameCallback

    PauseGameCmd ->
        return env { serverEnv = serverEnv { serverState = Paused } }

    GameOverCmd winner ->
        -- establish winner
        return env { serverEnv = serverEnv { serverState = Over } }

    AddPlayerAction sessionId action -> do
        let GameEnv {..} = gameEnv
        let playerMay    = lookup sessionId playerMap
        case playerMay of
            Nothing -> do
                EffLog.logE
                    ("Received a message that did not belong to any player" :: Text
                    )
                return env
            Just InGamePlayer {..} -> do
                let PlayerActions {..} = playerAction
                let newActions = PlayerActions $ insertWith
                        (++)
                        sessionId
                        (getAction action)
                        getPlayerActions
                return env { gameEnv = gameEnv { playerAction = newActions } }
    AddFuture trigger event -> return env
        { serverEnv =
            serverEnv { eventMap = insertMap trigger event (eventMap serverEnv)
                      }
        }
    RemoveFuture trigger -> return env
        { serverEnv = serverEnv
                          { eventMap = deleteMap trigger (eventMap serverEnv)
                          }
        }
    ReadyUpPlayer sessionId -> return env
        { gameEnv =
            gameEnv { readyPlayers = insertSet sessionId (readyPlayers gameEnv)
                    }
        }
    AddPlayer sessionId -> do
        let player   = newInGamePlayer sessionId (Location (0, 0))
        let connMay  = lookup sessionId (connectionMap networkEnv)
        let gameMap_ = gameMap $ gameLoop serverEnv
        let addedPlayer =
                modifyGameState addInGamePlayer (gameLoop serverEnv) player
        case connMay of
            Nothing   -> return env
            Just conn -> do
                atomically $ do
                    writeTQueue
                        (readChannel conn)
                        (LoginResponseMessage (LoginResponse sessionId player))
                    writeTQueue (readChannel conn) (GameMapMessage gameMap_)
                return env
                    { gameEnv   =
                        gameEnv
                            { playerMap = insertMap sessionId
                                                    player
                                                    (playerMap gameEnv)
                            }
                    , serverEnv = serverEnv { gameLoop = addedPlayer }
                    }
    ConnectPlayer uuid conn -> return env
        { networkEnv =
            networkEnv
                { connectionMap = insertMap uuid conn (connectionMap networkEnv)
                }
        }

    RemovePlayer sessionId -> return env
        { gameEnv    = gameEnv
                           { readyPlayers = deleteSet sessionId
                                                      (readyPlayers gameEnv)
                           , playerMap = deleteMap sessionId (playerMap gameEnv)
                           }
        , networkEnv =
            networkEnv
                { connectionMap = deleteMap sessionId (connectionMap networkEnv)
                }
        , serverEnv  = serverEnv
                           { gameLoop = modifyGameState removePlayer
                                                        (gameLoop serverEnv)
                                                        sessionId
                           }
        }
    Broadcast msg -> do
        EffLog.logE $ tshow msg
        runReader env (broadcastMessage msg)
        return env
    UpdateGameLoop newgameState -> return env
        { serverEnv =
            serverEnv
                { gameLoop = (gameLoop serverEnv) { gameState = newgameState }
                }
        , gameEnv   = gameEnv { playerAction = PlayerActions mempty }
        }
