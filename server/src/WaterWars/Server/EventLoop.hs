{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module WaterWars.Server.EventLoop
    ( runEventLoop
    )
where

import           ClassyPrelude           hiding ( ask
                                                , Reader
                                                )

import           Effectful
import           Effectful.Reader.Static
import           Effectful.Log

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
    | AddPlayerCmdActionCmd Player PlayerAction
    | AddFutureCmd Integer FutureEvent
    | RemoveFutureCmd Integer
    | ReadyUpPlayerCmd Player
    | AddPlayerCmd Player
    | ConnectPlayerCmd Player Connection
    | RemovePlayerCmd Player
    | BroadcastCmd ServerMessage
    | UpdateGameLoopCmd GameState
    deriving (Eq, Show)

runEventLoop
    :: MonadUnliftIO m
    => Logger
    -> TVar Env
    -> TQueue EventMessage
    -> m ()
runEventLoop logger envTvar queue = forever $ do
    env     <- readTVarIO envTvar
    message <- atomically $ readTQueue queue
    let actions = eventLoop message env
    newEnv <- liftIO $ runEff . runLog "event-loop" logger LogInfo $ handleCmd actions env
    atomically $ modifyTVar' envTvar (const newEnv)

eventLoop :: EventMessage -> Env -> [Command]
eventLoop (ClientMessageEvent sessionId clientMsg) env = case clientMsg of
    LoginMessage     _      -> [AddPlayerCmd sessionId]

    LogoutMessage    Logout -> [RemovePlayerCmd sessionId]

    GameSetupMessage _      -> []

    PlayerActionMessage playerAction ->
        [AddPlayerCmdActionCmd sessionId playerAction]

    ClientReadyMessage ClientReady ->
        let readyPlayers_ :: Int = length (readyPlayers $ gameEnv env)
            connectedPlayers_ :: Int = length (connectionMap $ networkEnv env)
            gameTick = gameTicks . gameState . gameLoop $ serverEnv env
            isAlreadyReady = member sessionId (readyPlayers $ gameEnv env)
            cmd = if not isAlreadyReady
                then if readyPlayers_ + 1 >= connectedPlayers_
                    then
                        [ StartGameInCmd 240
                        , AddFutureCmd (gameTick + 240) StartGame
                        , ReadyUpPlayerCmd sessionId
                        ]
                    else [ReadyUpPlayerCmd sessionId]
                else []
        in  cmd


eventLoop (GameLoopMessageEvent gameStateUpdate gameEvents) Env {..} =
    let ServerEnv {..} = serverEnv
        GameEnv {..}   = gameEnv
        gameTick       = gameTicks gameStateUpdate
        players        = getInGamePlayers $ inGamePlayers gameStateUpdate
        winner         = playerDescription $ players `indexEx` 0
        gameOverCmd    = case (serverState, length players) of
            (Running, 0) ->
                [ StopGameCmd
                , BroadcastCmd StopGame
                , AddFutureCmd (gameTick + 240) ResetGame
                ]
            (Running, 1) ->
                [ GameOverCmd winner
                , BroadcastCmd (StopGameWithWinner winner)
                , AddFutureCmd (gameTick + 240) ResetGame
                ]
            _ -> []
        actionToExecute = case lookup gameTick eventMap of
            Nothing        -> []
            Just ResetGame -> [ResetGameCmd, RemoveFutureCmd gameTick]
            Just StartGame -> [StartGameCmd, RemoveFutureCmd gameTick]
    in  [ UpdateGameLoopCmd gameStateUpdate
        , BroadcastCmd (GameStateMessage gameStateUpdate gameEvents)
        ]
        ++ gameOverCmd
        ++ actionToExecute

eventLoop (RegisterEvent uuid conn) _ = [ConnectPlayerCmd uuid conn]

handleCmd
    :: (Log :> r, IOE :> r)
    => [Command]
    -> Env
    -> Eff r Env
handleCmd cmds env = foldM handleCmd_ env cmds

handleCmd_
    :: (Log :> r, IOE :> r) => Env -> Command -> Eff r Env
handleCmd_ env@Env {..} cmd = case cmd of
    StartGameCmd -> do
        runReader env startGame
        return env { serverEnv = serverEnv { serverState = Running } }

    StartGameInCmd soon -> do
        let gameTick = gameTicks . gameState $ gameLoop serverEnv
        let msg      = GameWillStartMessage (GameStart (gameTick + soon))
        runReader env (broadcastMessage msg)
        return env

    StopGameCmd -> return env { serverEnv = serverEnv { serverState = Over } }

    ResetGameCmd ->
        -- TODO: this should not have side effects if possible
        runReader env restartGame

    PauseGameCmd ->
        return env { serverEnv = serverEnv { serverState = Paused } }

    GameOverCmd winner ->
        return env { serverEnv = serverEnv { serverState = Over } }

    AddPlayerCmdActionCmd sessionId action -> do
        let GameEnv {..} = gameEnv
        let playerMay    = lookup sessionId playerMap
        case playerMay of
            Nothing -> do
                logAttention_
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

    AddFutureCmd trigger event -> return env
        { serverEnv =
            serverEnv { eventMap = insertMap trigger event (eventMap serverEnv)
                      }
        }
    RemoveFutureCmd trigger -> return env
        { serverEnv = serverEnv
                          { eventMap = deleteMap trigger (eventMap serverEnv)
                          }
        }
    ReadyUpPlayerCmd sessionId -> return env
        { gameEnv =
            gameEnv { readyPlayers = insertSet sessionId (readyPlayers gameEnv)
                    }
        }
    AddPlayerCmd sessionId -> do
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
    ConnectPlayerCmd uuid conn -> return env
        { networkEnv =
            networkEnv
                { connectionMap = insertMap uuid conn (connectionMap networkEnv)
                }
        }

    RemovePlayerCmd sessionId -> return env
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
    BroadcastCmd msg -> do
        logTrace_ $ tshow msg
        runReader env (broadcastMessage msg)
        return env
    UpdateGameLoopCmd newgameState -> return env
        { serverEnv =
            serverEnv
                { gameLoop = (gameLoop serverEnv) { gameState = newgameState }
                }
        , gameEnv   = gameEnv { playerAction = PlayerActions mempty }
        }
