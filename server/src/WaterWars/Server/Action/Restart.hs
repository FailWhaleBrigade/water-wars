module WaterWars.Server.Action.Restart where

import           ClassyPrelude           hiding ( Reader
                                                , ask
                                                )
import           Control.Eff
import           Control.Eff.Reader.Strict
import           Control.Eff.Lift
import           Control.Eff.Log
import qualified Control.Eff.Log               as EffLog
import           WaterWars.Core.Game
import           WaterWars.Network.Protocol
import           WaterWars.Server.Env
import           WaterWars.Server.Events
import           WaterWars.Server.Action.Util


restartGameCallback
    :: (Member (Log Text) r, Member (Reader Env) r, MonadIO m, Lifted m r)
    => Eff r ()
restartGameCallback = do
    ServerEnv {..}  <- reader serverEnv
    GameEnv {..}    <- reader gameEnv
    NetworkEnv {..} <- reader networkEnv
    GameConfig {..} <- reader gameConfig
    EffLog.logE ("Restart the game" :: Text)
    newGameMap_ <- atomically $ do
        writeTVar readyPlayersTvar mempty -- demand that everyone ready's up again
        nextGameMap_ <- nextGameMap gameMapTvar

        modifyTVar' gameLoopTvar $ \gameLoop ->
            let
                GameState {..} = gameState gameLoop
                -- add all dead players to alive ones
                inGamePlayers' =
                    InGamePlayers
                        $  map (\p -> p { playerLocation = Location (0, 0) })
                               (getInGamePlayers inGamePlayers)
                        ++ map
                               (\DeadPlayer {..} -> newInGamePlayer
                                   deadPlayerDescription
                                   (Location (0, 0))
                               )
                               (getDeadPlayers gameDeadPlayers)
            in
                gameLoop
                    { gameState   = GameState
                                        { inGamePlayers   = inGamePlayers'
                                        , gameDeadPlayers = DeadPlayers empty
                                        , ..
                                        }
                    , gameRunning = False
                    , gameMap     = nextGameMap_
                    }

        return nextGameMap_

    broadcastMessage ResetGameMessage
    broadcastMessage (GameMapMessage newGameMap_)
