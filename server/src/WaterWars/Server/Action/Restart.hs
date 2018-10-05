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
import           WaterWars.Server.Action.Util


restartGame
    :: (Member (Log Text) r, Member (Reader Env) r, MonadIO m, Lifted m r)
    => Eff r Env
restartGame = do
    env@Env {..} <- ask
    let ServerEnv {..}  = serverEnv
    let GameConfig {..} = gameConfig
    let newGameMap      = advanceGameMaps gameMaps
    let current         = currentMap newGameMap
    let GameState {..}  = gameState gameLoop
    let inGamePlayers' =
            InGamePlayers
                $  map resetPlayer  (getInGamePlayers inGamePlayers)
                ++ map revivePlayer (getDeadPlayers gameDeadPlayers)


    EffLog.logE ("Restart the game" :: Text)
    broadcastMessage ResetGameMessage
    broadcastMessage (GameMapMessage current)

    return env
        { serverEnv = serverEnv
                          { gameLoop    =
                              GameLoopState
                                  { gameState = GameState
                                      { inGamePlayers   = inGamePlayers'
                                      , gameDeadPlayers = DeadPlayers empty
                                      , ..
                                      }
                                  , gameMap   = current
                                  }
                          , serverState = WarmUp
                          }
        , gameEnv   = gameEnv { readyPlayers = mempty }
        }

revivePlayer :: DeadPlayer -> InGamePlayer
revivePlayer DeadPlayer {..} =
    newInGamePlayer deadPlayerDescription (Location (0, 0))

resetPlayer :: InGamePlayer -> InGamePlayer
resetPlayer p = p { playerLocation = Location (0, 0) }
