module WaterWars.Server.Action.Restart where

import           Effectful
import           Effectful.Reader.Static
import           Effectful.Log
import           WaterWars.Core.Game
import           WaterWars.Network.Protocol
import           WaterWars.Server.Env
import           WaterWars.Server.Action.Util
import Data.Text (Text)


restartGame
    :: (Log :> r, Reader Env :> r, IOE :> r)
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
                $  fmap resetPlayer  (getInGamePlayers inGamePlayers)
                <> fmap revivePlayer (getDeadPlayers gameDeadPlayers)


    logInfo_ ("Restart the game" :: Text)
    broadcastMessage ResetGameMessage
    broadcastMessage (GameMapMessage current)

    return env
        { serverEnv = serverEnv
                          { gameLoop    =
                              GameLoopState
                                  { gameState = GameState
                                      { inGamePlayers   = inGamePlayers'
                                      , gameDeadPlayers = DeadPlayers mempty
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
