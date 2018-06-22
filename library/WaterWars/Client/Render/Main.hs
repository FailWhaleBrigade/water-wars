module WaterWars.Client.Render.Main(main) where

import ClassyPrelude

import Control.Monad.Except
import Control.Monad.Logger

import Graphics.Gloss.Interface.IO.Game

import Sound.ProteaAudio

import Options.Applicative

import WaterWars.Client.OptParse

import WaterWars.Client.Event.Message
import WaterWars.Client.Event.ClientLoop

import WaterWars.Client.Render.State (initializeState)
import WaterWars.Client.Render.Resources (setup)
import WaterWars.Client.Render.Display (renderIO)

import WaterWars.Client.Network.Connection (NetworkConfig(..), connectionThread)
import WaterWars.Network.Protocol

import WaterWars.Core.Game

window :: Display
window = InWindow "Water Wars" (800, 600) (10, 10)
-- window = Fullscreen

fps :: Int
fps = 60

backgroundColor :: Color
backgroundColor = white

opts :: ParserInfo Arguments
opts = info
    (argumentsParser <**> helper)
    (  fullDesc
    <> progDesc "Start an instance of the water-wars client."
    <> header "Fail Whale Brigade presents Water Wars."
    )


main :: IO ()
main = do
    result <- initAudio 64 44100 1024 -- max channels, mixing frequency, mixing buffer size
    unless result $ fail "failed to initialize the audio system"

    Arguments {..} <- execParser opts
    resources      <- runExceptT setup
    case resources of
        Left err -> putStrLn $ "Could not load texture. Cause: " ++ tshow err
        Right (bgTex, prjTex, playerTex, playerRunningTexs, playerDeathTexs, mantaTexs, countdownTexs, blocks, shootSound)
            -> do
                -- initialize world
                worldStm <- initializeState bgTex
                                            prjTex
                                            playerTex
                                            playerRunningTexs
                                            playerDeathTexs
                                            mantaTexs
                                            countdownTexs
                                            blocks
                                            shootSound
                -- initialize shared channels
                broadcastTChan <- atomically newTQueue
                receiveTChan   <- atomically newTQueue
                actionTvar <- newTVarIO (PlayerAction $ Action Nothing Nothing Nothing)

                -- start networking thread
                _              <- async {- Should never terminate -}
                    (connectionThread Nothing
                                      (NetworkConfig 1234 "localhost")
                                      broadcastTChan
                                      receiveTChan
                    )
                -- start event loop thread
                _ <- async $ do
                    runStdoutLoggingT
                        $ filterLogger (\_ level -> level /= LevelDebug)
                        $ eventLoop worldStm actionTvar broadcastTChan receiveTChan
                -- start a clock timer
                --_ <- async $ forever $ do 
                --    atomically $ writeTChan broadcastTChan SentEventMessage
                --    liftIO $ threadDelay (1000000 `div` 50)
                -- start music
                sample <- sampleFromFile "resources/sounds/Bubble_Game.ogg" 1.0
                soundLoop sample 1 1 0 1

                -- start gui
                playIO
                    window
                    backgroundColor
                    fps
                    worldStm
                    renderIO
                    (\event world -> do
                        atomically $ writeTQueue broadcastTChan
                                                (RenderEventMessage event)
                        return world
                    )
                    (\delta world -> do
                        atomically $ writeTQueue
                            broadcastTChan
                            (RenderTickEventMessage delta)
                        return world
                    )
                -- Will never be reached
                putStrLn "Goodbye, shutting down the Server!"
