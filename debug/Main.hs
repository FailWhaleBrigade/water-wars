import ClassyPrelude
import Control.Monad.Except

import Graphics.Gloss as Gloss
import Graphics.Gloss.Interface.IO.Game as Gloss

import WaterWars.Client.Codec.Resource (loadPngAsBmp, bulkLoad)
import WaterWars.Client.Render.State
import WaterWars.Client.Network.Connection
import WaterWars.Client.Render.Display (renderIO)

import           WaterWars.Network.Protocol

import           WaterWars.Client.Resources.Block
import           WaterWars.Core.Game

window :: Display
window = InWindow "Water Wars" (800, 600) (10, 10)
-- window = Fullscreen

fps :: Int
fps = 60

backgroundColor :: Color
backgroundColor = white

setup
    :: (MonadIO m, MonadError String m)
    => m
           ( Picture
           , Picture
           , Picture
           , [Picture]
           , [Picture]
           , [Picture]
           , BlockMap
           )
setup = do
    bgTex <- loadPngAsBmp "resources/textures/background/background.png"
    prjTex <- loadPngAsBmp "resources/textures/decoration/bubble.png"
    playerTex <- loadPngAsBmp "resources/textures/mermaid/idle/mermaid1.png"
    playerRunningTexs <- bulkLoad $ fromList
        (getMermaidPaths "resources/textures/mermaid/running/mermaid" 1)
    mantaTexs <- bulkLoad $ fromList
        [ "resources/textures/manta_animation/manta1.png"
        , "resources/textures/manta_animation/manta2.png"
        , "resources/textures/manta_animation/manta3.png"
        , "resources/textures/manta_animation/manta4.png"
        ]
    countdownTexs <- bulkLoad $ fromList
        [ "resources/textures/writing/3.png"
        , "resources/textures/writing/2.png"
        , "resources/textures/writing/1.png"
        , "resources/textures/writing/GO.png"
        ]
    blockMap <- loadBlockMap
    return
        ( bgTex
        , prjTex
        , playerTex
        , toList playerRunningTexs
        , toList mantaTexs
        , toList countdownTexs
        , blockMap
        )

getMermaidPaths :: String -> Int -> [String]
getMermaidPaths _ 15 = []
getMermaidPaths pathStart ind =
    (pathStart ++ show ind ++ ".png") : getMermaidPaths pathStart (ind + 1)

data NewWorld = NewWorld
    { originalWorld :: WorldSTM
    , states :: Seq GameState
    , frame :: Int
    }


main :: IO ()
main = do
    [fp] <- getArgs
    states <- fromList . mapMaybe readMay . lines <$> readFileUtf8 (unpack fp)
    resources <- runExceptT setup
    case resources of
        Left err -> putStrLn $ "Could not load texture. Cause: " ++ tshow err
        Right (bgTex, prjTex, playerTex, playerRunningTexs, mantaTexs, countdownTexs, blocks)
            -> do
                worldStm <- initializeState bgTex
                                            prjTex
                                            playerTex
                                            playerRunningTexs
                                            mantaTexs
                                            countdownTexs
                                            blocks
                let newWorld = NewWorld worldStm states 0
                playIO window
                       backgroundColor
                       fps
                       newWorld
                       (renderIO . originalWorld)
                       handleKeysIO'
                       (\_ w -> return w)
                -- Will never be reached
                putStrLn "Goodbye, shutting down the Client!"

handleKeysIO' :: Event -> NewWorld -> IO NewWorld
handleKeysIO' (EventKey (SpecialKey KeyLeft) Gloss.Up _ _) NewWorld {..} = do
    let frame' = frame - 1
    updateNewWorld NewWorld {frame = frame', ..}

handleKeysIO' (EventKey (SpecialKey KeyRight) Gloss.Up _ _) NewWorld {..} = do
    let frame' = frame + 1
    updateNewWorld NewWorld {frame = frame', ..}
handleKeysIO' _ world = return world

updateNewWorld :: NewWorld -> IO NewWorld
updateNewWorld NewWorld {..} = do
    let WorldSTM worldStm = originalWorld
    world <- readTVarIO worldStm
    let state = states `indexEx` frame
    let world'     = updateWorld (GameStateMessage state) world
    atomically $ writeTVar worldStm world'
    return $ NewWorld {..}



