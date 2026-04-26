{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

----------------------------------------------------------------------------
module Main where

----------------------------------------------------------------------------

import Control.Monad (replicateM_)
import Miso
import qualified Miso.CSS as CSS
import Miso.Canvas
import qualified Miso.Canvas as Canvas
import qualified Miso.Html as H
import Miso.Html.Property
import qualified Miso.Html.Property as CSS
import qualified Miso.Html.Property as H
import qualified Miso.Html.Property as P
import Miso.Lens
import WaterWars.Client.Render.State as State
import WaterWars.Client.Render.Display (render)
import WaterWars.Client.Resources.Resources
import Debug.Trace
import WaterWars.Core.DefaultGame
import WaterWars.Client.Render.State
import Json
import Miso.WebSocket (WebSocket)
import qualified Miso.WebSocket as WS
import qualified WaterWars.Network.Protocol as Protocol
import Miso.JSON (fromJSON, Result(..))
----------------------------------------------------------------------------

-- | Component model state
data Model
  = Model
  { _time :: (Double, Double)
  , _world :: World
  , _animationState :: Maybe RenderInfo
  , _websocket :: WebSocket
  , _connected :: Bool
  }
  deriving Eq


----------------------------------------------------------------------------

time :: Lens Model (Double, Double)
time = lens _time (\m k -> m{_time = k})

websocket :: Lens Model WebSocket
websocket = lens _websocket $ \r x -> r { _websocket = x }

connected :: Lens Model Bool
connected = lens _connected $ \r x -> r { _connected = x }

world :: Lens Model World
world = lens _world $ \r x -> r { _world = x }
----------------------------------------------------------------------------

-- | Sum type for App events
data Action
  -- Time ticks
  = GetTime
  | SetTime (Double, Double)
  -- WebSockets
  | OnOpen WebSocket
  | OnMessage Protocol.ServerMessage
  | OnClosed WS.Closed
  | OnError MisoString
  | Connect
  | Disconnect

----------------------------------------------------------------------------

-- | Entry point for a miso application
main :: IO ()
#ifdef INTERACTIVE
main = reload (startApp defaultEvents app)
#else
main = startApp defaultEvents app
#endif
----------------------------------------------------------------------------

-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif
----------------------------------------------------------------------------

-- | `component` takes as arguments the initial model, update function, view function
app :: App Model Action
app =
  (component emptyModel updateModel viewModel)
    { mount = Just GetTime
    }

----------------------------------------------------------------------------

-- | Empty application state
emptyModel :: Model
emptyModel = Model
 { _time = (0, 0)
 , _world = emptyWorld
 , _websocket = WS.emptyWebSocket
 , _connected = False
 }

----------------------------------------------------------------------------

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Effect parent Model Action
updateModel = \case
  GetTime ->
    io (SetTime <$> newTime)
  SetTime m -> do
    time .= m
    issue GetTime
  Connect -> do
    io_ $ consoleLog $ "Connecting"
    WS.connectJSON
      "ws://127.0.0.1:8080"
      OnOpen
      OnClosed
      (\ payload -> case fromJSON payload of
        Error err -> OnError err
        Success val -> OnMessage val)
      OnError
  OnOpen socket -> do
    websocket .= socket
    connected .= True
    WS.sendJSON socket (Protocol.LoginMessage (Protocol.Login Nothing))
  OnClosed closed -> do
    connected .= False
    io_ $ consoleLog $ ms (show closed)
    pure ()
  OnMessage message ->
    io_ $ consoleLog $ ms (show message)

  OnError errorMessage ->
    io_ (consoleError errorMessage)
  Disconnect ->
    WS.close =<< use websocket

newTime :: IO (Double, Double)
newTime = liftIO $ do
  date <- newDate
  (,) <$> getMilliseconds date <*> getSeconds date

----------------------------------------------------------------------------

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Model Action
viewModel model =
  H.div_
    [ P.className "main"
    , width_ "100%"
    , CSS.style_ [CSS.display "flex", CSS.margin "0", CSS.justifyContent "center"]
    ]
    [ Canvas.canvas
        [ width_ "800"
        , height_ "600"
        , CSS.style_ [CSS.flexGrow "0", CSS.justifySelf "center"]
        ]
        initCanvas
        (canvasDraw (800, 600) (model ^. time) (model ^. world))
    , H.div_
      [ key_ connId ]
      [ websocketView model ]
    ]
  where
    connId :: Int
    connId = 0

----------------------------------------------------------------------------
baseUrl :: MisoString
baseUrl = ""

initCanvas :: DOMRef -> Canvas Resources
initCanvas _ = do
  setup baseUrl

canvasDraw ::
  (Double, Double) ->
  (Double, Double) ->
  World ->
  Resources ->
  Canvas ()
canvasDraw (w, h) (millis', secs') world resources = do
  globalCompositeOperation SourceOver
  clearRect (0, 0, w, h)
  render
    (setTerrain (terrainDecoration defaultGameMap) (gameTerrain defaultGameMap) $ newRenderInfo resources)
    world

  save ()

oldCanvasDraw ::
  (Double, Double) ->
  (Double, Double) ->
  Int ->
    Canvas ()
oldCanvasDraw (w, h) (millis', secs') n = do
  let
    secs = secs' + fromIntegral n
    millis = millis' + fromIntegral n
  globalCompositeOperation DestinationOver
  clearRect (0, 0, w, h)
  let
    midPointX = w / 2
    midPointY = h / 2
  fillStyle $ Canvas.color (CSS.rgba 0 0 0 0.6)
  strokeStyle $ Canvas.color (CSS.rgba 0 153 255 0.4)
  save ()
  translate (midPointX, midPointY)
  rotate ((((2 * pi) / 60) * secs) + (((2 * pi) / 60000) * millis))
  translate (105, 0)
  fillRect (0, -12, 50, 24)
  -- drawImage (earth, -12, -12)
  save ()
  rotate ((((2 * pi) / 6) * secs) + (((2 * pi) / 6000) * millis))
  translate (0, 28.5)
  -- drawImage (moon, -3.5, -3.5)
  replicateM_ 2 (restore ())
  beginPath ()
  arc (midPointX, midPointY, 105, 0, pi * 2)
  stroke ()
  -- drawImage' (sun, 0, 0, w, h)

websocketView :: Model -> View Model Action
websocketView m =
  H.div_
  [ className "websocket-box" ]
  [ H.div_
    [ class_ "websocket-header" ]
    [ H.div_
      []
      [ H.span_
        [ classList_
          [ ("websocket-status", True)
          , ("status-disconnected", not (m ^. connected))
          , ("status-connected", m ^. connected)
          ]
        ]
        []
      , H.span_
        [ class_ "websocket-id"
        ]
        [ text "socket-0" ]
      ]
    ]
    , H.div_
      [ class_ "websocket-controls" ]
      [ optionalAttrs
        H.button_
        [ class_ "btn btn-success connect-btn"
        , H.onClick Connect
        ]
        (m ^. connected)
        [ disabled_ ]
        [ "Connect" ]
      , optionalAttrs
        H.button_
        [ class_ "btn btn-danger disconnect-btn"
        , H.onClick Disconnect
        ]
        (not (m ^. connected))
        [ disabled_ ]
        ["Disconnect"]
      ]
    ]
