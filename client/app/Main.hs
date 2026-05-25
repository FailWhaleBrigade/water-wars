{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

----------------------------------------------------------------------------
module Main (main) where

----------------------------------------------------------------------------

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, replicateM_, void)
import Data.Bifunctor
import qualified Data.Maybe as Maybe
import Miso
import qualified Miso.CSS as CSS
import Miso.Canvas
import qualified Miso.Canvas as Canvas
import qualified Miso.FFI as FFI
import qualified Miso.Html as H
import Miso.Html.Property
import qualified Miso.Html.Property as P
import Miso.JSON (Result (..), fromJSON, parseEither, withObject, (.:))
import Miso.Lens
import Miso.WebSocket (WebSocket)
import qualified Miso.WebSocket as WS
import WaterWars.Client.Render.Display (render)
import WaterWars.Client.Render.State
import WaterWars.Client.Render.Utils
import WaterWars.Client.World
import WaterWars.Core.Game.Base (Location (..))
import qualified WaterWars.Network.Protocol as Protocol
import WaterWars.Network.WasmJson ()
import DOMRect (DomRect)
import qualified DOMRect as DomRect

----------------------------------------------------------------------------

-- | Component model state
data Model
  = Model
  { _time :: (Double, Double)
  , _world :: World
  , _gameView :: Maybe GameView
  , _animationState :: AnimationState
  , _resources :: Maybe Resources
  , _websocket :: WebSocket
  , _connected :: Bool
  }
  deriving (Eq)

----------------------------------------------------------------------------

time :: Lens Model (Double, Double)
time = lens _time (\m k -> m{_time = k})

websocket :: Lens Model WebSocket
websocket = lens _websocket $ \r x -> r{_websocket = x}

connected :: Lens Model Bool
connected = lens _connected $ \r x -> r{_connected = x}

world :: Lens Model World
world = lens _world $ \r x -> r{_world = x}

animationState :: Lens Model AnimationState
animationState = lens _animationState $ \r x -> r{_animationState = x}

resources :: Lens Model (Maybe Resources)
resources = lens _resources $ \r x -> r{_resources = x}

gameView :: Lens Model (Maybe GameView)
gameView = lens _gameView $ \r x -> r{_gameView = x}

----------------------------------------------------------------------------

-- | Sum type for App events
data Action
  = -- Time ticks
    GetTime
  | SetTime (Double, Double)
  | Startup
  | FinishedResourceLoading Resources GameView
  | -- WebSockets
    OnOpen WebSocket
  | OnMessage Protocol.ServerMessage
  | OnClosed WS.Closed
  | OnError MisoString
  | Connect
  | Disconnect
  | -- WebSocket send operations
    SendUpdate
  | -- Client interaction
    DoAction GameAction
  | DoShootAction RealLocation
  | StopAction GameAction
  | Noop
  | Shoot PointerEvent

data GameAction
  = JumpAction
  | LeftAction
  | RightAction
  | DuckAction
  deriving (Show)

data GameView = GameView
  { _canvasGameView :: JSVal
  }
  deriving (Eq)

----------------------------------------------------------------------------

-- | Entry point for a miso application
main :: IO ()
#ifdef INTERACTIVE
main = reload (startApp defaultEvents app)
#else
main = startApp (defaultEvents <> keyboardEvents <> mouseEvents) app
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
    { mount = Just Startup
    , subs =
        [ timerSub
        , mouseClickSub Shoot
        ]
    }

mouseClickSub :: (PointerEvent -> action) -> Sub action
mouseClickSub = windowSub "click" pointerDecoder

timerSub :: (Action -> IO ()) -> IO ()
timerSub sink = void $ forever $ do
  threadDelay 20_000 -- 20ms in microseconds
  sink SendUpdate

----------------------------------------------------------------------------

-- | Empty application state
emptyModel :: Model
emptyModel =
  Model
    { _time = (0, 0)
    , _world = emptyWorld
    , _animationState = newAnimationState
    , _resources = Nothing
    , _websocket = WS.emptyWebSocket
    , _connected = False
    , _gameView = Nothing
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
  Startup -> do
    io $ do
      r <- setup baseUrl
      val <- getElementById canvasId
      -- set up event listeners
      focus canvasId
      pure $ FinishedResourceLoading r (GameView val)
  FinishedResourceLoading r gView -> do
    resources .= Just r
    animationState .= newAnimationState
    gameView .= Just gView
    issue GetTime
  Connect -> do
    WS.connectJSON
      "ws://127.0.0.1:8080"
      OnOpen
      OnClosed
      ( \payload -> case fromJSON payload of
          Error err -> OnError err
          Success val -> OnMessage val
      )
      OnError
  OnOpen socket -> do
    websocket .= socket
    connected .= True
    WS.sendJSON socket (Protocol.LoginMessage (Protocol.Login Nothing))
  OnClosed closed -> do
    connected .= False
  OnMessage message -> do
    anim <- use animationState
    w <- use world
    let
      (newWorld, animState, _mEvents) = updateWorld message anim w
    world .= newWorld
    animationState .= animState
  OnError errorMessage ->
    io_ (consoleError errorMessage)
  Disconnect ->
    WS.close =<< use websocket
  SendUpdate -> do
    isConnected <- use connected
    if isConnected
      then do
        socket <- use websocket
        w <- use world
        let
          (playerAction, newWorld) = extractGameAction (canvasWidth, canvasHeight) w
        WS.sendJSON socket (Protocol.PlayerActionMessage playerAction)
        world .= newWorld
      else do
        pure ()
  DoShootAction loc -> do
    world %= doShootAction loc
  DoAction ev -> do
    world %= doGameAction ev
  StopAction ev -> do
    world %= stopGameAction ev
  Noop ->
    pure ()
  Shoot ptrEv -> do
    g <- use gameView

    case g of
      Nothing -> do
        pure ()
      Just game -> io $ do
        boundingRect <- (_canvasGameView game) # "getBoundingClientRect" $ ()
        domRectM <- DomRect.getBoundingRectProps boundingRect
        FFI.consoleLog $ ms $ show ptrEv
        case domRectM of
          Nothing -> do
            FFI.consoleError ("water-wars: Parse error on DomRect.")
            pure Noop
          Just domRect -> do
            consoleLog $ ms $ show domRect
            inBounds domRect (client ptrEv) >>= \ case
              Nothing -> pure Noop
              Just loc -> do
                consoleLog $ ms $ "Shooting at: " <> show loc
                pure $ DoShootAction loc

inBounds :: DomRect -> (Double, Double) -> IO (Maybe RealLocation)
inBounds coll2D (x, y) = do
  let
    (targetX, targetY) = (x - DomRect.left coll2D, y - DomRect.top coll2D)
  consoleLog $ Miso.ms $ "inBounds: " <> show (targetX, targetY)
  if and
    [ 0 <= targetX
    , targetX <= DomRect.width coll2D
    , 0 <= targetY
    , targetY <= DomRect.height coll2D
    ]
    then
      pure $
        Just $
          RealLocation ( targetX, targetY)
    else
      pure Nothing

doShootAction :: RealLocation -> World -> World
doShootAction loc w@World{worldInfo} =
  w{worldInfo = worldInfo{shoot = Just loc}}

doGameAction :: GameAction -> World -> World
doGameAction gameAction w@World{worldInfo} = case gameAction of
  JumpAction -> w{worldInfo = worldInfo{jump = True}}
  LeftAction -> w{worldInfo = worldInfo{walkLeft = True}}
  RightAction -> w{worldInfo = worldInfo{walkRight = True}}
  DuckAction -> w{worldInfo = worldInfo{duck = True}}

stopGameAction :: GameAction -> World -> World
stopGameAction gameAction w@World{worldInfo} = case gameAction of
  JumpAction -> w{worldInfo = worldInfo{jump = False}}
  LeftAction -> w{worldInfo = worldInfo{walkLeft = False}}
  RightAction -> w{worldInfo = worldInfo{walkRight = False}}
  DuckAction -> w{worldInfo = worldInfo{duck = False}}

newTime :: IO (Double, Double)
newTime = liftIO $ do
  date <- newDate
  (,) <$> getMilliseconds date <*> getSeconds date

----------------------------------------------------------------------------

canvasId :: MisoString
canvasId = "game"

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Model Action
viewModel model =
  H.div_
    [ P.className "main"
    , width_ "100%"
    , CSS.style_ [CSS.display "flex", CSS.margin "0", CSS.justifyContent "center"]
    ]
    [ Canvas.canvas
        [ width_ (ms canvasWidth)
        , height_ (ms canvasHeight)
        , P.id_ canvasId
        , CSS.style_ [CSS.flexGrow "0", CSS.justifySelf "center"]
        , H.onKeyDown (keyboardEvent DoAction)
        , H.onKeyUp (keyboardEvent StopAction)
        , P.tabindex_ "1"
        ]
        (initCanvas (canvasWidth, canvasHeight))
        ( canvasDraw
            (canvasWidth, canvasHeight)
            (model ^. time)
            (model ^. resources)
            (model ^. animationState)
            (model ^. world)
        )
    , H.div_
        []
        [websocketView model]
    ]

canvasWidth :: Double
canvasWidth = 1400

canvasHeight :: Double
canvasHeight = 800

keyboardEvent :: (GameAction -> Action) -> KeyCode -> Action
keyboardEvent onMsg = Maybe.maybe Noop onMsg . keycodeToGameAction

keycodeToGameAction :: KeyCode -> Maybe GameAction
keycodeToGameAction (KeyCode val) = case val of
  -- https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/keyCode#Browser_compatibility
  0x41 {- A -} -> Just LeftAction
  0x53 {- S -} -> Just DuckAction
  0x44 {- D -} -> Just RightAction
  0x57 {- W -} -> Just JumpAction
  _ -> Nothing

----------------------------------------------------------------------------
baseUrl :: MisoString
baseUrl = ""

initCanvas :: (Double, Double) -> DOMRef -> Canvas ()
initCanvas (w, h) _ = do
  translate (1200, 1200)
  -- scale (1, -1)
  save ()

canvasDraw ::
  (Double, Double) ->
  (Double, Double) ->
  Maybe Resources ->
  AnimationState ->
  World ->
  () ->
  Canvas ()
canvasDraw (w, h) (millis', secs') mResources animState world_ () = do
  globalCompositeOperation SourceOver
  clearRect (0, 0, w, h)

  case mResources of
    Nothing -> pure ()
    Just res -> do
      render (w, h) res animState world_

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
    [className "websocket-box"]
    [ H.div_
        [class_ "websocket-header"]
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
                [text "socket-0"]
            ]
        ]
    , H.div_
        [class_ "websocket-controls"]
        [ optionalAttrs
            H.button_
            [ class_ "btn btn-success connect-btn"
            , H.onClick Connect
            ]
            (m ^. connected)
            [disabled_]
            ["Connect"]
        , optionalAttrs
            H.button_
            [ class_ "btn btn-danger disconnect-btn"
            , H.onClick Disconnect
            ]
            (not (m ^. connected))
            [disabled_]
            ["Disconnect"]
        ]
    ]
