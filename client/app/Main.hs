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
import WaterWars.Client.Render.State (World, newWorld, setTerrain)
import WaterWars.Client.Render.State (setup)
import WaterWars.Client.Render.Display (render)
import WaterWars.Client.Resources.Resources
import Debug.Trace
import WaterWars.Core.DefaultGame

----------------------------------------------------------------------------

-- | Component model state
data Model
  = Model
  { _counter :: Int
  , _time :: (Double, Double)
  }
  deriving (Show, Eq)

----------------------------------------------------------------------------
counter :: Lens Model Int
counter = lens _counter $ \record field -> record{_counter = field}

time :: Lens Model (Double, Double)
time = lens _time (\m k -> m{_time = k})
----------------------------------------------------------------------------

-- | Sum type for App events
data Action
  = AddOne
  | SubtractOne
  | SayHelloWorld
  | GetTime
  | SetTime (Double, Double)
  deriving (Show, Eq)

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
emptyModel = Model 0 (0, 0)

----------------------------------------------------------------------------

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Effect parent Model Action
updateModel = \case
  AddOne -> counter += 1
  SubtractOne -> counter -= 1
  SayHelloWorld -> io_ (consoleLog "Hello world")
  GetTime ->
    io (SetTime <$> newTime)
  SetTime m -> do
    time .= m
    issue GetTime

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
        (canvasDraw (800, 600) (model ^. time) 0)
    ]

----------------------------------------------------------------------------
baseUrl :: MisoString
baseUrl = ""

initCanvas :: DOMRef -> Canvas Resources
initCanvas _ = do
  setup baseUrl

canvasDraw ::
  (Double, Double) ->
  (Double, Double) ->
  Int ->
  Resources ->
  Canvas ()
canvasDraw (w, h) (millis', secs') n resources = do
  globalCompositeOperation DestinationOver
  clearRect (0, 0, w, h)
  render $ setTerrain (terrainDecoration defaultGameMap) (gameTerrain defaultGameMap) $ newWorld $ resources
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
