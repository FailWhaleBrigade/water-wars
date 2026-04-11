----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Miso
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import           Miso.Lens
import qualified Miso.Canvas as Canvas
import Miso.Html.Property
import Miso.Canvas
import qualified Miso.CSS as CSS
import Control.Monad (replicateM_)
import qualified Miso.Html.Property as H
----------------------------------------------------------------------------
-- | Component model state
data Model
  = Model
  { _counter :: Int
  , _time   :: (Double, Double)
  } deriving (Show, Eq)
----------------------------------------------------------------------------
counter :: Lens Model Int
counter = lens _counter $ \record field -> record { _counter = field }
----------------------------------------------------------------------------
-- | Sum type for App events
data Action
  = AddOne
  | SubtractOne
  | SayHelloWorld
  | GetTime
  | SetTime (Double, Double)
  deriving (Show, Eq)
-----------------------------------------------------------------------------
time = lens _time (\m k -> m { _time = k })
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
app = (component emptyModel updateModel viewModel)
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
  AddOne        -> counter += 1
  SubtractOne   -> counter -= 1
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
viewModel x =
  H.div_
    [ P.className "counter"
    ]
    $
      [ H.button_ [ H.onClick AddOne ] [ text "+" ]
      , text $ ms (x ^. counter)
      , H.button_ [ H.onClick SubtractOne ] [ text "-" ]
      , H.br_ []
      , H.button_ [ H.onClick SayHelloWorld ] [ text "Alert Hello World!" ]
      ] <>


        [ Canvas.canvas
          [ width_ "300"
          , height_ "300"
          ]
          initCanvas
          (canvasDraw (x ^. time) 0)
        , H.img_
          [H.src_ (baseUrl <> "/textures/block/blockendleft32.png")]
        ]
----------------------------------------------------------------------------
baseUrl :: MisoString
baseUrl = "http://127.0.0.1:8000"

initCanvas :: DOMRef -> Canvas (Image, Image, Image)
initCanvas _ = liftIO $ do
  sun <- newImage (baseUrl <> "/textures/background/background.png")
  moon <- newImage (baseUrl <> "/textures/block/block32.png")
  earth <- newImage (baseUrl <> "/textures/block/blockendleft32.png")
  pure (sun, moon, earth)

canvasDraw
  :: (Double, Double)
  -> Int
  -> (Image, Image, Image)
  -> Canvas ()
canvasDraw (millis', secs') n (sun, moon, earth) = do
   let
     secs = secs' + fromIntegral n
     millis = millis' + fromIntegral n
   globalCompositeOperation DestinationOver
   clearRect (0,0,300,300)
   fillStyle $ Canvas.color (CSS.rgba 0 0 0 0.6)
   strokeStyle $ Canvas.color (CSS.rgba 0 153 255 0.4)
   save ()
   translate (150, 150)
   rotate ((((2 * pi) / 60) * secs) + (((2 * pi) / 60000) * millis))
   translate (105,0)
   fillRect (0 ,-12, 50, 24)
   drawImage (earth, -12, -12)
   save ()
   rotate ((((2 * pi) / 6) * secs) + (((2 * pi) / 6000) * millis))
   translate (0,28.5)
   drawImage (moon, -3.5, -3.5)
   replicateM_ 2 (restore ())
   beginPath ()
   arc (150, 150, 105, 0, pi * 2)
   stroke ()
   drawImage' (sun, 0, 0, 300, 300)
