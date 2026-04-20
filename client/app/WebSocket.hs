-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE CPP                        #-}
-----------------------------------------------------------------------------
module WebSocket (websocketComponent) where
-----------------------------------------------------------------------------
import           Control.Monad (unless)
import           GHC.Generics
-----------------------------------------------------------------------------
import           Miso hiding (on)
import           Miso.Html
import           Miso.Html.Property
import           Miso.Lens
import           Miso.WebSocket
import           Miso.String (ToMisoString)
import qualified Miso.String as MS
import qualified WaterWars.Network.Protocol as Protocol
-----------------------------------------------------------------------------
data Message
  = Message
  { dateString :: MisoString
  , message :: MisoString
  , origin :: Origin
  } deriving (Eq, Show, Generic)
-----------------------------------------------------------------------------
data Origin = CLIENT | SYSTEM | SERVER
  deriving (Eq, Show, Generic)
-----------------------------------------------------------------------------
instance ToMisoString Origin where
  toMisoString = \case
    CLIENT -> "CLIENT"
    SYSTEM -> "SYSTEM"
    SERVER -> "SERVER"
-----------------------------------------------------------------------------
data Action
  = OnOpen WebSocket
  | OnMessage Protocol.ServerMessage
  | OnClosed Closed
  | OnError MisoString
  | Send
  | SendMessage MisoString
  | Update MisoString
  | Connect
  | Disconnect
  | NoOp
  | CloseBox
  | Clear
-----------------------------------------------------------------------------
data Model = Model
  { _msg :: MisoString
  , _websocket :: WebSocket
  , _connected :: Bool
  , _clearInput :: Bool
  , _boxId :: Int
  } deriving Eq
-----------------------------------------------------------------------------
msg :: Lens Model MisoString
msg = lens _msg $ \r x -> r { _msg = x }
-----------------------------------------------------------------------------
websocket :: Lens Model WebSocket
websocket = lens _websocket $ \r x -> r { _websocket = x }
-----------------------------------------------------------------------------
connected :: Lens Model Bool
connected = lens _connected $ \r x -> r { _connected = x }
-----------------------------------------------------------------------------
clearInput :: Lens Model Bool
clearInput = lens _clearInput $ \r x -> r { _clearInput = x }
-----------------------------------------------------------------------------
boxId :: Lens Model Int
boxId = lens _boxId $ \r x -> r { _boxId = x }
-----------------------------------------------------------------------------
emptyModel :: Int -> Model
emptyModel = Model mempty  emptyWebSocket False True
-----------------------------------------------------------------------------
websocketComponent :: Int -> Component parent Model Action
websocketComponent box = component (emptyModel box) updateModel viewModel
  where
    updateModel = \case
      Send -> do
        m <- use msg
        unless (MS.null m) $ do
          issue (SendMessage m)
          clearInput .= True
          msg .= ""
          io_ $ do
            consoleLog $ ms (show m)
            -- pure $ Append (Message dateString m CLIENT)
      SendMessage m -> do
        socket <- use websocket
        sendText socket m
      Connect -> do
        io_ $ consoleLog $ "Connecting"
        connectText
          "ws://127.0.0.1:8080"
          OnOpen
          OnClosed
          (\ msg -> case Protocol.deserialize (fromMisoString msg) of
             Left err -> OnError (ms err)
             Right serverMsg -> OnMessage serverMsg)
          OnError
      OnOpen socket -> do
        websocket .= socket
        connected .= True
      OnClosed closed -> do
        connected .= False
        -- io $ do
        --   date <- newDate
        --   dateString <- date & toLocaleString
        io_ $ consoleLog $ ms (show closed)
        pure ()
      OnMessage message ->
        io_ $ consoleLog $ ms (show message)

      OnError errorMessage ->
        io_ (consoleError errorMessage)
      Update input -> do
        clearInput .= False
        msg .= input
      NoOp ->
        pure ()
      CloseBox ->
        broadcast box
      Disconnect ->
        close =<< use websocket
      Clear -> do
        clearInput .= True
        msg .= ""
-----------------------------------------------------------------------------
viewModel :: Model -> View Model Action
viewModel m =
  div_
  [ className "websocket-box" ]
  [ div_
    [ class_ "websocket-header" ]
    [ div_
      []
      [ span_
        [ classList_
          [ ("websocket-status", True)
          , ("status-disconnected", not (m ^. connected))
          , ("status-connected", m ^. connected)
          ]
        ]
        []
      , span_
        [ class_ "websocket-id"
        ]
        [ text $ "socket-" <> ms (m ^. boxId) ]
      ]
    , button_
      [ aria_ "label" "Close"
      , class_ "btn-close"
      , onClick CloseBox
      ]
      [ "×" ]
    ]
    , div_
      [ class_ "websocket-controls" ]
      [ optionalAttrs
        button_
        [ class_ "btn btn-success connect-btn"
        , onClick Connect
        ]
        (m ^. connected)
        [ disabled_ ]
        [ "Connect" ]
      , optionalAttrs
        button_
        [ class_ "btn btn-danger disconnect-btn"
        , onClick Disconnect
        ]
        (not (m ^. connected))
        [ disabled_ ]
        ["Disconnect"]
      ]
    ]
