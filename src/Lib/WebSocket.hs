{-# LANGUAGE TemplateHaskell #-}
module Lib.WebSocket where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Yaml as Yaml
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Monad
import qualified Network.WebSockets as WS

import Wuss


import Lib.Utils
import qualified Lib.HTTP as H

data WSEvent a = WSEvent
    { wsEventEvent :: Text
    , wsEventData :: a
    } deriving (Show)
$(deriveJSON (removePrefix "wsEvent") ''WSEvent)

connectWS :: H.Server -> BS.ByteString -> IO ()
connectWS server token = do
    let host = C8.unpack $ H.serverHost server
    runSecureClient host 443 "/api/v4/websocket" $ \conn -> do
        putStrLn "Connected!"

        let auth = object
                [ "seq" .= ( 1 :: Int)
                , "action" .= ( "authentication_challenge" :: Text)
                , "data" .= object [ "token" .= ( decodeUtf8 $ token )]
                ]
        WS.sendTextData conn (encode auth)
        forever $ do
            msg <- WS.receiveData conn
            case decode msg ::Maybe (WSEvent Value) of
                Nothing -> print msg
                Just val -> print val
