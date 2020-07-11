{-# LANGUAGE TemplateHaskell #-}
module Lib.WebSocket where

import           Data.Aeson
import           Data.Aeson.TH                  ( deriveJSON )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as C8
import qualified Data.Yaml                     as Yaml
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.Text.Encoding             ( encodeUtf8
                                                , decodeUtf8
                                                )
import qualified Data.ByteString.Lazy.Char8    as L8
import           Control.Monad
import qualified Network.WebSockets            as WS
import           Control.Concurrent             ( forkIO )

import           Wuss

import qualified Reflex                        as R


import           Lib.Utils
import qualified Lib.HTTP                      as H
import           OpenAPI.Test


data WSEvent a = WSEvent
    { wsEventEvent :: Text
    , wsEventData :: a
    } deriving (Show)
$(deriveJSON (removePrefix "wsEvent") ''WSEvent)

data EventPosted = EventPosted
    { eventPostedPost :: Post
    , eventPostedChannelType :: Text
    }

instance FromJSON EventPosted where
    parseJSON = withObject "EventPosted" $ \o -> do
        eventPostedChannelType <- o .: "channel_type"
        postText <- o .: "post"
        eventPostedPost <- withEmbeddedJSON "Post" parseJSON (String postText)
        return $ EventPosted { .. }

connectWS :: H.Server -> BS.ByteString -> (L8.ByteString -> IO ()) -> IO ()
connectWS server token cb = do
    let host = C8.unpack $ H.serverHost server
    runSecureClient host 443 "/api/v4/websocket" $ \conn -> do
        let auth = object
                [ "seq" .= (1 :: Int)
                , "action" .= ("authentication_challenge" :: Text)
                , "data" .= object ["token" .= decodeUtf8 token]
                ]
        WS.sendTextData conn (encode auth)
        forever $ WS.receiveData conn >>= cb

