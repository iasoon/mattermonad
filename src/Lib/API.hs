{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.API where

import qualified Data.ByteString as BS
import Data.Text (Text)
import Network.HTTP.Client
import Data.Aeson (Value, encode, decode)
import Data.Aeson.TH (deriveJSON)
import Data.Maybe (fromJust)
import Data.Functor ((<&>))
import Control.Monad.Reader

import Lib.HTTP
import Lib.Session
import Lib.Types
import Lib.Utils

data GetChannelByName = GetChannelByName
    { getChanTeamName :: BS.ByteString
    , getChanChannelName :: BS.ByteString
    }

instance Monad m => ApiRequest (SessionT m) GetChannelByName where
    type ReqResult GetChannelByName = ChannelData

    encodeRequest GetChannelByName { .. } =

        asks sessionDefaultReq <&> \req -> req
            { method = "GET"
            , path = chanPath
            }
        where   teamPath = "/api/v4" <> "/teams/name/" <> getChanTeamName
                chanPath = teamPath <> "/channels/name/" <> getChanChannelName

    
    parseResult = fromJust . decode . responseBody

data CreatePostReq = CreatePostReq
    { createPostChannelId :: Text
    , createPostMessage :: Text
    }
$(deriveJSON (removePrefix "createPost") ''CreatePostReq)


instance Monad m => ApiRequest (SessionT m) CreatePostReq where
    type ReqResult CreatePostReq = Value

    encodeRequest createPostReq = do
        asks sessionDefaultReq <&> \req -> req
            { method = "POST"
            , path = "/api/v4/posts"
            , requestBody = RequestBodyLBS $ encode createPostReq
            }
    
    parseResult = fromJust . decode . responseBody
