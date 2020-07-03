{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}


module Lib.HTTP where

import           Network.HTTP.Client

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.CaseInsensitive          as CI
import qualified Data.List                     as List
import           Data.Text                      ( Text )
import           Data.Aeson.TH                  ( deriveJSON )
import           Control.Monad.Reader
import           Data.Functor                   ( (<&>) )
import           Data.Aeson                     ( encode
                                                , decode
                                                )
import           Data.Maybe                     ( fromJust )

import           Lib.Utils
import           OpenAPI.Lib


addBearerToken :: BS.ByteString -> Request -> Request
addBearerToken token req = req
    { requestHeaders = authHeader : (requestHeaders req)
    }
    where authHeader = (CI.mk "Authorization", "Bearer " <> token)

getHeader :: Response body -> BS.ByteString -> Maybe BS.ByteString
getHeader resp headerName =
    List.lookup (CI.mk headerName) $ responseHeaders resp

data Server = Server
    { serverHost :: BS.ByteString
    , serverPort :: Int
    , serverBasePath :: BS.ByteString
    }

mkServerRequest :: Server -> Request
mkServerRequest Server {..} = defaultRequest { host = serverHost
                                             , port = serverPort
                                             , path = serverBasePath
                                             }


data ApiClient = ApiClientConfig
    { clientHttpManager :: Manager
    , clientServerData :: Server
    }

-- class Monad m => ApiRequest m a where
--     type ReqResult a

--     encodeRequest :: a -> m Request
--     parseResult :: Response LBS.ByteString -> ReqResult a

instance HasHttpManager ApiClient where
    getHttpManager = clientHttpManager

data LoginReq = LoginReq
    { loginReqLoginId:: Text
    , loginReqPassword :: Text
    } deriving Show
$(deriveJSON (removePrefix "loginReq") ''LoginReq)

makeLoginReq :: Monad m => LoginReq -> (ReaderT ApiClient m) Request
makeLoginReq loginReq = asks (mkServerRequest . clientServerData) <&> \req ->
    req { method      = "POST"
        , path        = "/api/v4/users/login"
        , requestBody = RequestBodyLBS $ encode loginReq
        }

    -- parseResult resp = sessionToken
    --     where   sessionToken = fromJust $ getHeader resp "Token"

runReq
    :: forall m r
     . (HasHttpManager r, MonadIO m, MonadReader r m)
    => Request
    -> m (Response LBS.ByteString)
runReq httpReq = do
    mgr <- asks getHttpManager
    liftIO $ httpLbs httpReq mgr
