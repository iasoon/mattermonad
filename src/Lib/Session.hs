module Lib.Session where

import           Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import           Data.ByteString.Builder        ( toLazyByteString )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( fromMaybe )
import           Network.HTTP.Client            ( Request(..)
                                                , HasHttpManager
                                                , Response
                                                , getHttpManager
                                                , RequestBody(..)
                                                )
import           Control.Monad.Reader


import           Lib.HTTP
import           OpenAPI.Lib

data SessionData = SessionData
    { sessionToken :: BS.ByteString
    , sessionClient :: ApiClient
    }

instance HasHttpManager SessionData where
    getHttpManager = getHttpManager . sessionClient

type SessionT m = ReaderT SessionData m

sessionDefaultReq :: SessionData -> Request
sessionDefaultReq SessionData {..} =
    addBearerToken sessionToken $ mkServerRequest $ clientServerData
        sessionClient

runApiRequest
    :: (Monad m, MonadIO m, ApiRequest a)
    => a
    -> SessionT m (Response LBS.ByteString)
runApiRequest a = do
    req <- asks sessionDefaultReq <&> \req -> req
        { method      = getApiRequestMethod a
        , path        = LBS.toStrict
                        .  toLazyByteString
                        $  "/api/v4"
                        <> getApiRequestPath a
        , requestBody = RequestBodyLBS $ fromMaybe "" (getApiRequestBody a)
        }
    runReq req

