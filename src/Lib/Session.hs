{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}


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
                                                , responseBody
                                                , responseStatus
                                                )
import           Network.HTTP.Types             ( statusCode )
import           Control.Monad.Reader
import           GHC.TypeLits
import           Data.Aeson                     ( FromJSON
                                                , eitherDecode
                                                )
import           Data.Proxy                     ( Proxy(..) )



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

newtype ApiResponse req = ApiResponse (Response LBS.ByteString)

runApiRequest
    :: (Monad m, MonadIO m, ApiRequest a) => a -> SessionT m (ApiResponse a)
runApiRequest a = do
    req <- asks sessionDefaultReq <&> \req -> req
        { method      = getApiRequestMethod a
        , path        = LBS.toStrict
                        .  toLazyByteString
                        $  "/api/v4"
                        <> getApiRequestPath a
        , requestBody = RequestBodyLBS $ fromMaybe "" (getApiRequestBody a)
        }
    ApiResponse <$> runReq req

-- TODO: better errors
parseResponse
    :: forall status req resp
     . (KnownSymbol status, FromJSON resp, RequestResponse req status ~ resp)
    => Proxy status
    -> ApiResponse req
    -> Either String resp
parseResponse Proxy (ApiResponse resp) = if respStatus == expectedStatus
    then eitherDecode (responseBody resp)
    else Left "wrong status code"
  where
    respStatus     = show . statusCode . responseStatus $ resp
    expectedStatus = symbolVal (Proxy @status)
