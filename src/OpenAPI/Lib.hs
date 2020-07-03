module OpenAPI.Lib where

import qualified Network.HTTP.Client           as HTTP
import qualified Network.HTTP.Types            as HTTP
import           Data.ByteString                ( ByteString )
import           Data.ByteString.Builder        ( Builder )
import qualified Data.ByteString.Lazy          as LBS

class ApiRequest a where
    getApiRequestMethod :: a -> HTTP.Method
    getApiRequestPath :: a -> Builder
    getApiRequestBody :: a -> Maybe LBS.ByteString
