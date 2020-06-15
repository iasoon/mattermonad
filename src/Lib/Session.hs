module Lib.Session where

import Data.ByteString as BS
import Network.HTTP.Client (Request, HasHttpManager, getHttpManager)
import Control.Monad.Reader


import Lib.HTTP

data SessionData = SessionData
    { sessionToken :: BS.ByteString
    , sessionClient :: ApiClient
    }

instance HasHttpManager SessionData where
    getHttpManager = getHttpManager . sessionClient

type SessionT m = ReaderT SessionData m

sessionDefaultReq :: SessionData -> Request
sessionDefaultReq SessionData { .. }
    = addBearerToken sessionToken
    $ mkServerRequest $ clientServerData sessionClient

