{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module OpenAPI.Lib where

import qualified Network.HTTP.Client           as HTTP
import qualified Network.HTTP.Types            as HTTP
import           Data.ByteString                ( ByteString )
import           Data.ByteString.Builder        ( Builder )
import qualified Data.ByteString.Lazy          as LBS
import           GHC.TypeLits
import           Data.Proxy
import qualified Data.Aeson                    as A

class ApiRequest a where
    getApiRequestMethod :: a -> HTTP.Method
    getApiRequestPath :: a -> Builder
    getApiRequestBody :: a -> Maybe LBS.ByteString

type family RequestResponse req (status :: Symbol) :: (resp :: *)
