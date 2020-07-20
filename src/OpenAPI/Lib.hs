{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE UndecidableInstances #-}

module OpenAPI.Lib where

import qualified Network.HTTP.Client           as HTTP
import qualified Network.HTTP.Types            as HTTP
import           Data.ByteString                ( ByteString )
import           Data.ByteString.Builder        ( Builder )
import qualified Data.ByteString.Lazy          as LBS
import           GHC.TypeLits
import           Data.Proxy
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Types              as A
import           Data.Text                      ( Text )
import           Control.Monad                  ( (<=<) )
import qualified Data.HashMap.Strict           as M

class ApiRequest a where
    getApiRequestMethod :: a -> HTTP.Method
    getApiRequestPath :: a -> Builder
    getApiRequestBody :: a -> Maybe LBS.ByteString

type family RequestResponse req (status :: Symbol) :: (resp :: *)

class Sing a where
    sing :: a

data RequiredProp = RequiredProp

instance Sing RequiredProp where
    sing = RequiredProp

data OptionalProp = OptionalProp

instance Sing OptionalProp where
    sing = OptionalProp

class (Sing m, PropRepr m param ~ repr ) => PropertyType m param repr |  m param -> repr , m repr -> param where
    type PropRepr m param :: *

    decodeValue :: m -> Maybe A.Value -> A.Parser repr
    encodeValue :: m -> repr -> Maybe A.Value

instance (A.FromJSON a, A.ToJSON a) => PropertyType RequiredProp a a where
    type PropRepr RequiredProp a = a

    decodeValue RequiredProp = A.parseJSON <=< pFromMaybe
    encodeValue RequiredProp = Just . A.toJSON

instance (A.FromJSON a, A.ToJSON a) => PropertyType OptionalProp a (Maybe a) where
    type PropRepr OptionalProp a = Maybe a

    decodeValue OptionalProp = maybe (pure Nothing) A.parseJSON
    encodeValue OptionalProp = fmap A.toJSON

retrievePropValue
    :: PropertyType m param repr => m -> Text -> A.Object -> A.Parser repr
retrievePropValue decoder propName = decodeValue decoder . M.lookup propName

encodePropValue
    :: PropertyType m param repr => m -> Text -> repr -> Maybe A.Pair
encodePropValue encoder propName = fmap (propName A..=) . encodeValue encoder

pFromMaybe :: Maybe a -> A.Parser a
pFromMaybe (Just a) = pure a
pFromMaybe Nothing  = fail "No value"

