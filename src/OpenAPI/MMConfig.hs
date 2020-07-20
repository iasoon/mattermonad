{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module OpenAPI.MMConfig where

import qualified Data.Aeson                    as A
import qualified Data.Aeson.Types              as A
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified Data.Text                     as T

import           OpenAPI.Codegen
import           OpenAPI.Schema
import           OpenAPI.Lib

data MMBool = MMBool

instance Sing MMBool where
    sing = MMBool

instance PropertyEncoding MMBool Bool Bool where
    type PropRepr MMBool Bool = Bool

    encodeValue MMBool = Just . A.toJSON
    decodeValue MMBool = maybe (pure False) parseBool


mmConfig = ObjConfig { objPropEncoding = mmPropEncoding }

mmPropEncoding :: Property -> Name
mmPropEncoding Property {..} = case propertySchema of
    Lit BoolTy -> ''MMBool
    _          -> if propertyIsRequired then ''RequiredProp else ''OptionalProp


parseBool :: A.Value -> A.Parser Bool
parseBool (A.Bool   val    ) = return val
parseBool (A.String "true" ) = return True
parseBool (A.String "false") = return False
parseBool val                = A.typeMismatch "boolean" val
