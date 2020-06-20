{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Lib.Schema where

import qualified Data.Yaml as Yaml
import Language.Haskell.TH
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson.TH as A (deriveFromJSON)
import qualified Data.HashMap.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Lib.Utils
import Control.Monad (forM)
import Data.Tuple (uncurry)

import GHC.TypeLits

data SchemaType = StringType
                | NumberType
                | IntegerType
                | BooleanType
                | ObjectType
                | ArrayType
                | NullType
                | AnyType
                deriving (Show)

instance A.FromJSON SchemaType where
    parseJSON = A.withText "SchemaType" $ \case
        "string"  -> return StringType
        "number"  -> return NumberType
        "integer" -> return IntegerType
        "boolean" -> return BooleanType
        "object"  -> return ObjectType
        "array"   -> return ArrayType
        "null"    -> return NullType
        "any"     -> return AnyType
        t         -> fail $ "not a valid type: " ++ T.unpack t

data Property = Property
    { propertyName :: Text
    , propertyType :: SchemaType
    } deriving (Show)
$(A.deriveFromJSON (removePrefix "property") ''Property)

data ObjectSchema = ObjectSchema
    { objectProperties :: [Property]
    } deriving (Show)

parseProperty :: Text -> A.Value -> A.Parser Property
parseProperty propertyName = A.withObject (T.unpack propertyName) $ \props -> do
    propertyType <- props .: "type"
    return Property {..}

parseProperties :: A.Value -> A.Parser [Property]
parseProperties = A.withObject "properties" $ mapM (uncurry parseProperty) . M.toList

instance A.FromJSON ObjectSchema where
    parseJSON = A.withObject "ObjectSchema" $ \o -> do
        objectProperties <- o .: "properties" >>= parseProperties
        return ObjectSchema {..}

type Schema = M.HashMap String ObjectSchema
test :: IO ()
test = do
    schema :: Schema <- Yaml.decodeFileThrow "schema.yaml"
    putStrLn $ show schema


type family PropertyType (ty :: *) (name :: Symbol) :: *

makePropValue :: Name -> [(String, Name)]-> Q [Dec]
makePropValue name assocs = do
    -- sym <- newName "sym"
    -- let head = TypeFamilyHead name [(KindedTV sym (ConT ''Symbol))] (KindSig StarT) Nothing
    let propType = \t -> AppT (AppT (ConT ''PropertyType) (ConT name)) t
    let instDecls = flip map assocs $ \(str, tyname) -> 
            TySynInstD $ TySynEqn Nothing (propType (LitT $ StrTyLit str)) (ConT tyname)
    return $ instDecls
