{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Lib.Schema where

import qualified Data.Yaml as Yaml
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
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
import Data.List (span)
import qualified Data.Char as Char

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

propReprType :: Property -> Type
propReprType p = case propertyType p of
    StringType  -> ConT ''Text
    NumberType  -> ConT ''Double
    IntegerType -> ConT ''Int
    BooleanType -> ConT ''Bool
    ArrayType   -> ConT ''A.Array
    ObjectType  -> ConT ''A.Object
    AnyType     -> ConT ''A.Value
    _ -> error ("no tpe  repr for " ++ show (propertyType p))

data ObjectSchema = ObjectSchema
    { objectProperties :: [Property]
    } deriving (Show)

parseProperty :: Text -> A.Value -> A.Parser Property
parseProperty propertyName = A.withObject (T.unpack propertyName) $ \props -> do
    propertyType <- props .: "type"
    return Property {..}

parseProperties :: A.Value -> A.Parser [Property]
parseProperties = A.withObject "properties" $ mapM (uncurry parseProperty) . M.toList

readSchema :: String ->  Q Schema
readSchema = runIO . Yaml.decodeFileThrow

genDataTypes :: String -> Q [Dec]
genDataTypes path = do
    schema <- readSchema "schema.yaml"
    forM (M.toList schema) $ \(name, objSchema) -> return $ objectDatatype (name) objSchema


objectDatatype :: String -> ObjectSchema -> Dec
objectDatatype objName schema = 
    let name = mkName objName 
        propName = camelCase . ((unSnakeCase objName) ++) . unSnakeCase
    in
    DataD [] name [] Nothing 
        [ RecC name $ map (propVarBangType propName) $ objectProperties schema]
        []

propVarBangType :: (String -> String) -> Property -> VarBangType
propVarBangType propName prop = (name, bang, ty)
    where name = mkName $ propName $ T.unpack $ propertyName prop
          bang = Bang NoSourceUnpackedness NoSourceStrictness
          ty   = propReprType prop

unSnakeCase :: String -> [String]
unSnakeCase [] = []
unSnakeCase ('_':cs) = unSnakeCase cs
unSnakeCase cs = let (seg, rest) = span (/= '_') cs in (map Char.toLower seg):(unSnakeCase rest)

camelCase :: [String] -> String
camelCase [] = ""
camelCase (s:ss) = concat $ s:(map capitalize ss)
    where   capitalize "" = ""
            capitalize (c:cs) = (Char.toUpper c):cs

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