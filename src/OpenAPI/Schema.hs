{-# LANGUAGE TemplateHaskell #-}


module OpenAPI.Schema where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.TH as A (deriveFromJSON)
import qualified Data.Yaml as Yaml
import qualified Data.HashMap.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson ((.:), (.:?))
import Lib.Utils
import Control.Monad (forM)
import Data.Maybe (fromMaybe)

test :: IO ApiSpec
test = Yaml.decodeFileThrow "schema.yaml"

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

data ValueSchema = ObjectTy ObjectSchema
                 | ArrayTy ArraySchema
                 | SchemaTy SchemaType
                 deriving (Show)

instance A.FromJSON ValueSchema where
    parseJSON value = do
        ty <- A.withObject "schema" (.: "type") value
        case ty of
            ObjectType -> ObjectTy <$> A.parseJSON value
            ArrayType -> ArrayTy <$> A.parseJSON value
            ty -> return $ SchemaTy ty

data ObjectSchema = ObjectSchema
    { objectProperties :: [Property]
    } deriving (Show)

instance A.FromJSON ObjectSchema where
    parseJSON = A.withObject "ObjectSchema" $ \o -> do
        objectProperties <- o .: "properties" >>= parseProperties
        return ObjectSchema {..}

data ArraySchema = ArraySchema
    { arrayItems :: ValueSchema
    } deriving (Show)

instance A.FromJSON ArraySchema where
    parseJSON = A.withObject "ArraySchema" $ \o -> do
        arrayItems <- o .: "items"
        return ArraySchema {..}


data Property = Property
    { propertyName :: Text
    , propertyType :: SchemaType
    } deriving (Show)

parseProperty :: Text -> A.Value -> A.Parser Property
parseProperty propertyName = A.withObject (T.unpack propertyName) $ \props -> do
    propertyType <- props .: "type"
    return Property {..}

parseProperties :: A.Value -> A.Parser [Property]
parseProperties = A.withObject "properties" $ mapM (uncurry parseProperty) . M.toList

data Operation = Operation
    { operationMethod :: Text
    , operationPath :: Text
    , operationParameters :: [Parameter]
    , operationRequestBody :: Maybe RequestBody
    , operationResponses :: M.HashMap Text (RefOrLit Response)
    } deriving (Show)

data RequestBody = RequestBody
    { requestBodySchema :: ValueSchema
    , requestBodyRequired :: Bool
    } deriving (Show)

instance A.FromJSON RequestBody where
    parseJSON = A.withObject "RequestBody" $ \obj -> do
        requestBodyRequired <- fromMaybe False <$> obj .:? "required"
        requestBodySchema <- obj .: "content"
            >>= A.withObject "content" (.: "application/json")
            >>= A.withObject "" (.: "schema")
            >>= A.parseJSON
        return RequestBody { .. }

data Parameter = Parameter
    { parameterName :: Text
    , parameterIn :: Text
    , parameterRequired :: Bool
    } deriving (Show)

instance A.FromJSON Parameter where
    parseJSON = A.withObject "RequestParameter" $ \obj -> do
        parameterName <- obj .: "name"
        parameterIn <- obj .: "in"
        parameterRequired <- fromMaybe False <$> obj .:? "required"
        return Parameter { .. }


data Response = Response
    { responseDescription::Text
    , responseContentSchema :: RefOrLit ValueSchema
    } deriving (Show)

instance A.FromJSON Response where
    parseJSON = A.withObject "Response" $ \ obj -> do
        responseDescription <- obj .: "description"
        responseContentSchema <- obj .: "content"
            >>= A.withObject "content" (.: "application/json")
            >>= A.withObject "" (.: "schema")
            >>= A.parseJSON
        return Response { .. }


data RefOrLit a = Ref Text | Lit a deriving (Show)

parseRefOr :: (A.Value -> A.Parser a) -> A.Value -> A.Parser (RefOrLit a)
parseRefOr litParser (A.Object o) = (o .:? "$ref") >>= \case
    Just ref -> return $ Ref ref
    Nothing  -> Lit <$> litParser (A.Object o)
parseRefOr litParser value = Lit <$> litParser value

instance A.FromJSON a => A.FromJSON (RefOrLit a) where
    parseJSON = parseRefOr A.parseJSON

data Components = Components
    { componentsSchemas :: M.HashMap Text ObjectSchema
    } deriving (Show)

data ApiSpec = ApiSpec
    { apiSpecComponents :: Components
    , apiSpecOperations :: [Operation]
    } deriving (Show)

instance A.FromJSON ApiSpec where
    parseJSON = A.withObject "ApiSpec" $ \obj -> do
        apiSpecComponents <- obj .: "components"
        apiSpecOperations <- obj .: "paths" >>= parsePaths
        return ApiSpec { .. }

parsePaths :: A.Value -> A.Parser [Operation]
parsePaths = A.withObject "paths" $ 
    fmap concat .mapM (uncurry parsePathOps) . M.toList
    where   parsePathOps path = A.withObject (T.unpack path)
                (mapM (uncurry (parseOp path)) . M.toList)

parseOp :: Text -> Text -> A.Value -> A.Parser Operation
parseOp path method = A.withObject (T.unpack method) $ \obj -> do
    operationParameters <- fromMaybe [] <$> obj .:? "parameters"
    operationRequestBody <- obj .:? "requestBody"
    operationResponses <- obj .: "responses"
    let operationPath = path
        operationMethod = method
    return Operation { .. }


$(A.deriveFromJSON (removePrefix "property") ''Property)
$(A.deriveFromJSON (removePrefix "components") ''Components)