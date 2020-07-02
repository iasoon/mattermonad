{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module OpenAPI.Schema where

import qualified Data.Aeson                    as A
import qualified Data.Aeson.Types              as A
import qualified Data.Aeson.TH                 as A
                                                ( deriveFromJSON )
import qualified Data.HashMap.Strict           as M
import qualified Data.HashSet                  as S
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , (<?>)
                                                )
import           Lib.Utils
import           Control.Monad                  ( forM )
import           Data.Maybe                     ( fromMaybe )
import           Control.Monad.Trans.Maybe
import           Data.Hashable
import           Data.Functor                   ( (<&>) )
import           Control.Monad.Trans            ( lift )

data ApiSpec = ApiSpec
    { apiSpecComponents :: Components
    , apiSpecOperations :: Operations
    } deriving (Show)

data ValueSchema = ObjectTy ObjectSchema
                 | ArrayTy ArraySchema
                 | StringTy
                 | IntegerTy
                 | NumberTy
                 | BoolTy
                 deriving (Show)

type SchemaValue = RefOrLit ValueSchema

instance A.FromJSON ValueSchema where
    parseJSON value = do
        tyname :: Text <- A.withObject "schema" (.: "type") value
        case tyname of
            "string"  -> return StringTy
            "number"  -> return NumberTy
            "integer" -> return IntegerTy
            "boolean" -> return BoolTy
            "object"  -> parseSchema ObjectTy
            "array"   -> parseSchema ArrayTy
        where parseSchema con = con <$> A.parseJSON value

data ObjectSchema = ObjectSchema
    { objectProperties :: M.HashMap Text Property
    } deriving (Show)

data Property = Property
    { propertyName :: Text
    , propertySchema :: SchemaValue
    , propertyIsRequired :: Bool
    } deriving (Show)

parseProperties :: A.Object -> A.Parser [Property]
parseProperties obj = do
    propSchemas   <- fromMaybe M.empty <$> obj .:? "properties"
    requiredProps <- fromMaybe S.empty <$> obj .:? "required"
    forM (M.toList propSchemas) (uncurry (parseProperty requiredProps))

parseProperty :: (S.HashSet Text) -> Text -> A.Value -> A.Parser Property
parseProperty requiredProps name value = do
    schema <- A.parseJSON value
    return $ Property { propertyName       = name
                      , propertySchema     = schema
                      , propertyIsRequired = S.member name requiredProps
                      }

instance A.FromJSON ObjectSchema where
    parseJSON = A.withObject "ObjectSchema" $ \o -> do
        objectProperties <- indexBy propertyName <$> parseProperties o
        return ObjectSchema { .. }

data ArraySchema = ArraySchema
    { arrayItems :: RefOrLit ValueSchema
    } deriving (Show)

instance A.FromJSON ArraySchema where
    parseJSON = A.withObject "ArraySchema" $ \o -> do
        arrayItems <- o .: "items"
        return ArraySchema { .. }

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

instance A.FromJSON ApiSpec where
    parseJSON = A.withObject "ApiSpec" $ \obj -> do
        apiSpecComponents <- obj .: "components"
        apiSpecOperations <- obj .: "paths"
        return ApiSpec { .. }

data RequestBody = RequestBody
    { requestBodySchema :: RefOrLit ValueSchema
    , requestBodyContentType :: Text
    , requestBodyRequired :: Bool
    } deriving (Show)

instance A.FromJSON RequestBody where
    parseJSON = A.withObject "RequestBody" $ \obj -> do
        (ty, typeObj) <-
            obj .: "content" >>= A.withObject "content" fromSingleton
        schema              <- A.withObject "" (.: "schema") typeObj
        requestBodyRequired <- fromMaybe False <$> obj .:? "required"

        return RequestBody { requestBodySchema      = schema
                           , requestBodyContentType = ty
                           , ..
                           }
        where fromSingleton o = let [entry] = M.toList o in return entry

data Parameter = Parameter
    { parameterName :: Text
    , parameterIn :: Text
    , parameterRequired :: Bool
    , parameterSchema :: Maybe SchemaValue
    } deriving (Show)

instance A.FromJSON Parameter where
    parseJSON = A.withObject "RequestParameter" $ \obj -> do
        parameterName     <- obj .: "name"
        parameterIn       <- obj .: "in"
        parameterRequired <- fromMaybe False <$> obj .:? "required"
        parameterSchema   <- obj .:? "schema"
        return Parameter { .. }

data Response = Response
    { responseDescription::Text
    , responseContentSchema :: Maybe (RefOrLit ValueSchema)
    } deriving (Show)

(>>=?) :: Monad m => MaybeT m a -> (a -> m (Maybe b)) -> MaybeT m b
(>>=?) m f = m >>= MaybeT . f

instance A.FromJSON Response where
    parseJSON = A.withObject "Response" $ \obj -> do
        responseDescription   <- obj .: "description"
        responseContentSchema <-
            runMaybeT
            $    return obj
            >>=? (.:? "content")
            >>=? A.withObject "content" (.:? "application/json")
            >>=? A.withObject "" (.:? "schema")
        return Response { .. }

withMapEntries :: (Text -> A.Value -> A.Parser a) -> A.Value -> A.Parser [a]
withMapEntries f = A.withObject "map object" (mapM parseEntry . M.toList)
    where parseEntry (k, v) = f k v <?> A.Key k

newtype Operations = Operations (M.HashMap OperationKey Operation)
    deriving (Show)

data OperationKey = OperationKey
    { opKeyPath :: Text
    , opKeyMethod :: Text
    } deriving (Show, Eq)

instance Hashable OperationKey where
    hashWithSalt salt OperationKey { opKeyPath, opKeyMethod } =
        salt `hashWithSalt` opKeyPath `hashWithSalt` opKeyMethod

data Operation = Operation
    { operationMethod :: Text
    , operationPath :: Text
    , operationParameters :: [Parameter]
    , operationRequestBody :: Maybe RequestBody
    , operationResponses :: M.HashMap Text (RefOrLit Response)
    } deriving (Show)

getOpKey :: Operation -> OperationKey
getOpKey op = OperationKey { opKeyPath   = operationPath op
                           , opKeyMethod = operationMethod op
                           }

opKey :: Text -> Text -> OperationKey
opKey method path = OperationKey { opKeyPath = path, opKeyMethod = method }

opsFromList :: [Operation] -> Operations
opsFromList = Operations . indexBy getOpKey

findOp :: OperationKey -> Operations -> Maybe Operation
findOp key (Operations m) = M.lookup key m

instance A.FromJSON Operations where
    parseJSON = fmap opsFromList . parsePaths

parsePaths :: A.Value -> A.Parser [Operation]
parsePaths = fmap concat . withMapEntries parsePathOps
    where parsePathOps path = withMapEntries (parseOp path)

parseOp :: Text -> Text -> A.Value -> A.Parser Operation
parseOp path method = A.withObject (T.unpack method) $ \obj -> do
    operationParameters  <- fromMaybe [] <$> obj .:? "parameters"
    operationRequestBody <- obj .:? "requestBody"
    operationResponses   <- obj .: "responses"
    let operationPath   = path
        operationMethod = method
    return Operation { .. }

indexBy :: (Hashable a, Eq a) => (t -> a) -> [t] -> M.HashMap a t
indexBy f = M.fromList . map (\t -> (f t, t))

$(A.deriveFromJSON (removePrefix "components") ''Components)
