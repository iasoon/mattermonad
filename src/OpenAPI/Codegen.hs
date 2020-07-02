{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module OpenAPI.Codegen where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           OpenAPI.Schema
import qualified Data.Char                     as Char
import qualified Data.Aeson                    as A
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Yaml                     as Yaml
import qualified Data.HashMap.Strict           as M
import           Data.Maybe
import           Control.Monad.Trans.State

apiSpec :: IO ApiSpec
apiSpec = Yaml.decodeFileThrow "mattermost-openapi-v4.yaml"

data GeneratorState = GeneratorState
    { generatorTypeMap :: M.HashMap Text TyProps
    , generatorQueue :: [QueueElem]
    } deriving (Show)

data QueueElem = QueueRef Text | QueueObj Name ObjectSchema deriving (Show)

data TyProps = TyProps
    { tyName :: Name
    } deriving (Show)

emptyState = GeneratorState { generatorTypeMap = M.empty, generatorQueue = [] }

enqueue :: QueueElem -> Generator ()
enqueue elem = Generator $ modify $ \state ->
    state { generatorQueue = elem : generatorQueue state }

popQueue :: Generator (Maybe QueueElem)
popQueue = Generator $ do
    state <- get
    case generatorQueue state of
        []            -> return Nothing
        (item : rest) -> do
            put $ state { generatorQueue = rest }
            return $ Just item


runGen :: Generator a -> Q (a, GeneratorState)
runGen (Generator gen) = runStateT gen emptyState

newtype Generator a = Generator (StateT GeneratorState Q a)
    deriving (Functor, Applicative, Monad, MonadFail)


data SchemaComponent = SchemaComponent String
                     deriving (Show)

finishQueue :: ApiSpec -> Generator [Dec]
finishQueue spec = do
    res <- popQueue
    case res of
        Nothing   -> return []
        Just item -> do
            dec <- case item of
                QueueRef path ->
                    let comp = parseSchemaComponent $ T.unpack path
                    in  genComponent spec comp
                QueueObj name schema -> objectDecl name schema
            fmap (dec :) $ finishQueue spec


parseSchemaComponent :: String -> SchemaComponent
parseSchemaComponent str = case splitOn '/' str of
    ["#", "components", "schemas", name] -> SchemaComponent name
    _ -> error "unknown component"



genComponent :: ApiSpec -> SchemaComponent -> Generator Dec
genComponent apiSpec (SchemaComponent key) = do
    let
        val = M.lookup (T.pack key) $ componentsSchemas $ apiSpecComponents
            apiSpec
    case val of
        Nothing        -> fail "component schema not found"
        Just objSchema -> objectDecl (mkName key) objSchema


objectDecl :: Name -> ObjectSchema -> Generator Dec
objectDecl name ObjectSchema {..} =
    let objName  = nameBase name
        propName = camelCase . (decapitalize objName :) . unSnakeCase
    in  do
            props <- mapM (mkPropField propName) $ M.elems objectProperties
            return $ DataD [] name [] Nothing [RecC name props] []

mkPropField :: (String -> String) -> Property -> Generator VarBangType
mkPropField nameF Property {..} = do
    ty <- tyRequired propertyIsRequired <$> makeType name propertySchema
    return $ recordField name ty
    where name = nameF . T.unpack $ propertyName


genOperation :: ApiSpec -> OperationKey -> Name -> Generator Dec
genOperation spec opKey name = do
    let val      = findOp opKey $ apiSpecOperations spec
        objName  = nameBase name
        propName = camelCase . ((decapitalize objName) :) . unSnakeCase
    case val of
        Nothing       -> fail "operation schema not found"
        Just opSchema -> do
            paramProps <- mapM (mkParamField propName)
                $ operationParameters opSchema
            props <- case operationRequestBody opSchema of
                Nothing      -> return paramProps
                Just reqBody -> do
                    ty <- tyRequired (requestBodyRequired reqBody) <$> makeType
                        (propName $ "payload")
                        (requestBodySchema reqBody)
                    let field = recordField (propName "payload") ty
                    return $ field : paramProps
            return $ DataD [] name [] Nothing [RecC name props] []

mkParamField :: (String -> String) -> Parameter -> Generator VarBangType
mkParamField nameF param = do
    ty <- tyRequired (parameterRequired param) <$> paramTy param
    return $ recordField name ty
  where
    name    = nameF $ T.unpack $ parameterName param
    paramTy = maybe (pure $ ConT ''Text) getType . parameterSchema

recordField :: String -> Type -> VarBangType
recordField name ty = (mkName name, bang, ty)
    where bang = Bang NoSourceUnpackedness NoSourceStrictness

tyRequired :: Bool -> Type -> Type
tyRequired True  = id
tyRequired False = AppT (ConT ''Maybe)


getType :: SchemaValue -> Generator Type
getType (Lit lit ) = return $ simpleType lit
getType (Ref path) = do
    enqueue (QueueRef path)
    let SchemaComponent compName = parseSchemaComponent $ T.unpack path
    let tyName                   = mkName compName
    return $ ConT tyName

makeType :: String -> SchemaValue -> Generator Type
makeType tyName (Lit (ObjectTy schema)) = if M.null $ objectProperties schema
    then return $ ConT ''A.Object
    else do
        let name = mkName $ capitalize tyName
        enqueue (QueueObj name schema)
        return $ ConT name
makeType tyName (Lit (ArrayTy schema)) = do
    itemTy <- makeType tyName $ arrayItems schema
    return $ AppT ListT itemTy
makeType _ (Lit lit ) = return $ simpleType lit
makeType _ (Ref path) = do
    enqueue (QueueRef path)
    let SchemaComponent compName = parseSchemaComponent $ T.unpack path
    let tyName                   = mkName compName
    return $ ConT tyName

lookupRefName :: Text -> Generator (Maybe Name)
lookupRefName ref =
    Generator $ fmap tyName . M.lookup ref . generatorTypeMap <$> get

simpleType :: ValueSchema -> Type
simpleType StringTy     = ConT ''Text
simpleType IntegerTy    = ConT ''Int
simpleType NumberTy     = ConT ''Double
simpleType BoolTy       = ConT ''Bool
simpleType (ObjectTy _) = ConT ''A.Object
simpleType (ArrayTy  _) = ConT ''A.Array

splitOn :: Char -> String -> [String]
splitOn _ []       = []
splitOn t (c : cs) = if c == t
    then splitOn t cs
    else let (seg, rest) = span (/= t) cs in (c : seg) : (splitOn t rest)

unSnakeCase :: String -> [String]
unSnakeCase = map (map Char.toLower) . splitOn '_'

camelCase :: [String] -> String
camelCase []       = ""
camelCase (s : ss) = concat $ s : map capitalize ss

capitalize :: String -> String
capitalize ""       = ""
capitalize (c : cs) = Char.toUpper c : cs

decapitalize :: String -> String
decapitalize []       = []
decapitalize (c : cs) = Char.toLower c : cs
