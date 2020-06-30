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
    , generatorQueue :: [Text]
    } deriving (Show)

emptyState = GeneratorState { generatorTypeMap = M.empty, generatorQueue = [] }

data TyProps = TyProps
    { tyName :: Name
    } deriving (Show)

runGen :: Generator a -> Q (a, GeneratorState)
runGen (Generator gen) = runStateT gen emptyState

newtype Generator a = Generator (StateT GeneratorState Q a)
    deriving (Functor, Applicative, Monad, MonadFail)


data SchemaComponent = SchemaComponent String
                     deriving (Show)

parseSchemaComponent :: String -> SchemaComponent
parseSchemaComponent str = case splitOn '/' str of
    ["#", "components", "schemas", name] -> SchemaComponent name
    _ -> error "unknown component"

genQueue :: ApiSpec -> Generator [Dec]
genQueue spec = do
    res <- popQueue
    case res of
        Nothing   -> return []
        Just item -> do
            let comp = parseSchemaComponent $ T.unpack item
            dec <- genComponent spec comp
            fmap (dec :) $ genQueue spec

popQueue :: Generator (Maybe Text)
popQueue = Generator $ do
    state <- get
    case generatorQueue state of
        []            -> return Nothing
        (item : rest) -> do
            put $ state { generatorQueue = rest }
            return $ Just item

genComponent :: ApiSpec -> SchemaComponent -> Generator Dec
genComponent apiSpec (SchemaComponent key) = do
    let
        val = M.lookup (T.pack key) $ componentsSchemas $ apiSpecComponents
            apiSpec
    case val of
        Nothing        -> fail "component schema not found"
        Just objSchema -> objectDecl (mkName key) objSchema


objectDecl :: Name -> ObjectSchema -> Generator Dec
objectDecl name schema =
    let objName  = nameBase name
        propName = camelCase . ((decapitalize objName) :) . unSnakeCase
    in  do
            props <-
                mapM (propVarBangType propName) $ M.toList $ objectProperties
                    schema
            return $ DataD [] name [] Nothing [RecC name props] []

genOperation :: ApiSpec -> OperationKey -> Name -> Generator Dec
genOperation spec opKey name = do
    let val      = findOp opKey $ apiSpecOperations spec
        objName  = nameBase name
        propName = camelCase . ((decapitalize objName) :) . unSnakeCase
    case val of
        Nothing       -> fail "operation schema not found"
        Just opSchema -> do
            props <- mapM (mkParamField propName) $ operationParameters opSchema
            return $ DataD [] name [] Nothing [RecC name props] []

mkParamField :: (String -> String) -> Parameter -> Generator VarBangType
mkParamField nameF param = do
    baseTy <- maybe (pure $ ConT ''Text) getType (parameterSchema param)
    let ty = if parameterRequired param then baseTy else maybeTy baseTy
    return (name, bang, ty)
  where
    name    = mkName $ nameF $ T.unpack $ parameterName param
    bang    = Bang NoSourceUnpackedness NoSourceStrictness
    maybeTy = AppT (ConT ''Maybe)

propVarBangType
    :: (String -> String) -> (Text, SchemaValue) -> Generator VarBangType
propVarBangType nameF (propName, propType) = do
    ty <- getType propType
    return (name, bang, ty)
  where
    name = mkName $ nameF $ T.unpack $ propName
    bang = Bang NoSourceUnpackedness NoSourceStrictness

getType :: SchemaValue -> Generator Type
getType (Lit lit ) = return $ valueSchemaRepr lit
getType (Ref path) = do
    enqueue path
    let SchemaComponent compName = parseSchemaComponent $ T.unpack path
    let tyName                   = mkName compName
    return $ ConT tyName

enqueue :: Text -> Generator ()
enqueue ref = Generator $ modify $ \state ->
    state { generatorQueue = ref : (generatorQueue state) }

lookupRefName :: Text -> Generator (Maybe Name)
lookupRefName ref =
    Generator $ fmap tyName . M.lookup ref . generatorTypeMap <$> get

valueSchemaRepr :: ValueSchema -> Type
valueSchemaRepr StringTy               = ConT ''Text
valueSchemaRepr IntegerTy              = ConT ''Int
valueSchemaRepr NumberTy               = ConT ''Double
valueSchemaRepr BoolTy                 = ConT ''Bool
valueSchemaRepr (ObjectTy objSchema  ) = ConT ''A.Object
valueSchemaRepr (ArrayTy  arraySchema) = ConT ''A.Array

splitOn :: Char -> String -> [String]
splitOn _ []       = []
splitOn t (c : cs) = if c == t
    then splitOn t cs
    else let (seg, rest) = span (/= t) cs in (c : seg) : (splitOn t rest)

unSnakeCase :: String -> [String]
unSnakeCase = map (map Char.toLower) . splitOn '_'

camelCase :: [String] -> String
camelCase []       = ""
camelCase (s : ss) = concat $ s : (map capitalize ss)
  where
    capitalize ""       = ""
    capitalize (c : cs) = Char.toUpper c : cs

decapitalize :: String -> String
decapitalize []       = []
decapitalize (c : cs) = Char.toLower c : cs
