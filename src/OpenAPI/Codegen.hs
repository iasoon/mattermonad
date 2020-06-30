{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module OpenAPI.Codegen where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import OpenAPI.Schema
import qualified Data.Char as Char
import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Data.HashMap.Strict as M
import Control.Monad.Trans.State

apiSpec :: IO ApiSpec
apiSpec = Yaml.decodeFileThrow "mattermost-openapi-v4.yaml"

userDef = M.lookup "User" . componentsSchemas . apiSpecComponents <$> apiSpec
timezoneDef = M.lookup "Timezone" . componentsSchemas . apiSpecComponents <$> apiSpec

data GeneratorState = GeneratorState
    { generatorTypeMap :: M.HashMap Text TyProps
    , generatorQueue :: [Text]
    } deriving (Show)

emptyState = GeneratorState
    { generatorTypeMap = M.empty
    , generatorQueue = []
    }

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
        Nothing -> return []
        Just item -> do
            let comp = parseSchemaComponent $ T.unpack item
            dec <- genComponent spec comp
            fmap (dec:) $ genQueue spec

popQueue :: Generator (Maybe Text)
popQueue = Generator $ do
    state <- get
    case generatorQueue state of
        [] -> return Nothing
        (item:rest) -> do
            put $ state { generatorQueue = rest } 
            return $ Just item

genComponent :: ApiSpec -> SchemaComponent -> Generator Dec
genComponent apiSpec (SchemaComponent key) = do
    let val = M.lookup (T.pack key) $ componentsSchemas $ apiSpecComponents apiSpec
    case val of
        Nothing -> fail "component schema not found"
        Just objSchema -> do
            objectDecl (mkName key) objSchema


objectDecl :: Name -> ObjectSchema -> Generator Dec
objectDecl name schema =
    let objName = nameBase name
        propName = camelCase . ((unSnakeCase objName) ++) . unSnakeCase
    in do
        props <- mapM (propVarBangType propName) $ M.toList $ objectProperties schema
        return $ DataD [] name [] Nothing
            [ RecC name props ]
            []

propVarBangType :: (String -> String) -> (Text, SchemaValue) -> Generator VarBangType
propVarBangType nameF (propName, propType) = do
    ty <- getType propType
    return (name, bang, ty)
    where name = mkName $ nameF $ T.unpack $ propName
          bang = Bang NoSourceUnpackedness NoSourceStrictness

getType :: SchemaValue -> Generator Type
getType (Lit lit) = return $ valueSchemaRepr lit
getType (Ref path) = do
    enqueue path
    let SchemaComponent compName = parseSchemaComponent $ T.unpack path
    let tyName = mkName compName
    return $ ConT tyName

enqueue :: Text -> Generator ()
enqueue ref = Generator $ modify $ \state ->
    state { generatorQueue = ref:(generatorQueue state)}

lookupRefName :: Text -> Generator (Maybe Name)
lookupRefName ref = Generator $ do
    state <- get
    return $ fmap tyName $ M.lookup ref $ generatorTypeMap state

valueSchemaRepr :: ValueSchema -> Type
valueSchemaRepr StringTy = ConT ''Text
valueSchemaRepr IntegerTy = ConT ''Int
valueSchemaRepr NumberTy = ConT ''Double
valueSchemaRepr BoolTy = ConT ''Bool
valueSchemaRepr (ObjectTy objSchema) = ConT ''A.Object
valueSchemaRepr (ArrayTy arraySchema) = ConT ''A.Array


splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn t (c:cs) = if c == t
    then splitOn t cs
    else let (seg, rest) = span (/= t) cs in (c:seg):(splitOn t rest)

unSnakeCase :: String -> [String]
unSnakeCase = map (map Char.toLower) . splitOn '_'

camelCase :: [String] -> String
camelCase [] = ""
camelCase (s:ss) = concat $ s:(map capitalize ss)
    where   capitalize "" = ""
            capitalize (c:cs) = (Char.toUpper c):cs
