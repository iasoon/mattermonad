{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module OpenAPI.Codegen where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
                                         hiding ( lift )
import           OpenAPI.Schema
import qualified Data.Char                     as Char
import qualified Data.Aeson                    as A
import           Data.Aeson ((.=))
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Yaml                     as Yaml
import qualified Data.HashMap.Strict           as M
import           Data.Maybe
import           Control.Monad.Trans.State
import           Data.Aeson.TH                 as A
import           Lib.Utils
import           Control.Monad.Trans            ( lift )
import           OpenAPI.Lib
import qualified Data.ByteString as BS
import qualified Network.HTTP.Types as HTTP
import           Control.Monad (forM)

generateOperations :: FilePath -> [(Text, Text, String)] -> Q [Dec]
generateOperations path opSpecs = do
    spec <- runIO $ Yaml.decodeFileThrow path
    runGen . fmap concat $
        forM opSpecs $ \(method, path, tgtName) -> concat <$>
            (genOperation spec (opKey method path) (mkName tgtName) >>= \decs -> (decs:) <$> finishQueue spec)

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


runGen :: Generator a -> Q a
runGen (Generator gen) = evalStateT gen emptyState

newtype Generator a = Generator (StateT GeneratorState Q a)
    deriving (Functor, Applicative, Monad, MonadFail)

liftQ :: Q a -> Generator a
liftQ = Generator . lift

data SchemaComponent = SchemaComponent String
                     deriving (Show)

finishQueue :: ApiSpec -> Generator [[Dec]]
finishQueue spec = do
    res <- popQueue
    case res of
        Nothing   -> return []
        Just item -> do
            decs <- case item of
                QueueRef path ->
                    let comp = parseSchemaComponent $ T.unpack path
                    in  genComponent spec comp
                QueueObj name schema -> objectDecl name schema
            (decs :) <$> finishQueue spec


parseSchemaComponent :: String -> SchemaComponent
parseSchemaComponent str = case splitOn '/' str of
    ["#", "components", "schemas", name] -> SchemaComponent name
    _ -> error "unknown component"



genComponent :: ApiSpec -> SchemaComponent -> Generator [Dec]
genComponent apiSpec (SchemaComponent key) = do
    let
        val = M.lookup (T.pack key) $ componentsSchemas $ apiSpecComponents
            apiSpec
    case val of
        Nothing        -> fail "component schema not found"
        Just objSchema -> objectDecl (mkName key) objSchema


objectDecl :: Name -> ObjectSchema -> Generator [Dec]
objectDecl name schema@ObjectSchema {..} =
    let objName  = nameBase name
        propName = camelCase . (decapitalize objName :) . unSnakeCase
    in  do
            props <- mapM (mkPropField propName) $ M.elems objectProperties
            -- TODO: please please better structure
            fmap concat . sequence $
                [ return [DataD [] name [] Nothing [RecC name props] []]
                , objectToJSON name schema
                ]

mkPropField :: (String -> String) -> Property -> Generator VarBangType
mkPropField nameF Property {..} = do
    ty <- tyRequired propertyIsRequired <$> makeType name propertySchema
    return $ recordField name ty
    where name = nameF . T.unpack $ propertyName

objectToJSON :: Name -> ObjectSchema -> Generator [Dec]
objectToJSON tyName ObjectSchema {..} =
    return [ InstanceD Nothing [] (AppT (ConT ''A.ToJSON) (ConT tyName))
        [ FunD 'A.toJSON
            [ Clause [VarP objName] (NormalB (
                eApp 'A.object $ eApp 'catMaybes $ ListE $ map maybePropPair $ M.elems objectProperties
            )) []

            ]]]
    where   varName = decapitalize $ nameBase tyName
            propName = camelCase . (varName :) . unSnakeCase
            -- TODO: Oh please, clean me up

            objName = mkName "obj"
            obj = VarE objName
            textLit = LitE . StringL . T.unpack
            eJust = AppE (ConE 'Just)
            eApp name = AppE (VarE name)
            maybePropPair Property {..} = if propertyIsRequired
                then eJust $ AppE makePairFn (propValue obj)
                else AppE (eApp 'fmap makePairFn) (propValue obj)
                where makePairFn = InfixE (Just $ textLit propertyName) (VarE '(.=)) Nothing
                      propValue = AppE (VarE . mkName . propName . T.unpack $ propertyName)

genOperation :: ApiSpec -> OperationKey -> Name -> Generator [Dec]
genOperation spec opKey name = case findOp opKey (apiSpecOperations spec) of
    Nothing       -> fail "operation schema not found"
    Just opSchema -> concat <$> sequence
        [operationDec name opSchema, operationInstance name opSchema]

operationDec :: Name -> Operation -> Generator [Dec]
operationDec name Operation {..} = do
    -- TODO: clean up name logic
    let objName  = nameBase name
        propName = camelCase . ((decapitalize objName) :) . unSnakeCase
    paramProps <- mapM (mkParamField propName) operationParameters
    props      <- case operationRequestBody of
        Nothing               -> return paramProps
        Just RequestBody {..} -> do
            ty <-
                tyRequired requestBodyRequired
                    <$> makeType (propName "payload") requestBodySchema
            let field = recordField (propName "payload") ty
            return $ field : paramProps
    return [DataD [] name [] Nothing [RecC name props] []]

mkParamField :: (String -> String) -> Parameter -> Generator VarBangType
mkParamField nameF param = do
    ty <- tyRequired (parameterRequired param) <$> paramTy param
    return $ recordField name ty
  where
    name    = nameF $ T.unpack $ parameterName param
    paramTy = maybe (pure $ ConT ''Text) getType . parameterSchema

operationInstance :: Name -> Operation -> Generator [Dec]
operationInstance name Operation {..} =
    let objName  = nameBase name
        propName = camelCase . ((decapitalize objName) :) . unSnakeCase
    in
    return [ InstanceD
          Nothing
          []
          (AppT (ConT ''ApiRequest) (ConT name))
          [ FunD
              'getApiRequestMethod
              [ Clause [WildP]
                       (NormalB (LitE (StringL (T.unpack . T.toUpper $ operationMethod))))
                       []
              ]
          , FunD
              'getApiRequestPath
              [ Clause [VarP (mkName "obj")]
                       (NormalB (mkPathExp propName (T.unpack operationPath) (mkName "obj")))
                       []
              ]
           , FunD
              'getApiRequestBody
              [ Clause [VarP (mkName "obj")]
                       (if isJust operationRequestBody
                       then NormalB (AppE (ConE 'Just) (AppE (VarE 'A.encode) (AppE (VarE (mkName $ propName "payload")) (VarE (mkName "obj")))))
                       else NormalB (ConE 'Nothing))
                       []
              ]  
          ]
    ]

mkPathExp :: (String -> String) -> String -> Name -> Exp
mkPathExp nameFn path argName = AppE (VarE 'HTTP.encodePathSegments) $ ListE (map segmentE pathSegments)
    where pathSegments = splitOn '/' path
          segmentE ('{':cs) = let name = mkName . nameFn . takeWhile (/= '}') $ cs in AppE (VarE name) (VarE argName)
          segmentE cs = LitE . StringL $ cs

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
