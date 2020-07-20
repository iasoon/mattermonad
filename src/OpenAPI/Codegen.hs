{- TODO this module needs some major clean-up :^) -}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module OpenAPI.Codegen where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
                                         hiding ( lift )
import           OpenAPI.Schema
import qualified Data.Char                     as Char
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Types as A
import           Data.Aeson ((.=))
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Yaml                     as Yaml
import qualified Data.HashMap.Strict           as M
import           Data.Maybe
import           Control.Monad.Trans.State
import qualified Data.Aeson.TH                 as A
import           Lib.Utils
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Trans.Reader     ( ReaderT(..) )
import           OpenAPI.Lib
import qualified Data.ByteString as BS
import qualified Network.HTTP.Types as HTTP
import           Control.Monad (forM)


generateOperations :: FilePath -> [(Text, Text, String)] -> ObjConfig -> Q [Dec]
generateOperations path opSpecs generatorObjConfig = do
    generatorSpec <- runIO $ Yaml.decodeFileThrow path
    let conf = GeneratorConfig {..}
    runGen . fmap concat $
        forM opSpecs $ \(method, path, tgtName) -> concat <$>
            (genOperation generatorSpec (opKey method path) (mkName tgtName) >>= \decs -> (decs:) <$> finishQueue conf)

data GeneratorState = GeneratorState
    { generatorTypeMap :: M.HashMap Text Name
    , generatorQueue :: [QueueElem]
    } deriving (Show)

data QueueElem = QueueRef Text | QueueObj Name ObjectSchema deriving (Show)

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

data GeneratorConfig = GeneratorConfig
    { generatorSpec :: ApiSpec
    , generatorObjConfig :: ObjConfig
    }

finishQueue :: GeneratorConfig -> Generator [[Dec]]
finishQueue config@GeneratorConfig {..} = do
    res <- popQueue
    case res of
        Nothing   -> return []
        Just item -> do
            decs <- case item of
                QueueRef path ->
                    let comp = parseSchemaComponent $ T.unpack path
                    in  genComponent config comp
                QueueObj name schema -> objectDecl name generatorObjConfig schema
            (decs :) <$> finishQueue config


parseSchemaComponent :: String -> SchemaComponent
parseSchemaComponent str = case splitOn '/' str of
    ["#", "components", "schemas", name] -> SchemaComponent name
    _ -> error "unknown component"

genComponent :: GeneratorConfig -> SchemaComponent -> Generator [Dec]
genComponent GeneratorConfig {..} (SchemaComponent key) = do
    let
        val = M.lookup (T.pack key) $ componentsSchemas $ apiSpecComponents
            generatorSpec
    case val of
        Nothing        -> fail "component schema not found"
        Just objSchema -> objectDecl (mkName key) generatorObjConfig objSchema

data ObjConfig = ObjConfig
    { objConfigPropType :: String -> Property -> Generator Type
    , objConfigPropExp :: Property -> Exp
    }

singE :: Name -> Exp
singE tyName = SigE (VarE 'sing) (ConT tyName)

propFromJSON Property {..} obj = AppE (AppE (VarE op) obj) (LitE . StringL . T.unpack $ propertyName)
    where op = if propertyIsRequired then '(A..:) else '(A..:?)

propType name Property {..} = tyRequired propertyIsRequired <$> makeType name propertySchema

type ObjGen a = (ReaderT ObjConfig Generator) a

objectDecl :: Name -> ObjConfig -> ObjectSchema -> Generator [Dec]
objectDecl name config schema@ObjectSchema {..} =
    let objName  = nameBase name
        propName = camelCase . (decapitalize objName :) . unSnakeCase
    in  do
            props <- mapM (mkPropField config propName) $ M.elems objectProperties
            -- TODO: please please better structure
            concat <$> sequence
                [ return [DataD [] name [] Nothing [RecC name props] []]
                , objectToJSON name config schema
                , return $ objectFromJSON name config schema
                ]

mkPropField :: ObjConfig -> (String -> String) -> Property -> Generator VarBangType
mkPropField ObjConfig {..} nameF prop@Property {..} = do
    ty <- makeType name propertySchema
    let reprTy = AppT (AppT (ConT ''PropRepr) (ConT $ propEncoding prop)) ty
    return $ recordField name reprTy
    where name = nameF . T.unpack $ propertyName

propEncoding Property { .. } = if propertyIsRequired then ''RequiredProp else ''OptionalProp

-- TODO: toEncoding implementation
objectToJSON :: Name -> ObjConfig -> ObjectSchema -> Generator [Dec]
objectToJSON tyName config ObjectSchema {..} = do
    pairExps <- return [] -- mapM maybePropPair $ M.elems objectProperties
    return
        [ InstanceD Nothing [] (AppT (ConT ''A.ToJSON) (ConT tyName))
            [ FunD 'A.toJSON
                [ Clause [VarP objName] (NormalB (
                    eApp 'A.object $ eApp 'catMaybes $ ListE $ pairExps
                )) []
                ]
            ]
        ]
    where   varName = decapitalize $ nameBase tyName
            propName = camelCase . (varName :) . unSnakeCase
            -- TODO: Oh please, clean me up

            objName = mkName "obj"
            obj = VarE objName
            eJust = AppE (ConE 'Just)
            maybePropPair p@Property {..} = AppE encodePropFn propValue
                where propValue = AppE (VarE . mkName . propName . T.unpack $ propertyName) obj
                      name = propName . T.unpack $ propertyName
                      encodePropFn = AppE encodeFn (textLit propertyName)
                      encodeFn = AppE (VarE 'encodePropValue) (singE $ propEncoding p)


            -- maybePropPair p@Property {..} = AppE <$> (AppE (VarE 'encodePropValue) <$> (typedPropExp config name p)) <*> pure propValue
            -- maybePropPair Property {..} = if propertyIsRequired
            --     then eJust $ AppE makePairFn (propValue obj)
            --     else AppE (eApp 'fmap makePairFn) (propValue obj)
            --     where makePairFn = InfixE (Just $ textLit propertyName) (VarE '(.=)) Nothing
            --           

objectFromJSON :: Name -> ObjConfig -> ObjectSchema -> [Dec]
objectFromJSON tyName ObjConfig {..} ObjectSchema {..} =
    [ InstanceD Nothing [] (AppT (ConT ''A.FromJSON) (ConT tyName))
        [ FunD 'A.parseJSON [ Clause [] (NormalB
            (AppE (AppE (VarE 'A.withObject) (LitE (StringL (nameBase tyName))))
                    (LamE [VarP objName] args)
            ))
            []
        ]]
    ]
    where   objName = mkName "o"
            cons = AppE (VarE 'pure) (ConE tyName)
            args = foldl reduceFn cons $ M.elems objectProperties
            propParser prop = AppE (AppE (AppE (VarE 'retrievePropValue) (singE $ propEncoding prop) ) (textLit $ propertyName prop)) (VarE objName)
            reduceFn l r = UInfixE l (VarE '(<*>)) (propParser r)


textLit :: Text -> Exp
textLit = LitE . StringL . T.unpack

eApp :: Name -> Exp -> Exp
eApp name = AppE (VarE name)

genOperation :: ApiSpec -> OperationKey -> Name -> Generator [Dec]
genOperation spec opKey name = case findOp opKey (apiSpecOperations spec) of
    Nothing       -> fail "operation schema not found"
    Just opSchema -> concat <$> sequence
        [ operationDec name opSchema
        , operationInstance name opSchema
        , operationResponse name opSchema
        ]

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

operationResponse :: Name -> Operation -> Generator [Dec]
operationResponse opName Operation {..} =
    fmap concat . forM (M.toList operationResponses) $ \case
        (statusCode, Lit response) -> makeResponse opName statusCode response
        _ -> return [] -- TODO

httpStatusName :: Text -> String
httpStatusName "200" = "Ok"
httpStatusName "201" = "Created"
httpStatusName _ = error "unimplemented status code"
        
makeResponse :: Name -> Text -> Response -> Generator [Dec]
makeResponse opName statusCode Response {..} =
    case responseContentSchema of
        Nothing -> error "no schema given"
        Just schema -> do
            let respName = camelCase [nameBase opName, httpStatusName statusCode]
            respTy <- makeType respName schema
            requestResponseInstance (ConT opName) (T.unpack statusCode) respTy

requestResponseInstance :: Type -> String -> Type -> Generator [Dec]
requestResponseInstance opType statusCode respType = return
    [ TySynInstD $ TySynEqn Nothing 
        (AppT (AppT (ConT ''RequestResponse) opType)
              (LitT $ StrTyLit statusCode))
        respType
    ]

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
makeType _ (Ref path) = getRef path

getRef :: Text -> Generator Type
getRef path = lookupTyName path >>= \case
    Just tyName -> return $ ConT tyName
    Nothing -> do
        enqueue (QueueRef path)
        let SchemaComponent compName = parseSchemaComponent $ T.unpack path
        let tyName                   = mkName compName
        insertTyName path tyName
        return $ ConT tyName

lookupTyName :: Text -> Generator (Maybe Name)
lookupTyName path = Generator (M.lookup path <$> gets generatorTypeMap)

insertTyName :: Text -> Name -> Generator ()
insertTyName path name = Generator $ modify (\s -> s { generatorTypeMap = M.insert path name (generatorTypeMap s)})

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
