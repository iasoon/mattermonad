{-# LANGUAGE TemplateHaskell #-}
module OpenAPI.MMConfig where

import qualified Data.Aeson                    as A
import qualified Data.Aeson.Types              as A
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified Data.Text                     as T

import           OpenAPI.Codegen
import           OpenAPI.Schema

mmPropFromJSON prop@Property {..} = parser
  where
    parser = case propertySchema of
        (Lit BoolTy) -> parsePropWith prop (VarE 'parseBool)
        _            -> parseProp prop

parseProp :: Property -> Exp -> Exp
parseProp Property {..} objE = UInfixE objE accessorE fieldNameE
  where
    accessorE  = VarE $ if propertyIsRequired then '(A..:) else '(A..:?)
    fieldNameE = LitE . StringL . T.unpack $ propertyName

parsePropWith :: Property -> Exp -> Exp -> Exp
parsePropWith prop@Property {..} valueParser =
    InfixE (Just parseVal) (VarE '(=<<)) . Just . parseProp prop
  where
    parseVal = appIf (not propertyIsRequired) (AppE (VarE 'ugh)) valueParser

-- does this function exist?
ugh :: (a -> A.Parser b) -> Maybe a -> A.Parser (Maybe b)
ugh f = \case
    Just val -> Just <$> f val
    Nothing  -> return Nothing

appIf :: Bool -> (a -> a) -> a -> a
appIf True  f = f
appIf False _ = id

parseBool :: A.Value -> A.Parser Bool
parseBool (A.Bool   val    ) = return val
parseBool (A.String "true" ) = return True
parseBool (A.String "false") = return False
parseBool val                = A.typeMismatch "boolean" val
