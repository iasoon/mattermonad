{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# Language TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Lib where

import Lib.Schema
import Language.Haskell.TH
import Unsafe.Coerce
import GHC.TypeLits
import qualified Data.HashMap.Strict as M
import GHC.Base (Any)
import Data.Proxy

import GHC.OverloadedLabels

import Prelude hiding (lookup)
import Control.Lens

data MyObjType;

$(makePropValue ''MyObjType
    [ ("name", ''String)
    , ("number", ''Integer)
    , ("test", ''String)
    ])

data PropName (sym :: Symbol) where
    PropName :: PropName sym

nameToProxy :: PropName sym -> Proxy sym
nameToProxy _ = Proxy

propName :: KnownSymbol sym => PropName sym -> String
propName = symbolVal . nameToProxy

instance (s ~ s') => IsLabel s (PropName s') where
    fromLabel = PropName
    

newtype PropMap ty = PropMap { _unPropMap :: M.HashMap String Any }
emptyPropMap = PropMap M.empty

data PropVal :: * -> Symbol -> * where 
    Value :: forall ty sym . KnownSymbol sym => PropertyType ty sym -> PropVal ty sym

insert :: forall ty sym . KnownSymbol sym => PropName sym -> PropertyType ty sym -> PropMap ty -> PropMap ty
insert name val (PropMap propMap) = PropMap $ M.insert (propName name) (unsafeCoerce val) propMap

lookup :: forall ty sym . KnownSymbol sym =>  PropName sym -> PropMap ty -> Maybe (PropertyType ty sym)
lookup name (PropMap map) = fmap courseAny $ M.lookup (propName name) map

delete :: forall ty sym . KnownSymbol sym => PropName sym -> PropMap ty -> PropMap ty
delete name (PropMap map) = PropMap $ M.delete (propName name) map

courseAny :: forall t . Any -> t
courseAny = unsafeCoerce

property :: forall ty sym . KnownSymbol sym => PropName sym -> Lens' (PropMap ty) (Maybe (PropertyType ty sym))
property name = lens 
    (lookup name)
    (\obj -> \case
        Just val -> insert name val obj
        Nothing -> delete name obj)

runTest :: IO ()
runTest = do
    let m :: PropMap MyObjType = insert #name "bert" emptyPropMap
    let a = m ^. property #name
    putStrLn $ show $ a

$(genDataTypes "schema.yaml")
