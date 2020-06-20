{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# Language TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Lib where

import Lib.Schema
import Language.Haskell.TH
import Unsafe.Coerce
import GHC.TypeLits
import qualified Data.HashMap.Strict as M
import GHC.Base (Any)
import Data.Proxy

data MyObjType;

$(makePropValue ''MyObjType
    [ ("name", ''String)
    , ("number", ''Integer)
    ])


-- newtype MyPropTypeMapper sym :: Proxy (MyPropType sym)

newtype PropMap ty = PropMap { _unPropMap :: M.HashMap String Any }
emptyPropMap = PropMap M.empty

data PropVal :: * -> Symbol -> * where 
    Value :: forall ty sym . KnownSymbol sym => PropertyType ty sym -> PropVal ty sym

insert :: forall ty sym . PropVal ty sym -> PropMap ty -> PropMap ty
insert (Value val) (PropMap propMap) = PropMap $ M.insert (symbolVal @sym Proxy) (unsafeCoerce val) propMap

get :: forall ty sym . KnownSymbol sym => Proxy sym -> PropMap ty -> Maybe (PropertyType ty sym)
get p (PropMap map) = fmap courseAny $ M.lookup (symbolVal p) map

courseAny :: forall t . Any -> t
courseAny = unsafeCoerce

runTest :: IO ()
runTest = do
    let m = insert (Value @MyObjType @"name" "bert") emptyPropMap
    let naam = get @MyObjType @"name" Proxy m
    putStrLn $ show $ naam