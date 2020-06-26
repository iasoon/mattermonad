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
import OpenAPI.Schema


-- $(genDataTypes "schema.yaml")
