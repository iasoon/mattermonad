{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Lib where

import           Language.Haskell.TH
import           Unsafe.Coerce
import           GHC.TypeLits
import qualified Data.HashMap.Strict           as M
import           GHC.Base                       ( Any )
import           Data.Proxy
