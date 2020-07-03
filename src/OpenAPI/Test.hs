{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module OpenAPI.Test where

import           OpenAPI.Schema
import           OpenAPI.Codegen
import           Lib.Utils
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

$(runGen . fmap concat $ do
    spec <- liftQ $ runIO apiSpec
    genOperation spec (opKey "get" "/teams/name/{team_name}/channels/name/{channel_name}") (mkName "GetChannel") >>= \a -> fmap (a :) (finishQueue spec)
    )
