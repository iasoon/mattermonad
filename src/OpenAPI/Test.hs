{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module OpenAPI.Test where

import           OpenAPI.Codegen

$(generateOperations "mattermost-openapi-v4.yaml"
    [ ("get", "/teams/name/{team_name}/channels/name/{channel_name}", "GetChannel")
    , ("post", "/posts", "CreatePost")
    ])
