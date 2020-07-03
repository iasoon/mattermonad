{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module OpenAPI.Test where

import           OpenAPI.Codegen

$(generateOperations "mattermost-openapi-v4.yaml"
    [ ("get", "/teams/name/{team_name}/channels/name/{channel_name}", "GetChannel")
    , ("post", "/posts", "CreatePost")
    ])
