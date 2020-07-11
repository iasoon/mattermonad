{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module OpenAPI.Test where

import           OpenAPI.Codegen
import           OpenAPI.Schema
import           OpenAPI.MMConfig

$(generateOperations "mattermost-openapi-v4.yaml"
    [ ("get", "/teams/name/{team_name}/channels/name/{channel_name}", "GetChannel")
    , ("get", "/users/{user_id}", "GetUser")
    , ("post", "/posts", "CreatePost")
    ]
    ObjConfig
        { objConfigPropFromJSON = mmPropFromJSON
        }
    )
