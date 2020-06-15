{-# LANGUAGE TemplateHaskell #-}

module Lib.Types where

import Data.Text (Text)
import Data.Aeson.TH (deriveJSON)
import Lib.Utils

data User = User
    { userId :: Text
    , userUsername :: Text
    , userEmail :: Text
    , userNickname :: Text
    , userFirstName :: Text
    , userLastName :: Text
    } deriving (Show)
$(deriveJSON (removePrefix "user") ''User)


data ChannelData = ChannelData
    { chanDataId :: Text
    , chanDataName :: Text
    , chanDataDisplayName :: Text
    , chanDataHeader :: Text
    , chanDataPurpose :: Text
    , chanDataTeamId :: Text
    } deriving (Show)
$(deriveJSON (removePrefix "chanData") ''ChannelData)

