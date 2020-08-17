{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric      #-}

module Prowl.Program.Model
       (
          -- Data types
          UserSelection(..)
       ) where

import GHC.Generics

import Data.Text (Text)
import Data.Aeson (ToJSON(..), genericToEncoding, defaultOptions)

data UserSelection =
  UserSelection {
     _userSelectionOrg                    :: Text
  ,  _userSelectionRepo                   :: Text
  ,  _userSelectionBranch                 :: Text
  ,  _userSelectionPRHash                 :: Text
  ,  _userSelectionPullRequestIssueNumber :: Int
  } deriving stock (Eq, Show, Generic)

instance ToJSON UserSelection where
    toEncoding = genericToEncoding defaultOptions

