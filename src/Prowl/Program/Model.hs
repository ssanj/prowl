{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric      #-}

module Prowl.Program.Model
       (
          -- Data types
          UserSelection(..)
       ,  CheckoutDir
       ,  CheckoutFile
       ,  ProwlCheckoutDir
       ) where

import GHC.Generics

import Data.Text (Text)
import Data.Aeson (ToJSON(..), genericToEncoding, defaultOptions)

import Prowl.Common.Model (TaggedText)

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

data CheckoutDir
data CheckoutFile

type ProwlCheckoutDir = TaggedText CheckoutDir
