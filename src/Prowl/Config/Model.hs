{-# LANGUAGE DerivingStrategies #-}

module Prowl.Config.Model
       (
          -- Data types
          GithubAuth(..)
       ,  GithubApi(..)
       ,  GithubToken(..)
       ,  ProwlConfig(..)
       ,  ProwlRepositoryName(..)
       ,  ProwlSearchByDateType(..)
       ,  SearchType(..)
       ,  ProwlDate(..)
       ) where

import Data.Text (Text)
import Data.ByteString (ByteString)

newtype GithubApi = GithubApi Text

newtype GithubToken = GithubToken ByteString

data GithubAuth = GithubAuth GithubApi GithubToken

data ProwlConfig =
  ProwlConfig {
    _prowlConfigRepositoryName :: ProwlRepositoryName
  , _prowlConfigSearchType :: SearchType
  } deriving stock (Eq, Show)

newtype ProwlRepositoryName = ProwlRepositoryName Text deriving stock (Eq, Show)

newtype ProwlDate =
  ProwlDate {
    _prowlDateValue :: Text
  } deriving stock (Show, Eq)

data ProwlSearchByDateType = ProwlSearchByCreatedDate Text | ProwlSearchByUpdatedDate Text deriving stock (Eq, Show)

data SearchType = SearchByCreatedDate ProwlDate
                | SearchByUpdatedDate ProwlDate
                | SearchByDateTypeNotSupplied deriving stock (Eq, Show)

--newtype ProwlSearchDate = ProwlSearchDate (Chronos Date)