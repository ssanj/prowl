{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Prowl.Config.Model
       (
          -- Data types
          GithubAuth(..)
       ,  GithubApi
       ,  GithubDomain
       ,  GithubToken(..)
       ,  ProwlConfig(..)
       ,  ProwlOrganisationName(..)
       ,  ProwlSearchByDateType(..)
       ,  SearchType(..)
       ,  ProwlDate(..)
       ,  ProwlWorkDir(..)

          -- Function types
       ,  defaultWorkDir
       ,  getGithubApi
       ) where

import Prowl.Common.Model

import Data.Text (Text)
import Data.ByteString (ByteString)

data GithubApiUrl
data GithubApiDomain

type GithubApi = TaggedText GithubApiUrl

type GithubDomain = TaggedText GithubApiDomain

newtype GithubToken = GithubToken ByteString

data GithubAuth = GithubAuth GithubApi GithubToken

data ProwlConfig =
  ProwlConfig {
    _prowlConfigRepositoryName :: ProwlOrganisationName
  , _prowlConfigSearchType :: SearchType
  , _prowlConfigWorkingDirectory :: ProwlWorkDir
  } deriving stock (Eq, Show)

newtype ProwlOrganisationName = ProwlOrganisationName Text deriving stock (Eq, Show)

newtype ProwlDate =
  ProwlDate {
    _prowlDateValue :: Text
  } deriving stock (Show, Eq)

data ProwlSearchByDateType = ProwlSearchByCreatedDate Text | ProwlSearchByUpdatedDate Text deriving stock (Eq, Show)

data SearchType = SearchByCreatedDate ProwlDate
                | SearchByUpdatedDate ProwlDate
                | SearchByDateTypeNotSupplied deriving stock (Eq, Show)

newtype ProwlWorkDir = ProwlWorkDir { _prowlWorkDirLocation :: Text } deriving stock (Eq, Show)

defaultWorkDir :: ProwlWorkDir
defaultWorkDir = ProwlWorkDir "~/.prowl-work"

getGithubApi :: GithubDomain -> GithubApi
getGithubApi =  mkTextTag . (\d -> "https://" <>  d <> "/v3/api") . unmkTextTag