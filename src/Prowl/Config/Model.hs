{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Prowl.Config.Model
       (
          -- Data types
          GithubAuth(..)
       ,  GithubApi
       ,  GithubDomain
       ,  GithubHttpsCloneUrl
       ,  GithubToken(..)
       ,  ProwlConfig(..)
       ,  ProwlOrganisationName(..)
       ,  ProwlSearchByDateType(..)
       ,  SearchType(..)
       ,  ProwlDate(..)
       ,  ProwlWorkDir(..)
       ,  ProwlConfigDir
       ,  Language(..)

          -- Function types
       ,  defaultWorkDir
       ,  getGithubApi
       ,  getGithubCloneUrl
       ,  configDir
       ,  scriptName
       ) where

import Prowl.Common.Model

import Data.Text (Text)
import Data.ByteString (ByteString)

data GithubApiUrl
data GithubCloneUrl
data GithubApiDomain

type GithubApi = TaggedText GithubApiUrl

type GithubDomain = TaggedText GithubApiDomain

type GithubHttpsCloneUrl = TaggedText GithubCloneUrl

newtype GithubToken = GithubToken ByteString

data GithubAuth = GithubAuth GithubApi GithubToken

data ProwlConfig =
  ProwlConfig {
    _prowlConfigRepositoryName :: ProwlOrganisationName
  , _prowlConfigSearchType :: SearchType
  , _prowlConfigWorkingDirectory :: ProwlWorkDir
  , _prowlConfigGithubDomain :: GithubDomain
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

data ProwlConfigLocation

type ProwlConfigDir = TaggedText ProwlConfigLocation

configDir :: ProwlWorkDir -> ProwlConfigDir
configDir = mkTextTag . (\wd -> wd <> "/config"). _prowlWorkDirLocation

defaultWorkDir :: ProwlWorkDir
defaultWorkDir = ProwlWorkDir "~/.prowl-work"

getGithubApi :: GithubDomain -> GithubApi
getGithubApi =  mkTextTag . (\d -> "https://" <>  d <> "/api/v3") . unmkTextTag

getGithubCloneUrl :: GithubDomain -> GithubHttpsCloneUrl
getGithubCloneUrl =  mkTextTag . (\d -> "https://" <> d) . unmkTextTag

scriptName :: Text
scriptName = "script.sh"

data Language = Scala
              | Ruby
              | Haskell deriving stock (Eq, Show, Enum, Bounded)
