module Prowl.Config.Model
       (
          -- Data types
          GithubAuth(..)
       ,  GithubApi(..)
       ,  GithubToken(..)
       ) where

import Data.Text (Text)
import Data.ByteString (ByteString)

newtype GithubApi = GithubApi Text

newtype GithubToken = GithubToken ByteString

data GithubAuth = GithubAuth GithubApi GithubToken
