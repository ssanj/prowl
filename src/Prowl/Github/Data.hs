{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE FlexibleContexts #-}

module Prowl.Github.Data
       (
          -- Functions
          getIssueRepoFromURL
       ) where

import Text.Regex.PCRE.Heavy

import Safe (headMay)
import Prowl.Model (GithubRepo(..))

import GitHub as G

getIssueRepoFromURL :: G.URL -> Maybe GithubRepo
getIssueRepoFromURL url = do
  matches <- headMay $ scan [re|^https.*/api/v3/repos/.+/(.+)/issues/.+$|] $ G.getUrl url
  repo    <- headMay . snd $ matches
  Just $ GithubRepo repo
