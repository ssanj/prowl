{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE FlexibleContexts #-}

module Prowl.Github.Data
       (
          -- Functions
          getIssueRepoFromURL
       ,  getOrgAndRepoFromURL
       ) where

import Text.Regex.PCRE.Heavy

import Safe (headMay)
import Prowl.Model (GithubRepo(..), GithubOrg(..))

import GitHub as G

getIssueRepoFromURL :: G.URL -> Maybe GithubRepo
getIssueRepoFromURL url = do
  matches <- headMay $ scan [re|^https.*/api/v3/repos/.+/(.+)/issues/.+$|] $ G.getUrl url
  repo    <- headMay . snd $ matches
  Just $ GithubRepo repo

getOrgAndRepoFromURL :: G.URL -> Maybe (GithubOrg, GithubRepo)
getOrgAndRepoFromURL url = do
  matches <- headMay $ scan [re|^https.*/api/v3/repos/(.+)/(.+)$|] $ G.getUrl url
  case (snd matches) of
    (org:repo:[]) -> Just $ (GithubOrg org, GithubRepo repo)
    _ -> Nothing
