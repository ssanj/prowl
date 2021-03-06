{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prowl.Github.PullRequestDetailService (getDetailsForPR) where

import Prowl.Github.Model
import Prowl.Config.Model
import Prowl.Common.Model
import Prowl.Github.ServiceSupport

import Prowl.Github.Data (getOrgAndRepoFromURL)

import qualified GitHub as G

createPullRequestDetail :: GithubApi -> GithubOrg -> G.PullRequest -> GithubRepo -> PullRequestDetail
createPullRequestDetail api org pr repo =
  let branch                                             = PullRequestBranch . G.pullRequestCommitRef . G.pullRequestHead $ pr
      sha :: TaggedText GithubPRSHA                      = mkTextTag . G.pullRequestCommitSha . G.pullRequestHead $ pr
      prUrl :: TaggedText GithubPRUrl                    = mkTextTag . G.getUrl . G.pullRequestHtmlUrl $ pr
      repoUrlMaybe :: Maybe G.URL                        = (G.repoUrl <$>) . G.pullRequestCommitRepo . G.pullRequestHead $ pr
      prOrgAndRepoMaybe :: Maybe (GithubOrg, GithubRepo) = repoUrlMaybe >>= getOrgAndRepoFromURL
      (prOrg, prRepo) = maybe ((org, repo)) id prOrgAndRepoMaybe
  in PullRequestDetail api prOrg prRepo branch prUrl sha

getDetailsForPR :: GithubAuth -> GithubOrg -> GithubRepo -> PullRequestIssueNumber -> IO PullRequestDetail
getDetailsForPR auth ghOrg@(GithubOrg org) ghRepo (PullRequestIssueNumber issueNumber) = do
  let ownerName          = G.mkOwnerName org
      repoName           = G.mkRepoName . _prowlGithubRepo $ ghRepo
      (GithubAuth api _) =  auth
  matchesE <- G.executeRequest (toEntAuth auth) (G.pullRequestR ownerName repoName (G.IssueNumber issueNumber))
  matches  <- processResult matchesE
  pure $ createPullRequestDetail api ghOrg matches ghRepo
