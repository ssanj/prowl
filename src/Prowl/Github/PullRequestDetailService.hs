{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prowl.Github.PullRequestDetailService (getDetailsForPR) where

import Prowl.Github.Model
import Prowl.Config.Model
import Prowl.Github.ServiceSupport

import qualified GitHub as G

createPullRequestDetail :: GithubApi -> GithubOrg -> G.PullRequest -> GithubRepo -> PullRequestDetail
createPullRequestDetail api org pr repo =
  let branch                    = PullRequestBranch . G.pullRequestCommitRef . G.pullRequestHead $ pr
      sha :: SHAFor GithubPR    = mkSHAFor . G.pullRequestCommitSha . G.pullRequestHead $ pr
      prUrl :: UrlFor GithubPR  = mkUrlFor . G.getUrl . G.pullRequestHtmlUrl $ pr
  in PullRequestDetail api org repo branch prUrl sha

getDetailsForPR :: GithubAuth -> GithubOrg -> GithubRepo -> PullRequestIssueNumber -> IO PullRequestDetail
getDetailsForPR auth ghOrg@(GithubOrg org) ghRepo (PullRequestIssueNumber issueNumber) = do
  let ownerName          = G.mkOwnerName org
      repoName           = G.mkRepoName . _prowlGithubRepo $ ghRepo
      (GithubAuth api _) =  auth
  matchesE <- G.executeRequest (toEntAuth auth) (G.pullRequestR ownerName repoName (G.IssueNumber issueNumber))
  matches  <- processResult matchesE
  pure $ createPullRequestDetail api ghOrg matches ghRepo
