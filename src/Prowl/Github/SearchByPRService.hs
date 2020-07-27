{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prowl.Github.SearchByPRService (searchByPR) where

import Prowl.Github.Model
import Prowl.Config.Model
import Prowl.Github.ServiceSupport

import Prowl.Github.Data                     (getIssueRepoFromURL)
import Prowl.Github.PullRequestDetailService (getDetailsForPR)
import Prowl.Github.PullRequestReviewService (getReviewsForPR)

import Data.Vector      (Vector)

import qualified GitHub as G

--TODO: Parameterise this
searchByPR :: GithubAuth -> GithubOrg -> IO (Vector PullRequest)
searchByPR auth (GithubOrg org) = do
  let query = "org:" <> org <> " is:open is:pr created:>=2020-06-01"
  matchesE <- G.executeRequest (toEntAuth auth) (G.searchIssuesR query)
  matches <- processResult matchesE
  processMatches auth (GithubOrg org) matches

processMatches :: GithubAuth -> GithubOrg -> G.SearchResult G.Issue -> IO (Vector PullRequest)
processMatches auth org matches = traverse (issueToPR auth org)  (G.searchResultResults matches)

-- TODO: Separate out
issueToPR :: GithubAuth -> GithubOrg -> G.Issue -> IO PullRequest
issueToPR auth org issue = do
  let issueNumber = createPullRequestIssueNumber issue
      issueUrl    = getIssueUrl issue
  repo      <- getRepoUrlMaybe issueUrl
  prDetail  <- getDetailsForPR auth org repo issueNumber
  prReviews <- getReviewsForPR auth org repo issueNumber
  pure $ createPullRequest issue issueNumber prDetail prReviews

getIssueUrl :: G.Issue -> G.URL
getIssueUrl = G.issueUrl

createPullRequestIssueNumber :: G.Issue  -> PullRequestIssueNumber
createPullRequestIssueNumber = PullRequestIssueNumber . G.unIssueNumber . G.issueNumber

getRepoUrlMaybe :: G.URL -> IO GithubRepo
getRepoUrlMaybe url = do
  let maybRepo = getIssueRepoFromURL url
  processResult $ maybe (Left $ "Could not find repo in " <> show url) Right maybRepo

createPullRequest :: G.Issue -> PullRequestIssueNumber -> PullRequestDetail -> Vector PullRequestReview -> PullRequest
createPullRequest issue issueNumber =
    PullRequest
      (G.issueTitle issue)
      (getState . G.issueState $ issue)
      issueNumber
      (getIssueUser . G.issueUser $ issue)

getIssueUser :: G.SimpleUser -> PullRequestUser
getIssueUser = PullRequestUser . G.untagName . G.simpleUserLogin

getState :: G.IssueState -> PullRequestState
getState G.StateOpen   = PullRequestOpen
getState G.StateClosed = PullRequestClosed
