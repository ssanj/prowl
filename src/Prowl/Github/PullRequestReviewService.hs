{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prowl.Github.PullRequestReviewService (getReviewsForPR) where

import Prowl.Github.Model
import Prowl.Config.Model
import Prowl.Github.ServiceSupport

import Data.Vector (Vector)

import qualified GitHub as G
import qualified Data.Text.IO as T
import qualified Data.Text as T

createPullRequestReview :: G.Review -> PullRequestReview
createPullRequestReview review =
  let status = fromReviewState . G.reviewState $ review
      user   = PullRequestReviewUser . G.untagName. G.simpleUserLogin . G.reviewUser $ review
  in PullRequestReview status user

fromReviewState :: G.ReviewState -> PullRequestReviewState
fromReviewState G.ReviewStatePending          = PullRequestReviewStatePending
fromReviewState G.ReviewStateApproved         = PullRequestReviewStateApproved
fromReviewState G.ReviewStateDismissed        = PullRequestReviewStateDismissed
fromReviewState G.ReviewStateCommented        = PullRequestReviewStateCommented
fromReviewState G.ReviewStateChangesRequested = PullRequestReviewStateChangesRequested

getReviewsForPR :: GithubAuth -> GithubOrg -> GithubRepo -> PullRequestIssueNumber -> IO (Vector PullRequestReview)
getReviewsForPR auth (GithubOrg org) ghRepo (PullRequestIssueNumber prIssueNumber) = do
  let ownerName   = G.mkOwnerName org
      repoName    = G.mkRepoName . _prowlGithubRepo $ ghRepo
      issueNumber = G.IssueNumber prIssueNumber
  reviewsE <- G.executeRequest (toEntAuth auth) (G.pullRequestReviewsR ownerName repoName issueNumber G.FetchAll)
  T.putStrLn . T.pack . show $ reviewsE
  reviews <- processResult reviewsE
  pure $ createPullRequestReview <$> reviews