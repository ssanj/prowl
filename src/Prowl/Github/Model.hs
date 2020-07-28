{-# LANGUAGE DerivingStrategies #-}

module Prowl.Github.Model
       (
          -- Data types
          PullRequest(..)
       ,  GitHubApiError
       ,  PullRequestState(..)
       ,  PullRequestIssueNumber(..)
       ,  PullRequestUser(..)
       ,  PullRequestReviewUser(..)
       ,  PullRequestBranch(..)
       ,  PullRequestDetail(..)
       ,  PullRequestReview(..)
       ,  PullRequestReviewState(..)
       ,  ProwlException(..)
       ,  GithubOrg(..)
       ,  GithubRepo(..)
       ,  GithubPR
       ,  UrlFor
       ,  ProwlCreationDate(..)

          -- Functions
       ,  mkUrlFor
       ,  untagUrlFor
       ) where

import Control.Exception (Exception)
import Data.Text (Text)
import Data.Vector (Vector)

newtype GithubOrg = GithubOrg Text deriving stock (Show, Eq)

newtype GithubRepo = GithubRepo { _prowlGithubRepo :: Text } deriving stock (Show, Eq)

data PullRequest =
  PullRequest {
    _prowlPullRequestTitle        :: Text
  , _prowlPullRequestState        :: PullRequestState
  , _prowlPullRequestIssueNumber  :: PullRequestIssueNumber
  , _prowlPullRequestIssueUser    :: PullRequestUser
  , _prowlPullRequestDetail       :: PullRequestDetail
  , _prowlPullRequestReviews      :: Vector PullRequestReview
  } deriving stock (Show, Eq)

newtype PullRequestIssueNumber =
  PullRequestIssueNumber {
    _prowlPullRequestIssueNumberValue :: Int
  } deriving stock (Show, Eq)

newtype PullRequestUser =
  PullRequestUser {
    _prowlPullRequestUserValue :: Text
  } deriving stock (Show, Eq)

newtype PullRequestReviewUser = PullRequestReviewUser Text deriving stock (Show, Eq)

newtype UrlFor a = GithubURL Text deriving stock (Show, Eq)

data GithubPR

mkUrlFor :: Text -> UrlFor a
mkUrlFor = GithubURL

untagUrlFor :: UrlFor a -> Text
untagUrlFor (GithubURL value) = value

data PullRequestDetail =
  PullRequestDetail {
    _prowlPullRequestDetailBranch :: PullRequestBranch
  , _prowlPullRequestDetailRepo   :: GithubRepo
  , _prowlPullRequestDetailURL    :: UrlFor GithubPR
  }  deriving stock (Show, Eq)

data PullRequestReviewState
    = PullRequestReviewStatePending
    | PullRequestReviewStateApproved
    | PullRequestReviewStateDismissed
    | PullRequestReviewStateCommented
    | PullRequestReviewStateChangesRequested deriving stock (Show, Eq)

data PullRequestReview =
  PullRequestReview {
    _prowlPullRequestReviewState :: PullRequestReviewState
  , _prowlPullRequestReviewUser :: PullRequestReviewUser
  } deriving stock (Show, Eq)

newtype PullRequestBranch =
  PullRequestBranch {
    _prowlPullRequestBranchValue :: Text
  } deriving stock (Show, Eq)

type GitHubApiError = Either Text

data PullRequestState = PullRequestOpen | PullRequestClosed deriving stock (Show, Eq)

data ProwlException = ProwlException Text deriving stock Show

newtype ProwlCreationDate =
  ProwlCreationDate {
    _prowlCreationDateValue :: Text
  } deriving stock (Show, Eq)

instance Exception ProwlException