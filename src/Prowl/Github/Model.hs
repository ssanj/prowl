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
       ,  GithubPRUrl
       ,  GithubPRSHA
       ,  TaggedText
       ,  GithubSearchDate(..)

          -- Functions
       ,  mkTextTag
       ,  unmkTextTag
       ) where

import Data.Tagged (Tagged(..), untag)

import Control.Exception (Exception)
import Data.Text (Text)
import Data.Vector (Vector)
import Prowl.Config.Model (ProwlDate, GithubApi)

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

type TaggedText a = Tagged a Text

data GithubPRUrl
data GithubPRSHA

mkTextTag :: Text -> TaggedText a
mkTextTag = Tagged

unmkTextTag :: TaggedText a -> Text
unmkTextTag = untag

data PullRequestDetail =
  PullRequestDetail {
    _prowlPullRequestDetailApi    :: GithubApi
  , _prowlPullRequestDetailOrg    :: GithubOrg
  , _prowlPullRequestDetailRepo   :: GithubRepo
  , _prowlPullRequestDetailBranch :: PullRequestBranch
  , _prowlPullRequestDetailURL    :: TaggedText GithubPRUrl
  , _prowlPullRequestDetailSHA    :: TaggedText GithubPRSHA
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

data GithubSearchDate =
  CreationDate {
    _prowlGithubSearchDateCreationDate :: ProwlDate
  } |
  UpdationDate {
    _prowlGithubSearchDateCreationDate :: ProwlDate
  } deriving stock (Show, Eq)

instance Exception ProwlException