{-# LANGUAGE OverloadedStrings #-}

module Prowl.Format.Pretty
       (
          -- Functions
          printPullRequest
       ) where


import Prowl.Model
import Data.Text (Text, pack)

import qualified Data.Foldable as F

printPullRequest :: PullRequest -> [Text]
printPullRequest pr =
  [
    printTitle
  , printIssueNumber
  , printRepo
  , printBranch
  , printAuthor
  , printReviews
  , printURL
  ] <*> [pr]

printTitle :: PullRequest -> Text
printTitle = _prowlPullRequestTitle

printIssueNumber :: PullRequest -> Text
printIssueNumber pr = ("PR Number: " <>) . pack . show . _prowlPullRequestIssueNumberValue . _prowlPullRequestIssueNumber $ pr

printRepo :: PullRequest -> Text
printRepo pr = ("Repo: " <>) . _prowlGithubRepo . _prowlPullRequestDetailRepo . _prowlPullRequestDetail $ pr

printBranch :: PullRequest -> Text
printBranch pr = ("Branch: " <>) . _prowlPullRequestBranchValue . _prowlPullRequestDetailBranch . _prowlPullRequestDetail $ pr

printAuthor :: PullRequest -> Text
printAuthor pr = ("Author: " <>) . _prowlPullRequestUserValue . _prowlPullRequestIssueUser $ pr

printURL :: PullRequest -> Text
printURL pr = ("Link: " <>) . untagUrlFor . _prowlPullRequestDetailURL . _prowlPullRequestDetail $ pr

printReviews :: PullRequest -> Text
printReviews pr =
  let reviews = (_prowlPullRequestReviewState <$>) . _prowlPullRequestReviews $ pr
      (approved, changesRequested, commented, dismissed) =
        foldl countStatus (0,0,0,0) reviews
          where countStatus :: (Int, Int, Int, Int) -> PullRequestReviewState -> (Int, Int, Int, Int)
                countStatus acc PullRequestReviewStatePending                       = acc
                countStatus (ap, cr, cm, dm) PullRequestReviewStateApproved         = (ap + 1, cr, cm, dm)
                countStatus (ap, cr, cm, dm) PullRequestReviewStateChangesRequested = (ap, cr + 1, cm, dm)
                countStatus (ap, cr, cm, dm) PullRequestReviewStateCommented        = (ap, cr, cm + 1, dm)
                countStatus (ap, cr, cm, dm) PullRequestReviewStateDismissed        = (ap, cr, cm, dm + 1)
  in pack $ F.fold [
       "Review: "
     , "approved("
     , (show approved)
     , ") changes("
     , (show changesRequested)
     , ") comments("
     , (show commented)
     , ") dismissed("
     , (show dismissed)
     , ")"
     ]