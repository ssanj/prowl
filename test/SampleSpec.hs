{-# LANGUAGE OverloadedStrings #-}

module SampleSpec where

import Test.Tasty.HUnit ((@?=), Assertion)

import qualified Hedgehog              as H
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range

import Prowl.Model
import Prowl.Format.Pretty (printPullRequest)
import qualified Data.Vector  as V
import qualified Data.Text.IO as T
import qualified Data.Text as T

hprop_equality :: H.Property
hprop_equality =
  H.property $ do
    randomString <- H.forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    randomString H.=== randomString

unit_equality :: Assertion
unit_equality =
  let actual   = 100 :: Int
      expected = 100 :: Int
  in actual @?= expected


unit_other :: Assertion
unit_other =
  let pr1 =
        PullRequest
        {
          _prowlPullRequestTitle = "Giving Data Emotions"
        , _prowlPullRequestState = PullRequestOpen
        , _prowlPullRequestIssueNumber = PullRequestIssueNumber 123
        , _prowlPullRequestIssueUser = PullRequestUser "J La Forge"
        , _prowlPullRequestDetail =
          PullRequestDetail
          {
            _prowlPullRequestDetailBranch = PullRequestBranch "emote"
          , _prowlPullRequestDetailRepo =
            GithubRepo {_prowlGithubRepo = "data"}
          , _prowlPullRequestDetailURL = mkUrlFor "https://federation.space/api/v3/repos/st-enterprise/data/pulls/123"
          }
        , _prowlPullRequestReviews =
          (V.cons
            PullRequestReview {_prowlPullRequestReviewState = PullRequestReviewStateApproved, _prowlPullRequestReviewUser = PullRequestReviewUser "J L Picard"}
            (V.cons
              PullRequestReview {_prowlPullRequestReviewState = PullRequestReviewStateApproved, _prowlPullRequestReviewUser = PullRequestReviewUser "B Crusher"}
              V.empty
            )
          )
        }
      output = printPullRequest pr1
  in do
    T.putStrLn $ T.intercalate "\n" output
    True @?= True
