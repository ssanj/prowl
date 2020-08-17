module Prowl.Handler.DisplaySelection
       (
          -- Functions
          handleSelection
       ) where

import qualified Prowl.Github.Model  as P
import qualified Prowl.Program.Model as P

import qualified Data.Aeson.Text  as A
import qualified Data.Text.IO     as T
import qualified Data.Text.Lazy   as LT

handleSelection :: P.PullRequest -> IO ()
handleSelection = T.putStrLn . LT.toStrict . A.encodeToLazyText . pullRequestToResponse

pullRequestToResponse :: P.PullRequest -> P.UserSelection
pullRequestToResponse pr =
  let (P.GithubOrg org) = P._prowlPullRequestDetailOrg . P._prowlPullRequestDetail $ pr
      repo            = P._prowlGithubRepo . P._prowlPullRequestDetailRepo . P._prowlPullRequestDetail $ pr
      branch          = P._prowlPullRequestBranchValue . P._prowlPullRequestDetailBranch . P._prowlPullRequestDetail $ pr
      hash            = P.unmkTextTag . P._prowlPullRequestDetailSHA . P._prowlPullRequestDetail $ pr
      prIssueNumber   = P._prowlPullRequestIssueNumberValue . P._prowlPullRequestIssueNumber $ pr
  in P.UserSelection {
       P._userSelectionOrg                    = org
     , P._userSelectionRepo                   = repo
     , P._userSelectionBranch                 = branch
     , P._userSelectionPRHash                 = hash
     , P._userSelectionPullRequestIssueNumber = prIssueNumber
     }

