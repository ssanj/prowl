{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prowl.Program.ProwlSearch (main) where

import Prowl.GithubApi
import Prowl.Model
import Prowl.Program.Menu

import Data.Tuple          (swap)
import Control.Exception   (SomeException, catch, displayException)
import Prowl.Format.Pretty (printPullRequest)
import Prowl.Program.Model (UserSelection(..))

import qualified Data.Vector  as V
import qualified Data.Text.IO as T
import qualified Data.Text    as T
import qualified Data.Text.Lazy as LT
import qualified Data.Aeson.Text  as A

main :: GithubAuth -> GithubOrg -> GithubSearchDate -> IO ()
main = performSearchByPR

performSearchByPR :: GithubAuth -> GithubOrg -> GithubSearchDate -> IO ()
performSearchByPR auth org searchDate = do
  matches <- searchByPR auth org searchDate
  -- TODO: Do we want to differentiate between having to choose an item and not?
  if V.null matches then displayNoResults
  else displayMenu org matches `catch` generalErrorHandler

displayNoResults :: IO ()
displayNoResults = T.putStrLn "No matches found"

ripCord :: T.Text
ripCord = "exit"

displayMenu :: GithubOrg -> V.Vector PullRequest -> IO ()
displayMenu org prs =
  createMenu
    ripCord
    prompt
    (oneBasedIndex prs)
    printPRs
    (handlePRSelection org)

oneBasedIndex :: V.Vector a -> V.Vector (Int, a)
oneBasedIndex values = swap . fmap (+1). swap <$> V.indexed values

handlePRSelection :: GithubOrg -> (Int, PullRequest) -> IO ()
handlePRSelection org = T.putStrLn . LT.toStrict . A.encodeToLazyText . pullRequestToResponse org . snd

pullRequestToResponse :: GithubOrg -> PullRequest -> UserSelection
pullRequestToResponse (GithubOrg org) pr =
  let repo          = _prowlGithubRepo . _prowlPullRequestDetailRepo . _prowlPullRequestDetail $ pr
      branch        = _prowlPullRequestBranchValue . _prowlPullRequestDetailBranch . _prowlPullRequestDetail $ pr
      hash          = untagSHAFor . _prowlPullRequestDetailSHA . _prowlPullRequestDetail $ pr
      prIssueNumber = _prowlPullRequestIssueNumberValue . _prowlPullRequestIssueNumber $ pr
  in UserSelection {
       _userSelectionOrg                    = org
     , _userSelectionRepo                   = repo
     , _userSelectionBranch                 = branch
     , _userSelectionPRHash                 = hash
     , _userSelectionPullRequestIssueNumber = prIssueNumber
     }

generalErrorHandler :: SomeException -> IO ()
generalErrorHandler ex = T.putStrLn $ "Prowl failed: " <> (T.pack . displayException $ ex)

prompt :: [T.Text]
prompt = ["\n", "Please select a PR [number]: or '" <> ripCord <> "' to quit"]

printPRs :: V.Vector (Int, PullRequest) -> T.Text
printPRs prs =
  let header  = "\nProwl\n=====\n\n" :: T.Text
      prBlocks :: V.Vector T.Text = (\ipr -> T.intercalate "\n" . prWithIndex (fst ipr) $ printPullRequest (snd ipr)) <$> prs
  in  header <> (T.intercalate "\n\n----------\n\n" . V.toList $ prBlocks)

prWithIndex :: Int -> [T.Text] -> [T.Text]
prWithIndex index (firstLine:others) = (firstLine <> " [" <> T.pack (show index) <> "]") : others
prWithIndex _ others                 = others
