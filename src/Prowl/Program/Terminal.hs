{-# LANGUAGE OverloadedStrings #-}

module Prowl.Program.Terminal
       (
         -- Functions
         gitClone
       , mkDir
       , runCommand
       , runCommandF
       ) where

import System.Exit         (ExitCode(..))
import System.Directory    (createDirectoryIfMissing)
import Control.Monad       (void)

import qualified Prowl.Config.Model   as C
import qualified Prowl.Common.Model   as C
import qualified Prowl.Github.Model   as C
import qualified Prowl.Program.Model  as C
import qualified System.Process       as P
import qualified Data.Text            as T
import qualified Data.Text.IO         as T

newtype Command = Command T.Text
newtype Args = Args [T.Text]

gitClone :: C.ProwlWorkDir -> C.GithubDomain -> (C.TaggedText C.CheckoutDir -> IO ()) -> C.PullRequest -> IO ()
gitClone workingDir domain handler pr = do
  let wkdir     = C._prowlWorkDirLocation $ workingDir
      hash      = C.unmkTextTag . C._prowlPullRequestDetailSHA . C._prowlPullRequestDetail $ pr
      (C.GithubOrg org) = C._prowlPullRequestDetailOrg . C._prowlPullRequestDetail $ pr
      repo     = C._prowlGithubRepo . C._prowlPullRequestDetailRepo . C._prowlPullRequestDetail $ pr
      branch   = C._prowlPullRequestBranchValue . C._prowlPullRequestDetailBranch . C._prowlPullRequestDetail $ pr
      ghcUrl    = C.unmkTextTag . C.getGithubCloneUrl $ domain -- C.unmkTextTag .  C._prowlPullRequestDetailApi . C._prowlPullRequestDetail $ pr

      clonePath = joinDirectories "/" [ghcUrl, org, repo]
      cloneUrl  = clonePath <> ".git"
      cloneDir  = joinDirectories "/" [wkdir, "clones", org, repo, branch, hash]
  T.putStrLn $ "git cloning from " <> clonePath <> ":" <> branch <> " -> " <> cloneDir
  void $ runCommandF (Command "git") (Args ["clone", cloneUrl, "-b", branch, cloneDir])
  handler (C.mkTextTag cloneDir)
  -- TODO: Run a script based on a heuristic
  -- Accept a function to run after cloning, with the clone directory path as input

joinDirectories :: T.Text -> [T.Text] -> T.Text
joinDirectories = T.intercalate

mkDir :: C.ProwlWorkDir -> IO ()
mkDir = createDirectoryIfMissing True .  T.unpack . C._prowlWorkDirLocation

runCommandF :: Command -> Args -> IO T.Text
runCommandF (Command command) (Args args) = T.pack <$> (P.readProcess (T.unpack command) (fmap T.unpack args) "")

runCommand :: Command -> Args -> IO (Maybe [T.Text])
runCommand (Command command) (Args args) = do
  (exitCode, stdout, _) <- P.readProcessWithExitCode (T.unpack command) (fmap T.unpack args) ""
  case exitCode of
    ExitSuccess     -> pure . Just . fmap T.pack . lines $ stdout
    (ExitFailure _) -> pure Nothing
