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

import qualified Prowl.Config.Model  as C
import qualified Prowl.Github.Model  as C
import qualified System.Process      as P
import qualified Data.Text           as T
import qualified Data.Text.IO        as T

newtype Command = Command String
newtype Args = Args [String]

gitClone :: C.ProwlWorkDir -> C.PullRequest -> IO ()
gitClone workingDir pr = do
  let wkdir  = C._prowlWorkDirLocation $ workingDir
      hash   = C.untagSHAFor . C._prowlPullRequestDetailSHA . C._prowlPullRequestDetail $ pr
      (C.GithubOrg org) = C._prowlPullRequestDetailOrg . C._prowlPullRequestDetail $ pr
      repo   = C._prowlGithubRepo . C._prowlPullRequestDetailRepo . C._prowlPullRequestDetail $ pr
      branch = C._prowlPullRequestBranchValue . C._prowlPullRequestDetailBranch . C._prowlPullRequestDetail $ pr
      (C.GithubApi api)    = C._prowlPullRequestDetailApi . C._prowlPullRequestDetail $ pr

      cloneUrl = joinDirectories "/" [api, org, repo]
      cloneDir = joinDirectories "/" [wkdir, "clones", org, repo, branch, hash]
  -- _ <- runCommandF (Command "git") (Args ["clone", cloneUrl, "-b", branch, cloneDir])
  T.putStrLn $ "git clone " <> cloneUrl <> " -b " <> branch <> " " <> cloneDir

joinDirectories :: T.Text -> [T.Text] -> T.Text
joinDirectories = T.intercalate

mkDir :: C.ProwlWorkDir -> IO ()
mkDir = createDirectoryIfMissing True .  T.unpack . C._prowlWorkDirLocation

runCommandF :: Command -> Args -> IO T.Text
runCommandF (Command command) (Args args) = T.pack <$> (P.readProcess command args "")

runCommand :: Command -> Args -> IO (Maybe [T.Text])
runCommand (Command command) (Args args) = do
  (exitCode, stdout, _) <- P.readProcessWithExitCode command args ""
  case exitCode of
    ExitSuccess     -> pure . Just . fmap T.pack . lines $ stdout
    (ExitFailure _) -> pure Nothing
