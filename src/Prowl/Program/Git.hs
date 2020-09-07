{-# LANGUAGE OverloadedStrings #-}

module Prowl.Program.Git
       (
         -- Functions
         gitClone
       ) where

import Prowl.Program.Terminal

import Control.Monad (void)

import qualified Prowl.Config.Model     as C
import qualified Prowl.Common.Model     as C
import qualified Prowl.Github.Model     as C
import qualified Prowl.Program.Model    as C
import qualified Data.Text              as T

gitClone :: Monad m => C.ProgramOperations m -> C.ProwlWorkDir -> C.GithubDomain -> C.ScriptHandler m -> C.PullRequest -> m ()
gitClone progOps@(C.ProgramOperations _ consoleOps processOps) workingDir domain handler pr = do
  let wkdir          = C._prowlWorkDirLocation $ workingDir
      hash           = C.unmkTextTag . C._prowlPullRequestDetailSHA . C._prowlPullRequestDetail $ pr
      githubOrg      = C._prowlPullRequestDetailOrg . C._prowlPullRequestDetail $ pr
      githubRepo     = C._prowlPullRequestDetailRepo . C._prowlPullRequestDetail $ pr
      org            = C._prowlGithubOrg githubOrg
      repo           = C._prowlGithubRepo githubRepo
      branch         = C._prowlPullRequestBranchValue . C._prowlPullRequestDetailBranch . C._prowlPullRequestDetail $ pr
      ghcUrl         = C.unmkTextTag . C.getGithubCloneUrl $ domain

      prowlConfigDir = C.configDir workingDir
      clonePath      = joinDirectories "/" [ghcUrl, org, repo]
      cloneUrl       = clonePath <> ".git"
      cloneDir       = joinDirectories "/" [wkdir, "clones", org, repo, branch, hash]
  C.writeLn consoleOps $ "git cloning from " <> clonePath <> ":" <> branch <> " -> " <> cloneDir
  void $ C.runTerminalApp processOps (Command "git") (Args ["clone", cloneUrl, "-b", branch, cloneDir])
  handler progOps githubOrg githubRepo prowlConfigDir (C.mkTextTag cloneDir)

-- TODO: replace with connectDirs
joinDirectories :: T.Text -> [T.Text] -> T.Text
joinDirectories = T.intercalate
