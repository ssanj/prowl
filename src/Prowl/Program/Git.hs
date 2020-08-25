{-# LANGUAGE OverloadedStrings #-}

module Prowl.Program.Git
       (
         -- Functions
         gitClone
       ) where


import Prowl.Program.Terminal

-- import System.Directory    (createDirectoryIfMissing)
import Control.Monad       (void)

import qualified Prowl.Config.Model     as C
import qualified Prowl.Common.Model     as C
import qualified Prowl.Github.Model     as C
import qualified Prowl.Program.Model    as C
import qualified Data.Text              as T
import qualified Data.Text.IO           as T

type ScriptHandler = C.GithubOrg -> C.GithubRepo -> C.ProwlConfigDir -> C.ProwlCheckoutDir -> IO ()

gitClone :: C.ProwlWorkDir -> C.GithubDomain -> ScriptHandler -> C.PullRequest -> IO ()
gitClone workingDir domain handler pr = do
  let wkdir          = C._prowlWorkDirLocation $ workingDir
      hash           = C.unmkTextTag . C._prowlPullRequestDetailSHA . C._prowlPullRequestDetail $ pr
      githubOrg      = C._prowlPullRequestDetailOrg . C._prowlPullRequestDetail $ pr
      githubRepo     = C._prowlPullRequestDetailRepo . C._prowlPullRequestDetail $ pr
      org            = C._prowlGithubOrg githubOrg
      repo           = C._prowlGithubRepo githubRepo
      branch         = C._prowlPullRequestBranchValue . C._prowlPullRequestDetailBranch . C._prowlPullRequestDetail $ pr
      ghcUrl         = C.unmkTextTag . C.getGithubCloneUrl $ domain -- C.unmkTextTag .  C._prowlPullRequestDetailApi . C._prowlPullRequestDetail $ pr

      prowlConfigDir = C.configDir workingDir
      clonePath      = joinDirectories "/" [ghcUrl, org, repo]
      cloneUrl       = clonePath <> ".git"
      cloneDir       = joinDirectories "/" [wkdir, "clones", org, repo, branch, hash]
  T.putStrLn $ "git cloning from " <> clonePath <> ":" <> branch <> " -> " <> cloneDir
  void $ runCommandF (Command "git") (Args ["clone", cloneUrl, "-b", branch, cloneDir])
  handler githubOrg githubRepo prowlConfigDir (C.mkTextTag cloneDir)

joinDirectories :: T.Text -> [T.Text] -> T.Text
joinDirectories = T.intercalate
