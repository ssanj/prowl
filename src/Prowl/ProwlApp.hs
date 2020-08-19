{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Prowl.ProwlApp (main) where

import qualified Prowl.GithubApi                      as P
import qualified Prowl.Program.ProwlSearch            as APP
import qualified Prowl.Config.Model                   as P
import qualified Prowl.Model                          as P
import qualified Prowl.Commandline.CommandlineOptions as P
import qualified Prowl.Program.Terminal               as P
import qualified System.Environment                   as SYS
import qualified Data.Text.IO                         as T

import Data.String (IsString(..))

main :: P.ProwlCommand -> IO ()
main P.ProwlVersionCommand = T.putStrLn P.version
main (P.ProwlConfigCommand (P.ProwlConfig corg csearchType workDir domain))= do
  auth <- createGithubAuth domain
  (org, creationDate) <- getArguments corg csearchType
  let handler = P.gitClone workDir domain (\_ -> T.putStrLn "called script")
  APP.main auth org creationDate handler

getArguments :: P.ProwlOrganisationName -> P.SearchType -> IO (P.GithubOrg, P.GithubSearchDate)
getArguments org searchType =
  let githubOrg = toGithubOrg org
      githubSearchDateIO = (toGithubSearchDate searchType) <$> P.yesterdayDate
  in (githubOrg, ) <$> githubSearchDateIO

toGithubOrg :: P.ProwlOrganisationName -> P.GithubOrg
toGithubOrg (P.ProwlOrganisationName org)= P.GithubOrg org

toGithubSearchDate :: P.SearchType -> P.ProwlDate -> P.GithubSearchDate
toGithubSearchDate (P.SearchByCreatedDate date) _ = P.CreationDate date
toGithubSearchDate (P.SearchByUpdatedDate date) _ = P.UpdationDate date
toGithubSearchDate P.SearchByDateTypeNotSupplied defaultDate = P.UpdationDate defaultDate

createGithubAuth :: P.GithubDomain -> IO P.GithubAuth
createGithubAuth domain =
  P.GithubAuth (P.getGithubApi domain) <$>
    (P.GithubToken <$> fromSystemEnv "PROW_GITHUB_TOKEN")

fromSystemEnv :: IsString a => String -> IO a
fromSystemEnv key = fromString <$> SYS.getEnv key
