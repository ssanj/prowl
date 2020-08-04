{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Prowl.ProwlApp (main) where

import qualified Prowl.GithubApi                      as P
import qualified Prowl.Program.ProwlSearch            as APP
import qualified Prowl.Config.Model                   as P
import qualified Prowl.Model                          as P
import qualified Prowl.Commandline.CommandlineOptions as P
import qualified System.Environment                   as SYS
import qualified Data.Text.IO                         as T

import Data.String (IsString(..))

-- Add OptParsecApplicative
main :: P.ProwlCommand -> IO ()
main P.ProwlVersionCommand = T.putStrLn P.version
main (P.ProwlConfigCommand (P.ProwlConfig corg csearchType))= do
  auth <- createGithubAuth
  (org, creationDate) <- getArguments corg csearchType
  APP.main auth org creationDate

getArguments :: P.ProwlRepositoryName -> P.SearchType -> IO (P.GithubOrg, P.GithubSearchDate)
getArguments repo searchType =
  let githubOrg = toGithubOrg repo
      githubSearchDateIO = (toGithubSearchDate searchType) <$> P.yesterdayDate
  in (githubOrg, ) <$> githubSearchDateIO

toGithubOrg :: P.ProwlRepositoryName -> P.GithubOrg
toGithubOrg (P.ProwlRepositoryName repo)= P.GithubOrg repo

toGithubSearchDate :: P.SearchType -> P.ProwlDate -> P.GithubSearchDate
toGithubSearchDate (P.SearchByCreatedDate date) _ = P.CreationDate date
toGithubSearchDate (P.SearchByUpdatedDate date) _ = P.UpdationDate date
toGithubSearchDate P.SearchByDateTypeNotSupplied defaultDate = P.UpdationDate defaultDate

createGithubAuth :: IO P.GithubAuth
createGithubAuth =
  P.GithubAuth                                            <$>
    (P.GithubApi   <$> fromSystemEnv "PROW_GITHUB_API")   <*>
    (P.GithubToken <$> fromSystemEnv "PROW_GITHUB_TOKEN")


fromSystemEnv :: IsString a => String -> IO a
fromSystemEnv key = fromString <$> SYS.getEnv key
