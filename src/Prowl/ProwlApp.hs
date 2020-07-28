{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Prowl.ProwlApp (main) where

import qualified Prowl.GithubApi           as P
import qualified Prowl.Program.ProwlSearch as APP
import qualified Prowl.Config.Model        as P
import qualified Prowl.Model               as P
import qualified System.Environment        as SYS
import qualified Data.Text                 as T

import Data.String (IsString(..))

main :: IO ()
main = do
  auth <- createGithubAuth
  (org, creationDate) <- getArguments
  APP.main auth org creationDate

getArguments :: IO (P.GithubOrg, P.ProwlCreationDate)
getArguments = do
  args <- SYS.getArgs
  case args of
    []                  -> ioError . userError $ usage
    [org]               -> (createRepo org,) <$> P.defaultCreationDate
    [org, creationDate] -> (createRepo org,) <$> (getCreationDate creationDate)
    other               -> ioError . userError $ ("Invalid arguments: " <> show other)

createRepo :: String -> P.GithubOrg
createRepo = P.GithubOrg . T.pack

getCreationDate :: String -> IO P.ProwlCreationDate
getCreationDate inputCreationDate =
  let maybeCreationDate = P.parseCreationDate . T.pack $ inputCreationDate
  in maybe (ioError . userError $ creationDateFormat) pure maybeCreationDate

creationDateFormat :: String
creationDateFormat = "invalid creation_date format. Use: YYYY-MM-DD"

usage :: String
usage = "usage: prowl organisation <creation_date YYYY-MM-DD>"

createGithubAuth :: IO P.GithubAuth
createGithubAuth =
  P.GithubAuth                                            <$>
    (P.GithubApi   <$> fromSystemEnv "PROW_GITHUB_API")   <*>
    (P.GithubToken <$> fromSystemEnv "PROW_GITHUB_TOKEN")


fromSystemEnv :: IsString a => String -> IO a
fromSystemEnv key = fromString <$> SYS.getEnv key
