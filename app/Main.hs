{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Prowl.GitSample2    as P
import qualified Prowl.Config.Model  as P
import qualified System.Environment as SYS
import qualified Data.Text          as T

import Data.String (IsString(..))

main :: IO ()
main = do
  auth <- createGithubAuth
  org <- getOrg
  P.main auth org

getOrg :: IO T.Text
getOrg = do
  args <- SYS.getArgs
  case args of
    []       -> ioError . userError $ "organisation not supplied."
    (org:_)  -> pure . T.pack $ org

createGithubAuth :: IO P.GithubAuth
createGithubAuth =
  P.GithubAuth                                            <$>
    (P.GithubApi   <$> fromSystemEnv "PROW_GITHUB_API")   <*>
    (P.GithubToken <$> fromSystemEnv "PROW_GITHUB_TOKEN")


fromSystemEnv :: IsString a => String -> IO a
fromSystemEnv key = fromString <$> SYS.getEnv key
