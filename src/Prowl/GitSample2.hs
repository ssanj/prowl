{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prowl.GitSample2 (main) where

import Prelude.Compat
import Prowl.GithubApi
import Prowl.Model
import Prowl.Format.Pretty (printPullRequest)

import Data.Vector (Vector, toList)

import qualified Data.Text.IO as T
import qualified Data.Text    as T

main :: GithubAuth -> T.Text -> IO ()
main gauth org = do
  performSearchByPR gauth (GithubOrg org)

processMatches :: Vector PullRequest -> IO ()
processMatches = T.putStrLn . printPRs
                      where printPRs :: Vector PullRequest -> T.Text
                            printPRs prs =
                              let header  = "\nProwl\n=====\n\n" :: T.Text
                                  prBlocks = T.intercalate "\n" . printPullRequest <$> prs
                              in  header <> (T.intercalate "\n\n----------\n\n" . toList $ prBlocks)

performSearchByPR :: GithubAuth -> GithubOrg -> IO ()
performSearchByPR auth org = do
  matches <- searchByPR auth org
  processMatches matches --TODO: handle errors here
