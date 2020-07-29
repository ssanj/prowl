{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prowl.Program.ProwlSearch (main) where

import Prelude.Compat
import Prowl.GithubApi
import Prowl.Model
import Prowl.Format.Pretty (printPullRequest)

import Data.Vector (Vector, toList)

import qualified Data.Text.IO as T
import qualified Data.Text    as T
import Control.Exception (SomeException, catch, displayException)

main :: GithubAuth -> GithubOrg -> ProwlCreationDate -> IO ()
main = performSearchByPR

processMatches :: Vector PullRequest -> IO ()
processMatches = T.putStrLn . printPRs
                      where printPRs :: Vector PullRequest -> T.Text
                            printPRs prs =
                              let header  = "\nProwl\n=====\n\n" :: T.Text
                                  prBlocks = T.intercalate "\n" . printPullRequest <$> prs
                              in  header <> (T.intercalate "\n\n----------\n\n" . toList $ prBlocks)

performSearchByPR :: GithubAuth -> GithubOrg -> ProwlCreationDate -> IO ()
performSearchByPR auth org creationDate = do
  matches <- searchByPR auth org CreationDate creationDate
  processMatches matches `catch` generalErrorHandler

generalErrorHandler :: SomeException -> IO ()
generalErrorHandler ex = T.putStrLn $ "Prowl failed: " <> (T.pack . displayException $ ex)