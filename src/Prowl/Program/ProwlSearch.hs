{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prowl.Program.ProwlSearch (main) where

import Prelude.Compat
import Prowl.GithubApi
import Prowl.Model
import Prowl.Program.Menu

import Data.Tuple          (swap)
import Control.Exception   (SomeException, catch, displayException)
import Prowl.Format.Pretty (printPullRequest)

import qualified Data.Vector  as V
import qualified Data.Text.IO as T
import qualified Data.Text    as T

main :: GithubAuth -> GithubOrg -> GithubSearchDate -> IO ()
main = performSearchByPR

performSearchByPR :: GithubAuth -> GithubOrg -> GithubSearchDate -> IO ()
performSearchByPR auth org searchDate = do
  matches <- searchByPR auth org searchDate
  processMatches matches `catch` generalErrorHandler

processMatches :: V.Vector PullRequest -> IO ()
processMatches prs =
  createMenu
    prompt
    (oneBasedIndex prs)
    printPRs
    handlePRSelection

oneBasedIndex :: V.Vector a -> V.Vector (Int, a)
oneBasedIndex values = swap . fmap (+1). swap <$> V.indexed values

handlePRSelection :: (Int, PullRequest) -> IO ()
handlePRSelection = T.putStrLn . ("selected:\n" <>) . T.pack . show

generalErrorHandler :: SomeException -> IO ()
generalErrorHandler ex = T.putStrLn $ "Prowl failed: " <> (T.pack . displayException $ ex)

prompt :: [T.Text]
prompt = ["\n", "Please select a PR: "]

printPRs :: V.Vector (Int, PullRequest) -> T.Text
printPRs prs =
  let header  = "\nProwl\n=====\n\n" :: T.Text
      prBlocks :: V.Vector T.Text = (\ipr -> T.intercalate "\n" . prWithIndex (fst ipr) $ printPullRequest (snd ipr)) <$> prs
  in  header <> (T.intercalate "\n\n----------\n\n" . V.toList $ prBlocks)

prWithIndex :: Int -> [T.Text] -> [T.Text]
prWithIndex index (firstLine:others) = (firstLine <> " [" <> T.pack (show index) <> "]") : others
prWithIndex _ others                 = others
