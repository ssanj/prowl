{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prowl.Program.ProwlSearch (main) where

import Prowl.GithubApi
import Prowl.Model
import Prowl.Program.Menu

import Data.Tuple          (swap)
import Control.Exception   (SomeException, catch, displayException)
import Prowl.Format.Pretty (printPullRequest)

import qualified Data.Vector  as V
import qualified Data.Text.IO as T
import qualified Data.Text    as T

main :: GithubAuth -> GithubOrg -> GithubSearchDate -> (PullRequest -> IO ()) -> IO ()
main = performSearchByPR

performSearchByPR :: GithubAuth -> GithubOrg -> GithubSearchDate -> (PullRequest -> IO ()) -> IO ()
performSearchByPR auth org searchDate handlePRSelection = do
  matches <- searchByPR auth org searchDate
  -- TODO: Do we want to differentiate between having to choose an item and not?
  if V.null matches then displayNoResults
  else displayMenu matches handlePRSelection `catch` generalErrorHandler

displayNoResults :: IO ()
displayNoResults = T.putStrLn "No matches found"

ripCord :: T.Text
ripCord = "exit"

displayMenu :: V.Vector PullRequest -> (PullRequest -> IO ()) -> IO ()
displayMenu prs handler =
  createMenu
    ripCord
    prompt
    (oneBasedIndex prs)
    printPRs
    (handler . snd)

oneBasedIndex :: V.Vector a -> V.Vector (Int, a)
oneBasedIndex values = swap . fmap (+1). swap <$> V.indexed values

generalErrorHandler :: SomeException -> IO ()
generalErrorHandler ex = T.putStrLn $ "Prowl failed: " <> (T.pack . displayException $ ex)

prompt :: [T.Text]
prompt = ["\n", "Please select a PR [number]: or '" <> ripCord <> "' to quit"]

printPRs :: V.Vector (Int, PullRequest) -> T.Text
printPRs prs =
  let header  = "\nProwl\n=====\n\n" :: T.Text
      prBlocks :: V.Vector T.Text = (\ipr -> T.intercalate "\n" . prWithIndex (fst ipr) $ printPullRequest (snd ipr)) <$> prs
  in  header <> (T.intercalate "\n\n----------\n\n" . V.toList $ prBlocks)

prWithIndex :: Int -> [T.Text] -> [T.Text]
prWithIndex index (firstLine:others) = (firstLine <> " [" <> T.pack (show index) <> "]") : others
prWithIndex _ others                 = others
