{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prowl.Program.ProwlSearch (main) where

import Prelude.Compat
import Prowl.GithubApi
import Prowl.Model
import Prowl.Format.Pretty (printPullRequest)

import qualified Data.Vector as V
import Data.Tuple  (swap)

import qualified Data.Text.IO as T
import qualified Data.Text    as T
import Control.Exception (SomeException, catch, displayException, try)

main :: GithubAuth -> GithubOrg -> GithubSearchDate -> IO ()
main = performSearchByPR

processMatches :: V.Vector PullRequest -> ((Int, PullRequest) -> IO ()) -> IO ()
processMatches prs handler =
  let indexedPrs = (swap . fmap (+1). swap) <$> V.indexed prs
  in handleMenu
      "Please select a PR: "
      indexedPrs
      printPRs
      handler

handleMenu :: T.Text -> V.Vector a -> (V.Vector a -> T.Text) -> (a -> IO ()) -> IO ()
handleMenu prompt results renderer handler = do
  T.putStrLn . renderer $ results
  processInput prompt results renderer handler

processInput :: T.Text -> V.Vector a -> (V.Vector a -> T.Text) -> (a -> IO ()) -> IO ()
processInput prompt results renderer handler = do
  let retry :: IO ()
      retry = processInput prompt results renderer handler

  T.putStrLn ""
  T.putStrLn prompt
  inputE <- try readLn :: IO (Either SomeException Int)
  case inputE of
    Left _      -> retry
    Right input ->
      if input >= 1 && input <= (V.length results) then maybe retry handler (results V.!? (input - 1))
      else retry

printPRs :: V.Vector (Int, PullRequest) -> T.Text
printPRs prs =
  let header  = "\nProwl\n=====\n\n" :: T.Text
      prBlocks :: V.Vector T.Text = (\ipr -> T.intercalate "\n" . prWithIndex (fst ipr) $ printPullRequest (snd ipr)) <$> prs
  in  header <> (T.intercalate "\n\n----------\n\n" . V.toList $ prBlocks)

prWithIndex :: Int -> [T.Text] -> [T.Text]
prWithIndex index (firstLine:others) = (firstLine <> " [" <> (T.pack $ show index) <> "]") : others
prWithIndex _ others                 = others

performSearchByPR :: GithubAuth -> GithubOrg -> GithubSearchDate -> IO ()
performSearchByPR auth org searchDate = do
  matches <- searchByPR auth org searchDate
  processMatches matches (T.putStrLn . ("selected:\n" <>) . T.pack . show) `catch` generalErrorHandler

generalErrorHandler :: SomeException -> IO ()
generalErrorHandler ex = T.putStrLn $ "Prowl failed: " <> (T.pack . displayException $ ex)