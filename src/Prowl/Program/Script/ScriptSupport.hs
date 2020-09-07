{-# LANGUAGE OverloadedStrings #-}

module Prowl.Program.Script.ScriptSupport
       (
          -- Functions
          searchHandlers
       ,  noHandler
       ,  languageHandlers
       ) where

import Prowl.Program.Model
import Prowl.Github.Model
import Prowl.Config.Model
import Prowl.Program.Script.ScriptHandler

import Data.List.NonEmpty                  (NonEmpty, (<|))
import Data.Foldable                       (traverse_)


searchHandlers ::
               Monad m =>
               ProgramOperations m ->
               GithubOrg           ->
               GithubRepo          ->
               ProwlConfigDir      ->
               ProwlCheckoutDir    ->
               NonEmpty (m (Maybe ScriptToRunTag))
searchHandlers
  progHandler
  org
  repo
  configDirPath
  checkoutDir =
    pure (repoHandler progHandler org repo configDirPath)                   <>
    ((\f -> f progHandler configDirPath checkoutDir) <$> languageHandlers)  <>
    pure (defaultHandler configDirPath)

languageHandlers ::
                 Monad m =>
                 NonEmpty (
                   ProgramOperations m ->
                   ProwlConfigDir      ->
                     ProwlCheckoutDir  ->
                     m (Maybe ScriptToRunTag)
                   )
languageHandlers = scalaHandler <| rubyHandler <| (pure haskellHandler)

noHandler :: Applicative m => ConsoleOperations m -> m ()
noHandler consoleOps =
  traverse_ (writeLn consoleOps) [
                                   "No handlers found."
                                 , "please define a handler."
                                 , "Handlers are run in the following order:"
                                 , "\t1. <working_dir>/config/org/repo/script.sh (repo specific)"
                                 , "\t2. <working_dir>/config/<language>/script.sh (supported languages: scala|ruby|haskell)"
                                 , "\t3. <working_dir>/config/script.sh (generic)"
                                 ]
