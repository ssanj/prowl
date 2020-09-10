{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prowl.Program.ScriptRunner

       (
          -- Functions
          bootstrapCheckout
       ,  findHandler
       ,  runScript
       ,  printRunningShellScript
       ) where

import Prowl.Program.Model
import Prowl.Common.Model

import Data.List.NonEmpty                  (NonEmpty((:|)), nonEmpty)

import qualified Prowl.Github.Model                 as P
import qualified Prowl.Config.Model                 as P
import qualified Prowl.Program.Script.ScriptSupport as P
import qualified Prowl.Program.Script.ScriptHandler as P
import qualified Data.Text                          as T
import qualified Prowl.Program.Terminal             as PT

bootstrapCheckout ::
                  Monad m =>
                  ProgramOperations m ->
                  P.GithubOrg         ->
                  P.GithubRepo        ->
                  P.ProwlConfigDir    ->
                  ProwlCheckoutDir    ->
                  m ()
bootstrapCheckout
  progOps
  org
  repo
  configDir
  checkoutDir =
    let handlers   = P.searchHandlers progOps P.languageScript org repo configDir checkoutDir
        defHandler = P.noHandler . consoleOperations $ progOps
    in findHandler progOps checkoutDir handlers defHandler

findHandler :: Monad m => ProgramOperations m -> ProwlCheckoutDir -> NonEmpty (m (Maybe ScriptToRunTag)) -> m () -> m ()
findHandler progOps checkoutDir (first :| rest) fallback =
  do
    maybeScript <- first
    case maybeScript of
      (Just script) -> runScript progOps checkoutDir script
      Nothing       -> maybe fallback (\handlers -> findHandler progOps checkoutDir handlers fallback) (nonEmpty rest)

runScript :: Monad m => ProgramOperations m -> ProwlCheckoutDir -> ScriptToRunTag -> m ()
runScript (ProgramOperations _ consoleOps processOps) checkoutDir script =
  do
    printRunningShellScript consoleOps script checkoutDir
    output <- runShellCommand processOps (PT.Command . unmkTextTag $ script) (retagTextTag checkoutDir)
    printOutput consoleOps output

printRunningShellScript :: ConsoleOperations m -> ScriptToRunTag -> ProwlCheckoutDir -> m ()
printRunningShellScript consoleOps script checkoutDir =
  writeLn consoleOps $ (T.pack "Running shell script: " <> script +<> T.pack ", from: " <>+ checkoutDir)

