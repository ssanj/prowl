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
import qualified Data.Text                          as T
import qualified Prowl.Program.Terminal             as PT

bootstrapCheckout ::
                  Monad m =>
                  ProgramHandler m    ->
                  P.GithubOrg         ->
                  P.GithubRepo        ->
                  P.ProwlConfigDir    ->
                  ProwlCheckoutDir    ->
                  m ()
bootstrapCheckout
  progHandler
  org
  repo
  configDir
  checkoutDir =
    let handlers   = P.searchHandlers progHandler org repo configDir checkoutDir
        defHandler = P.noHandler . consoleOperations $ progHandler
    in findHandler progHandler checkoutDir handlers defHandler

findHandler :: Monad m => ProgramHandler m -> ProwlCheckoutDir -> NonEmpty (m (Maybe ScriptToRunTag)) -> m () -> m ()
findHandler progHandler checkoutDir (first :| rest) fallback =
  do
    maybeScript <- first
    case maybeScript of
      (Just script) -> runScript progHandler checkoutDir script
      Nothing       -> maybe fallback (\handlers -> findHandler progHandler checkoutDir handlers fallback) (nonEmpty rest)

runScript :: Monad m => ProgramHandler m -> ProwlCheckoutDir -> ScriptToRunTag -> m ()
runScript (ProgramHandler _ consoleOps processOps) checkoutDir script =
  do
    printRunningShellScript consoleOps script checkoutDir
    output <- runShellCommand processOps (PT.Command . unmkTextTag $ script) (retagTextTag checkoutDir)
    printOutput consoleOps output

printRunningShellScript :: ConsoleOperations m -> ScriptToRunTag -> ProwlCheckoutDir -> m ()
printRunningShellScript consoleOps script checkoutDir =
  writeLn consoleOps $ (T.pack "Running shell script: " <> script +<> T.pack ", from: " <>+ checkoutDir)

