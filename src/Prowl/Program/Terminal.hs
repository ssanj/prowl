{-# LANGUAGE OverloadedStrings #-}

module Prowl.Program.Terminal
       (
         -- Data types
         CmdWorkingDir
       , Command(..)
       , Args(..)

         -- Functions
       , runCommand
       , runCommandF
       , runShellCommandF
       ) where

import Prowl.Common.Model

import System.Exit         (ExitCode(..))

import qualified System.Process       as P
import qualified Data.Text            as T

newtype Command = Command T.Text
newtype Args = Args [T.Text]

data WorkingDir
type CmdWorkingDir = TaggedText WorkingDir

runCommandF :: Command -> Args -> IO T.Text
runCommandF (Command command) (Args args) = T.pack <$> (P.readProcess (T.unpack command) (fmap T.unpack args) "")

runShellCommandF :: Command -> CmdWorkingDir -> IO T.Text
runShellCommandF (Command command) wd =
  fmap T.pack $
    P.readCreateProcess
      (P.shell (T.unpack command)){
        P.cwd     = Just . T.unpack . unmkTextTag $ wd
      }
      ""

runCommand :: Command -> Args -> IO (Maybe [T.Text])
runCommand (Command command) (Args args) = do
  (exitCode, stdout, _) <- P.readProcessWithExitCode (T.unpack command) (fmap T.unpack args) ""
  case exitCode of
    ExitSuccess     -> pure . Just . fmap T.pack . lines $ stdout
    (ExitFailure _) -> pure Nothing
