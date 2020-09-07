{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prowl.Program.ScriptRunner

       (
          -- Functions
          bootstrapCheckout
       ) where

import Prowl.Program.Model
import Prowl.Common.Model

import Prowl.Program.FileSystem.FileSystem (absolute, connectDirs, findFile)
import Data.List.NonEmpty                  (NonEmpty((:|)), nonEmpty, (<|))
import Data.Foldable                       (traverse_)

import qualified Prowl.Github.Model     as P
import qualified Prowl.Config.Model     as P
import qualified Data.Text              as T
import qualified Prowl.Program.Terminal as PT

data ScriptFile

-- Script file (checked)
type ScriptToRun = TaggedText ScriptFile

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
    let handlers   = searchHandlers progHandler org repo configDir checkoutDir
        defHandler = noHandler . consoleOperations $ progHandler
    in findHandler progHandler checkoutDir handlers defHandler

findHandler :: Monad m => ProgramHandler m -> ProwlCheckoutDir -> NonEmpty (m (Maybe ScriptToRun)) -> m () -> m ()
findHandler progHandler checkoutDir (first :| rest) fallback =
  do
    maybeScript <- first
    case maybeScript of
      (Just script) -> runScript progHandler checkoutDir script
      Nothing       -> maybe fallback (\handlers -> findHandler progHandler checkoutDir handlers fallback) (nonEmpty rest)

runScript :: Monad m => ProgramHandler m -> ProwlCheckoutDir -> ScriptToRun -> m ()
runScript (ProgramHandler _ consoleOps processOps) checkoutDir script =
  do
    printRunningShellScript consoleOps script checkoutDir
    output <- runShellCommand processOps (PT.Command . unmkTextTag $ script) (retagTextTag checkoutDir)
    printOutput consoleOps output

printOutput :: ConsoleOperations m -> T.Text -> m ()
printOutput consoleOps = writeLn consoleOps

printRunningShellScript :: ConsoleOperations m -> ScriptToRun -> ProwlCheckoutDir -> m ()
printRunningShellScript consoleOps script checkoutDir =
  writeLn consoleOps $ (T.pack "Running shell script: " <> script +<> T.pack ", from: " <>+ checkoutDir)

searchHandlers ::
               Monad m =>
               ProgramHandler m    ->
               P.GithubOrg         ->
               P.GithubRepo        ->
               P.ProwlConfigDir    ->
               ProwlCheckoutDir    ->
               NonEmpty (m (Maybe ScriptToRun))
searchHandlers
  progHandler
  org
  repo
  configDir
  checkoutDir =
    pure (repoHandler progHandler org repo configDir)                   <>
    ((\f -> f progHandler configDir checkoutDir) <$> languageHandlers)  <>
    pure (defaultHandler configDir)

repoHandler ::
            Monad m =>
            ProgramHandler m    ->
            P.GithubOrg         ->
            P.GithubRepo        ->
            P.ProwlConfigDir    ->
            m (Maybe ScriptToRun)
repoHandler
  (ProgramHandler fileOps consoleOps _)
  (P.GithubOrg org)
  (P.GithubRepo repo)
  configDir =
    do
      writeLn consoleOps "repoHandler"
      let relativeDirs :: [DirNameTag] = mkTextTag <$> [org, repo]
          repoDirectory                = connectDirs (retagTextTag configDir) relativeDirs
      result <- findFile fileOps $ Direct repoDirectory (mkTextTag P.scriptName)
      case result of
        (FileExists file) -> pure . Just . retagTextTag $ file
        FileDoesNotExist -> pure Nothing

byLanguageHandler ::
                  Monad m =>
                  ProgramHandler m   ->
                  P.Language         ->
                  FileSearchType     ->
                  P.ProwlConfigDir   ->
                  m (Maybe ScriptToRun)
byLanguageHandler
  (ProgramHandler fileOps consoleOps _)
  lang
  searchType
  configDir =
    do
      writeLn consoleOps ("called with: " <> (T.pack . show $ lang))
      foundLangBuildFile <- findFile fileOps searchType
      foundLangScript    <- langugeScript fileOps lang configDir
      pure $ liftFFR2 (\_ sf -> retagTextTag sf) foundLangBuildFile foundLangScript

langugeScript :: FileOperations m -> P.Language -> P.ProwlConfigDir -> m FileFindResult
langugeScript fileOps lang configDir =
    let scriptFileDir = connectDirs (retagTextTag configDir) [mkTextTag . T.toLower . T.pack . show $ lang]
    in findFile fileOps (scriptFileIn scriptFileDir)

scalaHandler ::
             Monad m =>
             ProgramHandler m ->
             P.ProwlConfigDir ->
             ProwlCheckoutDir ->
             m (Maybe ScriptToRun)
scalaHandler
  progHandler
  configDir
  checkoutDir =
      byLanguageHandler progHandler P.Scala (Direct (retagTextTag checkoutDir) (mkTextTag "build.sbt")) configDir

rubyHandler ::
             Monad m =>
             ProgramHandler m ->
             P.ProwlConfigDir ->
             ProwlCheckoutDir ->
             m (Maybe ScriptToRun)
rubyHandler
  progHandler
  configDir
  checkoutDir =
      byLanguageHandler progHandler P.Ruby (Direct (retagTextTag checkoutDir) (mkTextTag "Gemfile")) configDir

haskellHandler ::
             Monad m =>
             ProgramHandler m ->
             P.ProwlConfigDir ->
             ProwlCheckoutDir ->
             m (Maybe ScriptToRun)
haskellHandler
  progHandler
  configDir
  checkoutDir =
    do
      let buildDir :: DirPathTag  = retagTextTag checkoutDir
      byLanguageHandler progHandler P.Haskell (ByFilter buildDir ((".cabal" `T.isSuffixOf`) . unmkTextTag)) configDir


languageHandlers ::
                 Monad m =>
                 NonEmpty (
                   ProgramHandler m ->
                   P.ProwlConfigDir ->
                   ProwlCheckoutDir ->
                   m (Maybe ScriptToRun)
                 )
languageHandlers = scalaHandler <| rubyHandler <| (pure haskellHandler)

defaultHandler ::
             Applicative m =>
             P.ProwlConfigDir ->
             m (Maybe ScriptToRun)
defaultHandler
  configDir =
    do
      let buildDir :: DirPathTag  = retagTextTag configDir
      pure . Just . retagTextTag . absoluteScript $ buildDir

absoluteScript :: DirPathTag -> FilePathTag
absoluteScript = (flip absolute) scriptFileNameTag

scriptFileIn :: DirPathTag -> FileSearchType
scriptFileIn = (flip Direct) scriptFileNameTag

scriptFileNameTag :: FileNameTag
scriptFileNameTag = mkTextTag P.scriptName

-- -- handlerInvocationError :: IOException -> IO ()
-- -- handlerInvocationError ex = traverse_ T.putStrLn [
-- --                                                    "Error executing handlers: " <>  (T.pack . show $ ex)
-- --                                                  , "Using default handler"
-- --                                                  , " - "
-- --                                                  ]

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

