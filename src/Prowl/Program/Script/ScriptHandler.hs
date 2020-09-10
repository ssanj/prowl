{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Prowl.Program.Script.ScriptHandler
       (
          -- Data types
         LanguageScriptFinder
          -- Functions
       ,  repoHandler
       ,  scalaHandler
       ,  rubyHandler
       ,  haskellHandler
       ,  defaultHandler
       ,  byLanguageHandler -- for testing
       ,  languageScript     -- for testing
       ) where

import Prowl.Program.Model
import Prowl.Common.Model
import Prowl.Github.Model
import Prowl.Config.Model

import qualified Data.Text    as T

type LanguageScriptFinder m = (FileOperations m -> Language -> ProwlConfigDir -> m FileFindResult)

repoHandler ::
            Monad m =>
            ProgramOperations m ->
            GithubOrg           ->
            GithubRepo          ->
            ProwlConfigDir      ->
            m (Maybe ScriptToRunTag)
repoHandler
  (ProgramOperations fileOps consoleOps _)
  (GithubOrg org)
  (GithubRepo repo)
  configDirPath =
    do
      writeLn consoleOps "repoHandler"
      let relativeDirs :: [DirNameTag] = mkTextTag <$> [org, repo]
          repoDirectory                = connectDirs (retagTextTag configDirPath) relativeDirs
      result <- findFile fileOps $ Direct repoDirectory (mkTextTag scriptName)
      case result of
        (FileExists file) -> pure . Just . retagTextTag $ file
        FileDoesNotExist  -> pure Nothing

scalaHandler ::
             Monad m =>
             ProgramOperations m    ->
             LanguageScriptFinder m ->
             ProwlConfigDir         ->
             ProwlCheckoutDir       ->
             m (Maybe ScriptToRunTag)
scalaHandler
  progHandler
  languageScriptFinder
  configDirPath
  checkoutDir =
      byLanguageHandler progHandler languageScriptFinder Scala (Direct (retagTextTag checkoutDir) (mkTextTag "build.sbt")) configDirPath

rubyHandler ::
             Monad m =>
             ProgramOperations m    ->
             LanguageScriptFinder m ->
             ProwlConfigDir         ->
             ProwlCheckoutDir       ->
             m (Maybe ScriptToRunTag)
rubyHandler
  progHandler
  languageScriptFinder
  configDirPath
  checkoutDir =
      byLanguageHandler progHandler languageScriptFinder Ruby (Direct (retagTextTag checkoutDir) (mkTextTag "Gemfile")) configDirPath

haskellHandler ::
             Monad m =>
             ProgramOperations m    ->
             LanguageScriptFinder m ->
             ProwlConfigDir         ->
             ProwlCheckoutDir       ->
             m (Maybe ScriptToRunTag)
haskellHandler
  progHandler
  languageScriptFinder
  configDirPath
  checkoutDir =
    do
      let buildDir :: DirPathTag  = retagTextTag checkoutDir
      byLanguageHandler progHandler languageScriptFinder Haskell (ByFilter buildDir ((".cabal" `T.isSuffixOf`) . unmkTextTag)) configDirPath

defaultHandler ::
             Applicative m =>
             ProwlConfigDir ->
             m (Maybe ScriptToRunTag)
defaultHandler
  configDirPath =
    do
      let buildDir :: DirPathTag  = retagTextTag configDirPath
      pure . Just . retagTextTag . absoluteScript $ buildDir

byLanguageHandler ::
                  Monad m =>
                  ProgramOperations m ->
                  LanguageScriptFinder m ->
                  Language            ->
                  FileSearchType      ->
                  ProwlConfigDir      ->
                  m (Maybe ScriptToRunTag)
byLanguageHandler
  (ProgramOperations fileOps consoleOps _)
  languageScriptFinder
  lang
  searchType
  configDirPath =
    do
      writeLn consoleOps ("called with: " <> (T.pack . show $ lang))
      foundLangBuildFile <- findFile fileOps searchType
      case foundLangBuildFile of
        FileDoesNotExist -> pure Nothing
        (FileExists _)   -> do
          foundLangScriptFile <- languageScriptFinder fileOps lang configDirPath
          case foundLangScriptFile of
            FileDoesNotExist -> pure Nothing
            (FileExists scriptFile) -> pure . Just . retagTextTag $ scriptFile

languageScript :: FileOperations m -> Language -> ProwlConfigDir -> m FileFindResult
languageScript fileOps lang configDirPath =
    let scriptFileDir = connectDirs (retagTextTag configDirPath) [mkTextTag . T.toLower . T.pack . show $ lang]
    in findFile fileOps (scriptFileIn scriptFileDir)
