{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Prowl.Program.Script.ScriptHandler
       (
          -- Functions
          repoHandler
       ,  scalaHandler
       ,  rubyHandler
       ,  haskellHandler
       ,  defaultHandler
       ,  byLanguageHandler -- for testing
       ,  langugeScript     -- for testing
       ) where

import Prowl.Program.Model
import Prowl.Common.Model
import Prowl.Github.Model
import Prowl.Config.Model

import qualified Data.Text    as T

repoHandler ::
            Monad m =>
            ProgramHandler m    ->
            GithubOrg         ->
            GithubRepo        ->
            ProwlConfigDir    ->
            m (Maybe ScriptToRunTag)
repoHandler
  (ProgramHandler fileOps consoleOps _)
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
        FileDoesNotExist -> pure Nothing

scalaHandler ::
             Monad m =>
             ProgramHandler m ->
             ProwlConfigDir ->
             ProwlCheckoutDir ->
             m (Maybe ScriptToRunTag)
scalaHandler
  progHandler
  configDirPath
  checkoutDir =
      byLanguageHandler progHandler Scala (Direct (retagTextTag checkoutDir) (mkTextTag "build.sbt")) configDirPath

rubyHandler ::
             Monad m =>
             ProgramHandler m ->
             ProwlConfigDir ->
             ProwlCheckoutDir ->
             m (Maybe ScriptToRunTag)
rubyHandler
  progHandler
  configDirPath
  checkoutDir =
      byLanguageHandler progHandler Ruby (Direct (retagTextTag checkoutDir) (mkTextTag "Gemfile")) configDirPath

haskellHandler ::
             Monad m =>
             ProgramHandler m ->
             ProwlConfigDir ->
             ProwlCheckoutDir ->
             m (Maybe ScriptToRunTag)
haskellHandler
  progHandler
  configDirPath
  checkoutDir =
    do
      let buildDir :: DirPathTag  = retagTextTag checkoutDir
      byLanguageHandler progHandler Haskell (ByFilter buildDir ((".cabal" `T.isSuffixOf`) . unmkTextTag)) configDirPath

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
                  ProgramHandler m ->
                  Language         ->
                  FileSearchType   ->
                  ProwlConfigDir   ->
                  m (Maybe ScriptToRunTag)
byLanguageHandler
  (ProgramHandler fileOps consoleOps _)
  lang
  searchType
  configDirPath =
    do
      writeLn consoleOps ("called with: " <> (T.pack . show $ lang))
      foundLangBuildFile <- findFile fileOps searchType
      foundLangScript    <- langugeScript fileOps lang configDirPath
      pure $ liftFFR2 (\_ sf -> retagTextTag sf) foundLangBuildFile foundLangScript

langugeScript :: FileOperations m -> Language -> ProwlConfigDir -> m FileFindResult
langugeScript fileOps lang configDirPath =
    let scriptFileDir = connectDirs (retagTextTag configDirPath) [mkTextTag . T.toLower . T.pack . show $ lang]
    in findFile fileOps (scriptFileIn scriptFileDir)
