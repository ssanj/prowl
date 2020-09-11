{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies  #-}

module Prowl.Program.Script.ScriptHandlerSpec where

import Prowl.Common.Model
import Prowl.Github.Model
import Prowl.Program.Model
import Prowl.Program.Script.ScriptHandler
import Control.Monad.State.Lazy
import Prowl.Program.Script.Fixtures

import Prowl.Config.Model              (ProwlConfigDir, Language(..))
import Prelude                  hiding (head)
import Test.Tasty.HUnit                ((@?=), Assertion)

import qualified Data.Text       as T
import qualified Data.Map.Strict as Map

unit_shouldFindProjectSpecificScriptIfExists :: Assertion
unit_shouldFindProjectSpecificScriptIfExists =
   let projectFile           = ("/some/dir/path/org1/repo1/script.sh" :: T.Text)
       projectFilePathTag    = (mkTextTag projectFile :: FilePathTag)
       projectFileScriptTag  = (mkTextTag projectFile :: ScriptToRunTag)
       programOps            = withFileOperations fileOperationsDoesFileExist undefined
       org                   = GithubOrg "org1"
       repo                  = GithubRepo "repo1"
       configDir             = mkTextTag "/some/dir/path"
       handlerProg           = repoHandler programOps org repo configDir
       (result, outputState) = runState handlerProg Map.empty
   in do
      result      @?= Just projectFileScriptTag
      outputState @?= Map.fromList [
                                     ("console.writeLn",    [TextValue "repoHandler"])
                                   , ("file.doesFileExist", [(FileResult $ FileExists projectFilePathTag)])
                                   ]

unit_shouldNotFindProjectSpecificScriptIfNotExists :: Assertion
unit_shouldNotFindProjectSpecificScriptIfNotExists =
   let programOps            = withFileOperations fileOperationsDoesFileFailExist undefined
       org                   = GithubOrg "org1"
       repo                  = GithubRepo "repo1"
       configDir             = mkTextTag "/some/dir/path"
       handlerProg           = repoHandler programOps org repo configDir
       (result, outputState) = runState handlerProg Map.empty
   in do
      result      @?= Nothing
      outputState @?= Map.fromList [
                                     ("console.writeLn", [TextValue "repoHandler"])
                                   ]

unit_languageScriptWithFile :: Assertion
unit_languageScriptWithFile =
     let configDir             = "/config/dir/path" :: T.Text
         configDirTag          = (mkTextTag configDir :: ProwlConfigDir)
         languageFilePath      = "/config/dir/path/scala/script.sh" :: T.Text
         languageFilePathTag   = (mkTextTag languageFilePath :: FilePathTag)
         fileOps               = FileOperations fileOperationsDoesFileExist undefined
         handlerProg           = languageScript fileOps Scala configDirTag
         (result, outputState) = runState handlerProg Map.empty
   in do
      result      @?= FileExists languageFilePathTag
      outputState @?= Map.fromList [
                                     ("file.doesFileExist", [FileResult $ FileExists languageFilePathTag])
                                   ]

unit_byLanguageHandlerFile :: Assertion
unit_byLanguageHandlerFile  =
     let configDir             = "/config/dir/path" :: T.Text
         configDirTag          = (mkTextTag configDir :: ProwlConfigDir)
         projectDirPathTag     = (mkTextTag $ "/some/project/org/repo" :: DirPathTag)
         buildFileNameTag      = (mkTextTag $ "build.sbt" :: FileNameTag)
         scriptFilePathTag     = (mkTextTag $ "/config/dir/path/scala/script.sh" :: FilePathTag)
         buildFilePathTag      = (mkTextTag $ "/some/project/org/repo/build.sbt" :: FilePathTag)
         scriptTag             = (retagTextTag scriptFilePathTag :: ScriptToRunTag)
         fileOps               = withFileOperations fileOperationsDoesFileExist undefined
         langFinder            = languageScriptFinder (Just scriptFilePathTag)
         searchType            = Direct projectDirPathTag buildFileNameTag
         handlerProg           = byLanguageHandler fileOps langFinder Scala searchType configDirTag
         (result, outputState) = runState handlerProg Map.empty
   in do
      result      @?= (Just scriptTag)
      outputState @?= Map.fromList [
                                     ("console.writeLn", [TextValue "called with: Scala"])
                                   , ("file.doesFileExist", [FileResult $ FileExists buildFilePathTag])
                                   , ("languageScriptFinder", [
                                                                TextValue "Scala",
                                                                TextValue configDir,
                                                                TextValue . unmkTextTag $ scriptFilePathTag
                                                              ])
                                   ]

unit_byLanguageHandlerWithoutBuildFile :: Assertion
unit_byLanguageHandlerWithoutBuildFile  =
     let configDir             = "/config/dir/path" :: T.Text
         configDirTag          = (mkTextTag configDir :: ProwlConfigDir)
         projectDirPathTag     = (mkTextTag $ "/some/project/org/repo" :: DirPathTag)
         buildFileNameTag      = (mkTextTag $ "build.sbt" :: FileNameTag)
         scriptFilePathTag     = (mkTextTag $ "/config/dir/path/scala/script.sh" :: FilePathTag)
         fileOps               = withFileOperations fileOperationsDoesFileFailExist undefined
         langFinder            = languageScriptFinder (Just scriptFilePathTag)
         searchType            = Direct projectDirPathTag buildFileNameTag
         handlerProg           = byLanguageHandler fileOps langFinder Scala searchType configDirTag
         (result, outputState) = runState handlerProg Map.empty
   in do
      result      @?= Nothing
      outputState @?= Map.fromList [
                                     ("console.writeLn", [TextValue "called with: Scala"])
                                   ]

unit_byLanguageHandlerWithoutScriptFile :: Assertion
unit_byLanguageHandlerWithoutScriptFile  =
     let configDir              = "/config/dir/path" :: T.Text
         configDirTag           = (mkTextTag configDir :: ProwlConfigDir)
         projectDirPathTag      = (mkTextTag $ "/some/project/org/repo" :: DirPathTag)
         sbtFileNameTag         = (mkTextTag $ "build.sbt" :: FileNameTag)
         buildFilePathTag       = (mkTextTag $ "/some/project/org/repo/build.sbt" :: FilePathTag)
         fileOps                = withFileOperations fileOperationsDoesFileExist undefined
         langFinder             = languageScriptFinder Nothing
         searchType             = Direct projectDirPathTag sbtFileNameTag
         handlerProg            = byLanguageHandler fileOps langFinder Scala searchType configDirTag
         (result, outputState)  = runState handlerProg Map.empty
   in do
      result      @?= Nothing
      outputState @?= Map.fromList [
                                     ("console.writeLn", [TextValue "called with: Scala"])
                                   , ("file.doesFileExist", [FileResult $ FileExists buildFilePathTag])
                                   , ("languageScriptFinder", [
                                                                TextValue "Scala",
                                                                TextValue configDir
                                                              ])
                                   ]

unit_defaultHandler :: Assertion
unit_defaultHandler =
  let configDir             = "/config/dir/path" :: T.Text
      configDirTag          = (mkTextTag configDir :: ProwlConfigDir)
      defaultScriptTag      = (mkTextTag "/config/dir/path/script.sh" :: ScriptToRunTag)
      handlerProg           = defaultHandler configDirTag :: TestM (Maybe ScriptToRunTag)
      (result, outputState) = runState handlerProg Map.empty
  in do
    result      @?= Just defaultScriptTag
    outputState @?= Map.empty
