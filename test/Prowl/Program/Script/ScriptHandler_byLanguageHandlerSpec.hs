{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies  #-}

module Prowl.Program.Script.ScriptHandler_byLanguageHandlerSpec where

import Prowl.Common.Model
import Prowl.Program.Model
import Prowl.Program.Script.ScriptHandler
import Control.Monad.State.Lazy
import Prowl.Program.Script.Fixtures

import Prowl.Config.Model              (ProwlConfigDir, Language(..))
import Prelude                  hiding (head)
import Test.Tasty.HUnit                ((@?=), Assertion)

import qualified Data.Text       as T
import qualified Data.Map.Strict as Map

unit_byLanguageHandler :: Assertion
unit_byLanguageHandler  =
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
         fileOps               = withFileOperations fileOperationsDoesFileExistFail undefined
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
