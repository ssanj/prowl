{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies  #-}

module Prowl.Program.Script.ScriptHandlerSpec where

import Prowl.Common.Model
import Prowl.Github.Model
import Prowl.Program.Model
import Prowl.Program.Script.ScriptHandler
import Control.Monad.State.Lazy

import Prowl.Config.Model              (ProwlConfigDir, Language(..))
import Prelude                  hiding (head)
-- import Test.Tasty                      (TestTree)
-- import Test.Tasty                      (testGroup)
-- import Test.Tasty.HUnit                (assertFailure, (@?=), Assertion)
import Test.Tasty.HUnit                ((@?=), Assertion)

import qualified Data.Text       as T
import qualified Data.Map.Strict as Map

data ResultType  = TextValue T.Text | FileResult FileFindResult deriving stock (Eq, Show)
type ResultState = Map.Map T.Text [ResultType]
type TestM       = State ResultState

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

unit_byLanguageHandlerWithFile :: Assertion
unit_byLanguageHandlerWithFile  =
     let configDir              = "/config/dir/path" :: T.Text
         configDirTag           = (mkTextTag configDir :: ProwlConfigDir)
         projectDirPathTag      = (mkTextTag $ "/some/project/org/repo" :: DirPathTag)
         sbtFileNameTag         = (mkTextTag $ "build.sbt" :: FileNameTag)
         scalaScriptFilePathTag = (mkTextTag $ "/config/dir/path/scala/script.sh" :: FilePathTag)
         sbtFilePathTag         = (mkTextTag $ "/some/project/org/repo/build.sbt" :: FilePathTag)
         scalaScripTag          = (retagTextTag scalaScriptFilePathTag :: ScriptToRunTag)
         fileOps                = withFileOperations fileOperationsDoesFileExist undefined
         langFinder             = languageScriptFinder (Just scalaScriptFilePathTag)
         searchType             = Direct projectDirPathTag sbtFileNameTag
         handlerProg            = byLanguageHandler fileOps langFinder Scala searchType configDirTag
         (result, outputState)  = runState handlerProg Map.empty
   in do
      result      @?= (Just scalaScripTag)
      outputState @?= Map.fromList [
                                     ("console.writeLn", [TextValue "called with: Scala"])
                                   , ("file.doesFileExist", [FileResult $ FileExists sbtFilePathTag])
                                   ]

-- unit_byLanguageHandlerWithFile :: Assertion
-- unit_byLanguageHandlerWithFile =
--      let configDir             = "/config/dir/path" :: T.Text
--          configDirTag          = (mkTextTag configDir :: ProwlConfigDir)
--          languageFilePath      = "/config/dir/path/scala/script.sh" :: T.Text
--          languageFilePathTag   = (mkTextTag languageFilePath :: FilePathTag)
--          progOps               = withFileOperations fileOperationsDoesFileExist undefined
--          handlerProg           = byLanguageHandler progOps Scala (Direct ) configDirTag
--          (result, outputState) = runState handlerProg Map.empty
--    in do
--       result      @?= FileExists languageFilePathTag
--       outputState @?= Map.fromList [
--                                      ("console.writeLn", [TextValue "called with: scala"])
--                                    , ("file.doesFileExist", [(FileResult $ FileExists languageFilePathTag)])
--                                    ]

-- type LanguageScriptFinder m = (FileOperations m -> Language -> ProwlConfigDir -> m FileFindResult)
languageScriptFinder :: Maybe FilePathTag -> LanguageScriptFinder TestM
languageScriptFinder filepathMaybe _ _ _ = pure $ maybe FileDoesNotExist FileExists filepathMaybe

consoleOperationsWriteLn :: ConsoleOperations TestM
consoleOperationsWriteLn =
  ConsoleOperations {
    writeLn = \msg ->
      let textValue = [TextValue msg]
      in  modify(Map.insertWith (<>) "console.writeLn" textValue)
  }

fileOperationsDoesFileExist :: FilePathTag -> TestM FileFindResult
fileOperationsDoesFileExist filePath = do
  resultMap <- get
  let findResult = FileExists filePath
      result = [FileResult findResult]
  put $ Map.insertWith (<>) "file.doesFileExist" result resultMap
  pure findResult

-- fileOperationsDoesFileExist :: Map.Map FileSearchType FilePathTag -> FileSearchType -> FilePathTag -> TestM FileFindResult
-- fileOperationsDoesFileExist fileSearchMap searchType filePath = do
--   resultMap <- get
--   let fileMaybe = M.lookup filePath fileSearchMap
--       findResult = FileExists filePath
--       result = [FileResult findResult]
--   put $ Map.insertWith (<>) "file.doesFileExist" result resultMap
--   pure findResult

fileOperationsDoesFileFailExist :: FilePathTag -> TestM FileFindResult
fileOperationsDoesFileFailExist _ = pure FileDoesNotExist

withFileOperations :: (FilePathTag -> TestM FileFindResult)                         ->
                      (DirPathTag -> (FileNameTag -> Bool) -> TestM FileFindResult) ->
                      ProgramOperations TestM
withFileOperations doesFileExistTest doesFileMatchTest =
  defaultProgramOperations {
    fileOperations = FileOperations doesFileExistTest doesFileMatchTest
  }

defaultProgramOperations :: ProgramOperations TestM
defaultProgramOperations = ProgramOperations undefined consoleOperationsWriteLn undefined
