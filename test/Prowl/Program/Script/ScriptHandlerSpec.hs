{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies  #-}

module Prowl.Program.Script.ScriptHandlerSpec where

import Prowl.Common.Model
import Prowl.Github.Model
import Prowl.Program.Model
import Prowl.Program.Script.ScriptHandler
import Control.Monad.State.Lazy

import Prelude                  hiding (head)
-- import Test.Tasty                      (TestTree)
-- import Test.Tasty                      (testGroup)
-- import Test.Tasty.HUnit                (assertFailure, (@?=), Assertion)
import Test.Tasty.HUnit                ((@?=), Assertion)
-- import Data.List                       (find)

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
