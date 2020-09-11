{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Prowl.Program.Script.Fixtures where

import Prowl.Common.Model
import Prowl.Program.Model

import Prowl.Program.Script.ScriptHandler (LanguageScriptFinder)

import qualified Data.Text                as T
import qualified Data.Map.Strict          as Map
import qualified Control.Monad.State.Lazy as ST

data ResultType  = TextValue T.Text | FileResult FileFindResult deriving stock (Eq, Show)
type ResultState = Map.Map T.Text [ResultType]
type TestM       = ST.State ResultState

languageScriptFinder :: Maybe FilePathTag -> LanguageScriptFinder TestM
languageScriptFinder filepathMaybe _ lang configDir = do
  let langValue      = TextValue . T.pack . show $ lang
      configDirValue = TextValue $ unmkTextTag configDir
      filepathValues = maybe [] (\fp -> [TextValue $ unmkTextTag fp]) filepathMaybe
      stateValues    = langValue : configDirValue : filepathValues
  updateState "languageScriptFinder" stateValues
  pure $ maybe FileDoesNotExist FileExists filepathMaybe

updateState :: T.Text -> [ResultType] -> TestM ()
updateState key values = ST.modify(Map.insertWith (<>) key values)

consoleOperationsWriteLn :: ConsoleOperations TestM
consoleOperationsWriteLn =
  ConsoleOperations {
    writeLn = \msg ->
      let textValue = [TextValue msg]
      in  updateState "console.writeLn" textValue
  }

fileOperationsDoesFileExist :: FilePathTag -> TestM FileFindResult
fileOperationsDoesFileExist filePath = do
  let findResult = FileExists filePath
      result = [FileResult findResult]
  updateState "file.doesFileExist" result
  pure findResult

fileOperationsDoesFileExistFail :: FilePathTag -> TestM FileFindResult
fileOperationsDoesFileExistFail _ = pure FileDoesNotExist

withFileOperations :: (FilePathTag -> TestM FileFindResult)                         ->
                      (DirPathTag -> (FileNameTag -> Bool) -> TestM FileFindResult) ->
                      ProgramOperations TestM
withFileOperations doesFileExistTest doesFileMatchTest =
  defaultProgramOperations {
    fileOperations = FileOperations doesFileExistTest doesFileMatchTest
  }

defaultProgramOperations :: ProgramOperations TestM
defaultProgramOperations = ProgramOperations undefined consoleOperationsWriteLn undefined