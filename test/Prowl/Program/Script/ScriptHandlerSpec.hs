{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies  #-}

module Prowl.Program.Script.ScriptHandlerSpec where

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

unit_languageScriptWithoutFile :: Assertion
unit_languageScriptWithoutFile =
     let configDir             = "/config/dir/path" :: T.Text
         configDirTag          = (mkTextTag configDir :: ProwlConfigDir)
         fileOps               = FileOperations fileOperationsDoesFileExistFail undefined
         handlerProg           = languageScript fileOps Scala configDirTag
         (result, outputState) = runState handlerProg Map.empty
   in do
      result      @?= FileDoesNotExist
      outputState @?= Map.empty

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
