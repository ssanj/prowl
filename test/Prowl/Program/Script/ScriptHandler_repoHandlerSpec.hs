{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies  #-}

module Prowl.Program.Script.ScriptHandler_repoHandlerSpec where

import Prowl.Common.Model
import Prowl.Github.Model
import Prowl.Program.Model
import Prowl.Program.Script.ScriptHandler
import Control.Monad.State.Lazy
import Prowl.Program.Script.Fixtures

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
   let programOps            = withFileOperations fileOperationsDoesFileExistFail undefined
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