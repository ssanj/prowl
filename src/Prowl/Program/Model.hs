{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Prowl.Program.Model
       (
          -- Data types
          UserSelection(..)
       ,  CheckoutDir
       ,  CheckoutFile
       ,  DirPathTag
       ,  FileNameTag
       ,  DirNameTag
       ,  ScriptToRunTag
       ,  ProwlCheckoutDir
       ,  FileFindResult(..)
       ,  FilePathTag
       ,  FileSearchType(..)
       ,  FileOperations(..)
       ,  ConsoleOperations(..)
       ,  ProcessOperations(..)
       ,  ProgramOperations(..)
       ,  ScriptHandler

          -- Functions
       ,  liftFFR2
       ,  toMaybe
       ,  fromMaybe
       ,  absoluteScript
       ,  absolute
       ,  scriptFileIn
       ,  scriptFileNameTag
       ,  printOutput
       ,  connectDirs
       ,  findFile
       ) where


import GHC.Generics
import Prowl.Common.Model
import Prowl.Github.Model

import Prelude                      hiding (FilePath)
import Data.Text                           (Text, intercalate)
import Data.Aeson                          (ToJSON(..), genericToEncoding, defaultOptions)
import Prowl.Config.Model                  (ProwlConfigDir, scriptName)


import qualified Prowl.Program.Terminal as T
import qualified Control.Applicative    as A

data UserSelection =
  UserSelection {
     _userSelectionOrg                    :: Text
  ,  _userSelectionRepo                   :: Text
  ,  _userSelectionBranch                 :: Text
  ,  _userSelectionPRHash                 :: Text
  ,  _userSelectionPullRequestIssueNumber :: Int
  } deriving stock (Eq, Show, Generic)

instance ToJSON UserSelection where
    toEncoding = genericToEncoding defaultOptions

data CheckoutDir
data CheckoutFile

type ProwlCheckoutDir = TaggedText CheckoutDir

-- An absolute file path
data FilePath

-- An absolute directory path
data DirPath

-- A directory name
data DirName

-- A file name (name + extension)
data FileName

-- Path to a script
data ScriptFile

-- Absolute path  to a specific file
type FilePathTag = TaggedText FilePath

-- Absolute path to a specific directory
type DirPathTag = TaggedText DirPath

-- Directory name
type DirNameTag = TaggedText DirName

-- File name (name + extension)
type FileNameTag = TaggedText FileName


-- Path to a script file that can be run
type ScriptToRunTag = TaggedText ScriptFile


-- Result of searching for a file
data FileFindResult = FileExists FilePathTag | FileDoesNotExist  deriving stock (Eq, Show)

-- fileFindResult2 :: FileFindResult -> FileFindResult -> (FilePathTag -> FilePathTag -> a) -> Maybe a
-- fileFindResult2 (FileExists fp1) (FileExists fp2) f = Just $ f fp1 fp2
-- fileFindResult2 _ _ _ = Nothing


type ScriptHandler m =
                     ProgramOperations m  ->
                     GithubOrg         ->
                     GithubRepo        ->
                     ProwlConfigDir    ->
                     ProwlCheckoutDir  ->
                     m ()

liftFFR2 :: (FilePathTag -> FilePathTag -> a) -> FileFindResult -> FileFindResult -> Maybe a
liftFFR2 f fp1 fp2 = A.liftA2 f (toMaybe fp1) (toMaybe fp2)

toMaybe :: FileFindResult -> Maybe FilePathTag
toMaybe (FileExists path) = Just path
toMaybe FileDoesNotExist = Nothing

fromMaybe :: Maybe FilePathTag -> FileFindResult
fromMaybe (Just path) = FileExists path
fromMaybe Nothing     = FileDoesNotExist

-- Type of file search to perform
-- Either Direct to a specific file
-- Or by filter by matching file names in a directory
data FileSearchType = Direct DirPathTag FileNameTag | ByFilter DirPathTag (FileNameTag -> Bool)

data FileOperations m =
  FileOperations {
    doesFileExist :: FilePathTag -> m FileFindResult
  , doesFileMatch :: DirPathTag -> (FileNameTag -> Bool) -> m FileFindResult
  }

data ConsoleOperations m =
  ConsoleOperations {
    writeLn :: Text -> m ()
  }

data ProgramOperations m =
  ProgramOperations {
    fileOperations    :: FileOperations m
  , consoleOperations :: ConsoleOperations m
  , processOperations :: ProcessOperations m
 }


data ProcessOperations m =
  ProcessOperations {
    runShellCommand :: T.Command -> T.CmdWorkingDir -> m Text
  , runTerminalApp  :: T.Command -> T.Args -> m Text
  }

absoluteScript :: DirPathTag -> FilePathTag
absoluteScript = (flip absolute) scriptFileNameTag

scriptFileIn :: DirPathTag -> FileSearchType
scriptFileIn = (flip Direct) scriptFileNameTag

scriptFileNameTag :: FileNameTag
scriptFileNameTag = mkTextTag scriptName

printOutput :: ConsoleOperations m -> Text -> m ()
printOutput consoleOps = writeLn consoleOps

absolute :: DirPathTag -> FileNameTag -> FilePathTag
absolute dirTag filenameTag =
  let dir      = unmkTextTag dirTag
      filename = unmkTextTag filenameTag
  in mkTextTag (dir <> "/" <> filename)

connectDirs :: DirPathTag -> [DirNameTag] -> DirPathTag
connectDirs dpTag dirNameTags =
  let dirPath = unmkTextTag dpTag
      dirNames = unmkTextTag <$> dirNameTags
  in mkTextTag . intercalate "/" $ (dirPath : dirNames)

findFile :: FileOperations m -> FileSearchType -> m FileFindResult
findFile fileOps (Direct dir filename)   = doesFileExist fileOps (absolute dir filename)
findFile fileOps (ByFilter dirPathTag f) = doesFileMatch fileOps dirPathTag f
