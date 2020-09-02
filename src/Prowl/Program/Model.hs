{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric      #-}

module Prowl.Program.Model
       (
          -- Data types
          UserSelection(..)
       ,  CheckoutDir
       ,  CheckoutFile
       ,  DirPathTag
       ,  FileNameTag
       ,  DirNameTag
       ,  ProwlCheckoutDir
       ,  FileFindResult(..)
       ,  FilePathTag
       ,  FileSearchType(..)
       ,  FileOperations(..)
       ,  ConsoleOperations(..)
       ,  ProgramHandler(..)
       ) where

import Prelude hiding (FilePath)
import GHC.Generics

import Data.Text (Text)
import Data.Aeson (ToJSON(..), genericToEncoding, defaultOptions)

import Prowl.Common.Model (TaggedText)

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

-- Absolute path  to a specific file
type FilePathTag = TaggedText FilePath

-- Absolute path to a specific directory
type DirPathTag = TaggedText DirPath

-- Directory name
type DirNameTag = TaggedText DirName

-- File name (name + extension)
type FileNameTag = TaggedText FileName

-- Result of searching for a file
data FileFindResult = FileExists FilePathTag | FileDoesNotExist

-- Type of file search to perform
-- Either Direct to a specific file
-- Or by filter by matching file names in a directory
data FileSearchType = Direct FilePathTag | ByFilter DirPathTag (FileNameTag -> Bool)

data FileOperations m =
  FileOperations {
    doesFileExist :: FilePathTag -> m FileFindResult
  , doesFileMatch :: DirPathTag -> (FileNameTag -> Bool) -> m FileFindResult
  }

data ConsoleOperations m =
  ConsoleOperations {
    writeLn :: Text -> m ()
  }

data ProgramHandler m =
  ProgramHandler {
    programHandlerFileOperation :: FileOperations m
  , programHandlerConsoleOperation :: ConsoleOperations m
 }