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

          -- Functions
       ,  liftFFR2
       ,  toMaybe
       ,  fromMaybe
       ) where

import Prelude hiding (FilePath)
import GHC.Generics

import Data.Text           (Text)
import Data.Aeson          (ToJSON(..), genericToEncoding, defaultOptions)

import Prowl.Common.Model (TaggedText)

import qualified Control.Applicative as A

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

-- fileFindResult2 :: FileFindResult -> FileFindResult -> (FilePathTag -> FilePathTag -> a) -> Maybe a
-- fileFindResult2 (FileExists fp1) (FileExists fp2) f = Just $ f fp1 fp2
-- fileFindResult2 _ _ _ = Nothing

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

data ProgramHandler m =
  ProgramHandler {
    programHandlerFileOperation :: FileOperations m
  , programHandlerConsoleOperation :: ConsoleOperations m
 }