{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prowl.Program.IO.FileOperations
       (
          -- Functions
          ioFileOperations
       ,  findFileIO
       ) where

import Prowl.Common.Model
import Prowl.Program.Model

import Data.Bool                           (bool)
import Data.Foldable                       (find)

import qualified System.Directory as D
import qualified Data.Text        as T

ioFileOperations :: FileOperations IO
ioFileOperations = FileOperations doesFileExistIO doesFileMatchIO

doesFileExistIO :: FilePathTag -> IO FileFindResult
doesFileExistIO filepath = (bool FileDoesNotExist (FileExists filepath)) <$> (D.doesFileExist . T.unpack . unmkTextTag $ filepath)

doesFileMatchIO :: DirPathTag -> (FileNameTag -> Bool) -> IO FileFindResult
doesFileMatchIO dir fileNameFilter = do
  files <- D.listDirectory . T.unpack . unmkTextTag $ dir
  let filenames :: [FileNameTag]      = (mkTextTag . T.pack) <$> files
      firstMatch :: Maybe FileNameTag = find fileNameFilter filenames
      found :: FileFindResult         = maybe FileDoesNotExist (FileExists . absolute dir) firstMatch
  pure found

findFileIO :: FileSearchType -> IO FileFindResult
findFileIO searchType = findFile ioFileOperations searchType

