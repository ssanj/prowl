{-# LANGUAGE OverloadedStrings #-}

module Prowl.Program.FileSystem.FileSystem
       (
          -- Functions
          absolute
       ,  connectDirs
       ,  findFile
       ) where

import Prowl.Program.Model
import Prowl.Common.Model

import qualified Data.Text    as T

absolute :: DirPathTag -> FileNameTag -> FilePathTag
absolute dirTag filenameTag =
  let dir      = unmkTextTag dirTag
      filename = unmkTextTag filenameTag
  in mkTextTag (dir <> "/" <> filename)

connectDirs :: DirPathTag -> [DirNameTag] -> DirPathTag
connectDirs dpTag dirNameTags =
  let dirPath = unmkTextTag dpTag
      dirNames = unmkTextTag <$> dirNameTags
  in mkTextTag . T.intercalate "/" $ (dirPath : dirNames)

findFile :: FileOperations m -> FileSearchType -> m FileFindResult
findFile fileOps (Direct dir filename)   = doesFileExist fileOps (absolute dir filename)
findFile fileOps (ByFilter dirPathTag f) = doesFileMatch fileOps dirPathTag f
