{-# LANGUAGE DerivingStrategies #-}

module Prowl.Commandline.Model
       (
          -- Data types
          ProwlCommand(..)
       ,  ProwlVersion(..)
       ,  ProwlGitHash(..)
       ,  VersionInfo(..)
       ) where

import Prowl.Model (ProwlConfig)
import Data.Text   (Text)

data ProwlCommand = ProwlConfigCommand ProwlConfig
                  | ProwlVersionCommand deriving stock (Eq, Show)

newtype ProwlVersion = ProwlVersion Text deriving stock (Eq, Show)

newtype ProwlGitHash = ProwlGitHash Text deriving stock (Eq, Show)

data VersionInfo =
  VersionInfo {
    _prowlVersion :: ProwlVersion
  , _prowlGitHash :: ProwlGitHash
  } deriving stock (Eq, Show)
