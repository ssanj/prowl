{-# LANGUAGE DerivingStrategies #-}

module Prowl.Github.Search.Model
       (
          -- Data types
          GithubSearchDate(..)
       ) where

data GithubSearchDate = CreationDate | UpdationDate deriving stock (Show, Eq)
