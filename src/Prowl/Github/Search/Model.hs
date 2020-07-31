{-# LANGUAGE DerivingStrategies #-}

module Prowl.Github.Search.Model
       (
          -- Data types
          XGithubSearchDate(..)
       ) where

data XGithubSearchDate = XCreationDate | XUpdationDate deriving stock (Show, Eq)
