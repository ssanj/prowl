{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prowl.Github.ServiceSupport (toEntAuth, processResult) where

import Prowl.Github.Model
import Prowl.Config.Model
import Control.Exception (throwIO)

import qualified GitHub            as G
import qualified Data.Text         as T

toEntAuth :: GithubAuth -> G.Auth
toEntAuth (GithubAuth (GithubApi api) (GithubToken token)) = G.EnterpriseOAuth api token

processResult :: Show e => Either e a -> IO a
processResult (Left er)      = throwIO . ProwlException . T.pack . show $ er
processResult (Right result) = pure result