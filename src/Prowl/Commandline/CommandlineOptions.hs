{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE OverloadedStrings  #-}

module Prowl.Commandline.CommandlineOptions
      (
         -- Data types
         ProwlCommand(..)
         -- Functions
      ,  prowlInfo
      ,  parseConfig
      ,  parseArguments
      ,  versionHelper
      ,  versionInfo
      ,  versionText
      ,  version
      ) where

import Options.Applicative
import Prowl.Commandline.Model
import Prowl.Config.Model
import Prowl.Github.Time (parseProwlDate)


import Development.GitRev (gitHash)

import qualified Paths_prowl  as PROWL
import qualified Data.Text    as T
import qualified Data.Version as DV

parseArguments :: IO ProwlCommand
parseArguments = customExecParser helpfulPrefs prowlInfo
                  where
                    helpfulPrefs :: ParserPrefs
                    helpfulPrefs = defaultPrefs { prefShowHelpOnError = True, prefShowHelpOnEmpty = True }

prowlInfo :: ParserInfo ProwlCommand
prowlInfo =
  info ((versionHelper <|> parseConfig) <**> helper) (
    fullDesc                                 <>
    header (T.unpack . headerVersionText $ versionInfo) <>
    progDesc ("<prowl desc>") <>
    footer "---"
  )

headerVersionText :: VersionInfo -> T.Text
headerVersionText (VersionInfo (ProwlVersion v) (ProwlGitHash h))  = "prowl: " <> v <>  " " <> h

parseConfig :: Parser ProwlCommand
parseConfig =
  let config = ProwlConfig <$>
                parseRepositoryName <*>
                parseSearchByDate   <*>
                parseWorkDirectory
  in ProwlConfigCommand <$> config

parseRepositoryName :: Parser ProwlRepositoryName
parseRepositoryName =
  ProwlRepositoryName <$>
    strOption (
        short 'r'                          <>
        long "repository"                  <>
        help "Github repository to search" <>
        metavar "REPOSITORY"
      )

parseSearchByDate :: Parser SearchType
parseSearchByDate =
  let maybeSearchTypeParser = optional (parseSearchByCreationDate <|> parseSearchByUpdatedDate)
  in (maybe SearchByDateTypeNotSupplied liftProwlDate) <$> maybeSearchTypeParser
    where
      liftProwlDate :: ProwlSearchByDateType -> SearchType
      liftProwlDate (ProwlSearchByCreatedDate dateText) = maybe SearchByDateTypeNotSupplied SearchByCreatedDate (parseProwlDate dateText)
      liftProwlDate (ProwlSearchByUpdatedDate dateText) = maybe SearchByDateTypeNotSupplied SearchByUpdatedDate (parseProwlDate dateText)

parseSearchByCreationDate :: Parser ProwlSearchByDateType
parseSearchByCreationDate =
  ProwlSearchByCreatedDate <$> strOption (
    short 'c'                     <>
    long "created"                <>
    help "search by created date (YYYY-MM-DD)" <>
    metavar "CREATED"
  )

parseSearchByUpdatedDate :: Parser ProwlSearchByDateType
parseSearchByUpdatedDate =
  ProwlSearchByUpdatedDate <$> strOption (
    short 'u'                     <>
    long "updated"                <>
    help "search by updated date (YYYY-MM-DD)" <>
    metavar "UPDATED"
  )

parseWorkDirectory :: Parser ProwlWorkDir
parseWorkDirectory =
  ProwlWorkDir <$> strOption (
    short 'w'                                       <>
    long "workdir"                                  <>
    showDefault                                     <>
    value (_prowlWorkDirLocation defaultWorkDir)    <>
    help "working directory"                        <>
    metavar "WORK_DIR"
  )

versionHelper :: Parser ProwlCommand
versionHelper =
  flag' (ProwlVersionCommand) (
    short 'v'                 <>
    long "version"            <>
    help "Show prowl version"
  )

versionText :: VersionInfo -> T.Text
versionText (VersionInfo (ProwlVersion v) (ProwlGitHash h)) = "prowl version " <> v <>  " githash:" <> h

versionInfo :: VersionInfo
versionInfo = VersionInfo (ProwlVersion $ T.pack . DV.showVersion $ PROWL.version) (ProwlGitHash . T.pack $ $(gitHash))

version :: T.Text
version = versionText versionInfo