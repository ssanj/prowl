{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prowl.Program.ScriptRunner

       (
--           -- Functions
--           general
       ) where

import Prowl.Program.Model
import Prowl.Common.Model

import Prowl.Program.FileSystem.FileSystem (absolute, connectDirs, findFile)

import qualified Prowl.Github.Model     as P
import qualified Prowl.Config.Model     as P
import qualified Data.Text              as T

data ScriptFile

-- Script file (checked)
type ScriptToRun = TaggedText ScriptFile

repoHandler ::
            Monad m =>
            ProgramHandler m    ->
            P.GithubOrg         ->
            P.GithubRepo        ->
            P.ProwlConfigDir    ->
            m (Maybe ScriptToRun)
repoHandler
  (ProgramHandler fileOps consoleOps)
  (P.GithubOrg org)
  (P.GithubRepo repo)
  configDir =
    do
      writeLn consoleOps "repoHandler"
      let relativeDirs :: [DirNameTag] = mkTextTag <$> [org, repo]
          repoDirectory                = connectDirs (retagTextTag configDir) relativeDirs
      result <- findFile fileOps $ Direct repoDirectory (mkTextTag P.scriptName)
      case result of
        (FileExists file) -> pure . Just . retagTextTag $ file
        FileDoesNotExist -> pure Nothing

byLanguageHandler ::
                  Monad m =>
                  ProgramHandler m   ->
                  P.Language         ->
                  FileSearchType     ->
                  P.ProwlConfigDir   ->
                  m (Maybe ScriptToRun)
byLanguageHandler
  (ProgramHandler fileOps consoleOps)
  lang
  searchType
  configDir =
    do
      writeLn consoleOps ("called with: " <> (T.pack . show $ lang))
      foundLangBuildFile <- findFile fileOps searchType
      foundLangScript    <- langugeScript fileOps lang configDir
      pure $ liftFFR2 (\_ sf -> retagTextTag sf) foundLangBuildFile foundLangScript

langugeScript :: FileOperations m -> P.Language -> P.ProwlConfigDir -> m FileFindResult
langugeScript fileOps lang configDir =
    let scriptFileDir = connectDirs (retagTextTag configDir) [mkTextTag . T.toLower . T.pack . show $ lang]
    in findFile fileOps (scriptFileIn scriptFileDir)

scalaHandler ::
             Monad m =>
             ProgramHandler m ->
             P.ProwlConfigDir ->
             ProwlCheckoutDir ->
             m (Maybe ScriptToRun)
scalaHandler
  progHandler
  configDir
  checkoutDir =
      byLanguageHandler progHandler P.Scala (Direct (retagTextTag checkoutDir) (mkTextTag "build.sbt")) configDir

rubyHandler ::
             Monad m =>
             ProgramHandler m ->
             P.ProwlConfigDir ->
             ProwlCheckoutDir ->
             m (Maybe ScriptToRun)
rubyHandler
  progHandler
  configDir
  checkoutDir =
      byLanguageHandler progHandler P.Ruby (Direct (retagTextTag checkoutDir) (mkTextTag "Gemfile")) configDir

haskellHandler ::
             Monad m =>
             ProgramHandler m ->
             P.ProwlConfigDir ->
             ProwlCheckoutDir ->
             m (Maybe ScriptToRun)
haskellHandler
  progHandler
  configDir
  checkoutDir =
    do
      let buildDir :: DirPathTag  = retagTextTag checkoutDir
      byLanguageHandler progHandler P.Haskell (ByFilter buildDir ((".cabal" `T.isSuffixOf`) . unmkTextTag)) configDir


languageHandlers ::
                 Monad m =>
                 [
                   ProgramHandler m ->
                   P.ProwlConfigDir ->
                   ProwlCheckoutDir ->
                   m (Maybe ScriptToRun)
                 ]

languageHandlers = [scalaHandler, rubyHandler, haskellHandler]

defaultHandler ::
             Applicative m =>
             P.ProwlConfigDir ->
             m (Maybe ScriptToRun)
defaultHandler
  configDir =
    do
      let buildDir :: DirPathTag  = retagTextTag configDir
      pure . Just . retagTextTag . absoluteScript $ buildDir


searchHandlers ::
               Monad m =>
               ProgramHandler m    ->
               P.GithubOrg         ->
               P.GithubRepo        ->
               P.ProwlConfigDir    ->
               ProwlCheckoutDir    ->
               [m (Maybe ScriptToRun)]
searchHandlers
  progHandler
  org
  repo
  configDir
  checkoutDir =
    [repoHandler progHandler org repo configDir]                       <>
    ((\f -> f progHandler configDir checkoutDir) <$> languageHandlers) <>
    [defaultHandler configDir]


absoluteScript :: DirPathTag -> FilePathTag
absoluteScript = (flip absolute) scriptFileNameTag

scriptFileIn :: DirPathTag -> FileSearchType
scriptFileIn = (flip Direct) scriptFileNameTag

scriptFileNameTag :: FileNameTag
scriptFileNameTag = mkTextTag P.scriptName

-- data ProcessOperations m =
--   ProcessOperations {
--     runShellCommandF :: P.Command -> P.CmdWorkingDir -> m T.Text
--   }


-- slash :: T.Text
-- slash = "/"

-- general :: Functor m => FileOperations m -> ProcessOperations m -> m ()
-- general fileOp _ = void $ doesScriptExitAt fileOp (mkTextTag "blee")



-- -- runScript2 :: P.GithubOrg -> P.GithubRepo -> P.ProwlConfigDir -> P.ProwlCheckoutDir ->  IO ()
-- -- runScript2 org repo configDir checkedOutDir = undefined

--   -- let scriptHandlers =
--   --       ([repoHandler org repo configDir])-- <> (languageHandlers configDir checkedOutDir) <> [genericHandler configDir])
--   --     chosenScript = asum scriptHandlers
--   -- in (chosenScript >>= runScriptCommand checkedOutDir) `catch` (\e -> handlerInvocationError e >> noHandler)


-- -- handlerInvocationError :: IOException -> IO ()
-- -- handlerInvocationError ex = traverse_ T.putStrLn [
-- --                                                    "Error executing handlers: " <>  (T.pack . show $ ex)
-- --                                                  , "Using default handler"
-- --                                                  , " - "
-- --                                                  ]

-- -- noHandler :: IO ()
-- -- noHandler = traverse_ T.putStrLn [
-- --                                    "No handlers found."
-- --                                  , "please define a handler."
-- --                                  , "Handlers are run in the following order:"
-- --                                  , "\t1. <working_dir>/config/org/repo/script.sh (repo specific)"
-- --                                  , "\t2. <working_dir>/config/<language>/script.sh (supported languages: scala|ruby|haskell)"
-- --                                  , "\t3. <working_dir>/config/script.sh (generic)"
-- --                                  ]


-- languageHandlers :: Monad m => FileOperations m -> ConsoleOperations m -> P.ProwlConfigDir -> P.ProwlCheckoutDir -> [m (Maybe ScriptToRun)]
-- languageHandlers fileOps pathToScript configDir checkedOutDir =
--     [ scalaHandler   fileOps pathToScript configDir checkedOutDir
--     , rubyHandler    fileOps pathToScript configDir checkedOutDir
--     , haskellHandler fileOps pathToScript configDir checkedOutDir
--     ]

-- -- TODO: run script from checkedOutDir
-- runScriptCommand :: Monad m => ProcessOperations m -> ConsoleOperations m -> P.ProwlCheckoutDir -> ScriptToRun -> m ()
-- runScriptCommand procOps consoleOps wd scriptToRun =
--   let command = P.Command (unmkTextTag scriptToRun)
--   in runShellCommandF procOps command (retagTextTag wd) >>= writeLn consoleOps

-- genericHandler :: Monad m => FileOperations m -> ConsoleOperations m -> P.ProwlConfigDir -> m (Maybe ScriptToRun)
-- genericHandler fileOps consoleOps configDir = writeLn consoleOps "genericHandler" >> testScript fileOps [unmkTextTag configDir, P.scriptName]

-- ifM :: Monad m => m Bool -> m a -> m a -> m a
-- ifM mbool trueAction falseAction = mbool >>= bool falseAction trueAction
