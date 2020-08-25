{-# LANGUAGE OverloadedStrings #-}

module Prowl.Program.Script
       (
          -- Functions
          runScript
       ) where


import Prowl.Common.Model

import Prowl.Program.Terminal (runShellCommandF, Command(..))
import System.Directory       (doesFileExist)

import Control.Monad          (void)
import Data.Foldable          (asum, traverse_)
import Control.Exception      (catch, IOException)

import qualified Data.Text           as T
import qualified Data.Text.IO        as T

import qualified Prowl.Config.Model     as P
import qualified Prowl.Program.Model    as P
import qualified Prowl.Github.Model     as P

data ScriptFile

type ScriptToRun = TaggedText ScriptFile

data ScriptPath

type PathToScript = TaggedText ScriptPath

slash :: T.Text
slash = "/"

runScript :: P.GithubOrg -> P.GithubRepo -> P.ProwlConfigDir -> P.ProwlCheckoutDir ->  IO ()
runScript org repo configDir checkedOutDir =
  let scriptHandlers =
        [repoHandler org repo configDir] <> (languageHandlers configDir) <> [genericHandler configDir]
      chosenScript = asum scriptHandlers
  in (chosenScript >>= maybe noHandler (runScriptCommand checkedOutDir)) `catch` (\e -> handlerInvocationError e >> noHandler)

handlerInvocationError :: IOException -> IO ()
handlerInvocationError ex = traverse_ T.putStrLn [
                                                   "Error executing handlers: " <>  (T.pack . show $ ex)
                                                 , "Using default handler"
                                                 , " - "
                                                 ]

noHandler :: IO ()
noHandler = traverse_ T.putStrLn [
                                   "No handlers found."
                                 , "please define a handler."
                                 , "Handlers are run in the following order:"
                                 , "\t1. <working_dir>/config/org/repo/script.sh (repo specific)"
                                 , "\t2. <working_dir>/config/<language>/script.sh (supported languages: scala|ruby|haskell)"
                                 , "\t3. <working_dir>/config/script.sh (generic)"
                                 ]

repoHandler :: P.GithubOrg -> P.GithubRepo -> P.ProwlConfigDir -> IO (Maybe ScriptToRun)
repoHandler (P.GithubOrg org) (P.GithubRepo repo) configDir =
  testScript [(unmkTextTag configDir), org, repo, P.scriptName]

languageHandlers :: P.ProwlConfigDir -> [IO (Maybe ScriptToRun)]
languageHandlers configDir =
    [ scalaHandler configDir
    , rubyHandler configDir
    , haskellHandler configDir
    ]

-- TODO: run script from checkedOutDir
runScriptCommand :: P.ProwlCheckoutDir -> ScriptToRun -> IO ()
runScriptCommand wd scriptToRun =
  let command = Command (unmkTextTag scriptToRun)
  in void $ runShellCommandF command (retagTextTag wd)

testScript :: [T.Text] -> IO (Maybe ScriptToRun)
testScript = scriptHandler . joinPath

joinPath :: [T.Text] -> PathToScript
joinPath = mkTextTag . T.intercalate slash

scriptHandler :: PathToScript -> IO (Maybe ScriptToRun)
scriptHandler pathToScript = whenM (doesScriptExitAt pathToScript) (pure . Just . retagTextTag $ pathToScript)

doesScriptExitAt :: PathToScript -> IO Bool
doesScriptExitAt = doesFileExist . T.unpack . unmkTextTag

findLanguageHandler :: P.Language -> P.ProwlConfigDir ->IO (Maybe ScriptToRun)
findLanguageHandler lang configDir =
  testScript [unmkTextTag configDir, (T.toLower . T.pack . show $ lang), P.scriptName]

scalaHandler :: P.ProwlConfigDir -> IO (Maybe ScriptToRun)
scalaHandler = findLanguageHandler P.Scala

rubyHandler :: P.ProwlConfigDir -> IO (Maybe ScriptToRun)
rubyHandler = findLanguageHandler P.Ruby

haskellHandler :: P.ProwlConfigDir -> IO (Maybe ScriptToRun)
haskellHandler = findLanguageHandler P.Haskell

genericHandler :: P.ProwlConfigDir -> IO (Maybe ScriptToRun)
genericHandler configDir = testScript [unmkTextTag configDir, P.scriptName]

whenM :: (Monad m, Monoid a) => m Bool -> m a -> m a
whenM mbool trueAction = mbool >>= \b -> if b then trueAction else (return mempty)
