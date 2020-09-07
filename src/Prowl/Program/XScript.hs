{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Prowl.Program.XScript
       (
          -- Functions
          runScript
       ) where


import Prowl.Common.Model

import Prowl.Program.Terminal (runShellCommandF, Command(..))
import System.Directory       (doesFileExist)

-- import Control.Monad          (void)
import Data.Foldable          (asum, traverse_)
import Control.Exception      (catch, IOException)

import qualified Data.Text           as T
import qualified Data.Text.IO        as T

import qualified Prowl.Config.Model     as P
import qualified Prowl.Program.Model    as P
import qualified Prowl.Github.Model     as P


data ScriptFile

-- Script file (checked)
type ScriptToRun = TaggedText ScriptFile

data ScriptPath

-- Possible script file (unchecked)
type PathToScript = TaggedText ScriptPath

data LanguageIdentifier

type BuildFile = TaggedText LanguageIdentifier

-- type TagX c q = TaggedText q

-- type BuildFile = TagX FilePath LanguageIdentifier
-- type PathToScript = TagX FilePath ScriptPath

-- type BuildFile = TagX FilePath LanguageIdentifier Text
-- type BuildFile = TagX FilePath Script Text
-- type BuildFile = TagX FilePath ValidatedScript Text


slash :: T.Text
slash = "/"

runScript :: P.GithubOrg -> P.GithubRepo -> P.ProwlConfigDir -> P.ProwlCheckoutDir ->  IO ()
runScript org repo configDir checkedOutDir =
  let scriptHandlers =
        ([repoHandler org repo configDir] <> (languageHandlers configDir checkedOutDir) <> [genericHandler configDir])
      chosenScript = asum scriptHandlers
  in (chosenScript >>= runScriptCommand checkedOutDir) `catch` (\e -> handlerInvocationError e >> noHandler)


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

repoHandler :: P.GithubOrg -> P.GithubRepo -> P.ProwlConfigDir -> IO ScriptToRun
repoHandler (P.GithubOrg org) (P.GithubRepo repo) configDir =
  T.putStrLn "repoHandler" >>
    testScript [(unmkTextTag configDir), org, repo, P.scriptName]

languageHandlers :: P.ProwlConfigDir -> P.ProwlCheckoutDir -> [IO ScriptToRun]
languageHandlers configDir checkedOutDir =
    [ scalaHandler  configDir  checkedOutDir
    , rubyHandler   configDir  checkedOutDir
    , haskellHandler configDir checkedOutDir
    ]

-- TODO: run script from checkedOutDir
runScriptCommand :: P.ProwlCheckoutDir -> ScriptToRun -> IO ()
runScriptCommand wd scriptToRun =
  let command = Command (unmkTextTag scriptToRun)
  in runShellCommandF command (retagTextTag wd) >>= T.putStrLn

testScript :: [T.Text] -> IO ScriptToRun
testScript = scriptHandler . joinPath

joinPath :: [T.Text] -> PathToScript
joinPath = mkTextTag . T.intercalate slash

scriptHandler :: PathToScript -> IO ScriptToRun
scriptHandler pathToScript = failOnFalse (doesScriptExitAt pathToScript) (pure . retagTextTag $ pathToScript)

doesScriptExitAt :: PathToScript -> IO Bool
doesScriptExitAt = doesFileExist . T.unpack . unmkTextTag

findLanguageHandler :: P.Language -> BuildFile -> P.ProwlConfigDir -> P.ProwlCheckoutDir -> IO ScriptToRun
findLanguageHandler lang buildFile configDir checkoutDir = do
  T.putStrLn ("called with: " <> (T.pack . show $ lang))
  _ <- testScript [unmkTextTag checkoutDir, unmkTextTag buildFile]
  testScript [unmkTextTag configDir, (T.toLower . T.pack . show $ lang), P.scriptName]

scalaHandler :: P.ProwlConfigDir -> P.ProwlCheckoutDir -> IO ScriptToRun
scalaHandler = findLanguageHandler P.Scala (mkTextTag "build.sbt") --specific

rubyHandler :: P.ProwlConfigDir -> P.ProwlCheckoutDir -> IO ScriptToRun
rubyHandler = findLanguageHandler P.Ruby (mkTextTag "Gemfile") -- specific

haskellHandler :: P.ProwlConfigDir -> P.ProwlCheckoutDir -> IO ScriptToRun
haskellHandler = findLanguageHandler P.Haskell (mkTextTag "stack.yaml") -- specific

genericHandler :: P.ProwlConfigDir -> IO ScriptToRun
genericHandler configDir = T.putStrLn "genericHandler" >> testScript [unmkTextTag configDir, P.scriptName]

-- whenM :: (Monad m, Monoid a) => m Bool -> m a -> m a
-- whenM mbool trueAction = mbool >>= \b -> if b then trueAction else pure mempty

failOnFalse :: IO Bool -> IO a -> IO a
failOnFalse mbool trueAction = mbool >>= \b -> if b then trueAction else  ioError $ userError "condition failed"
