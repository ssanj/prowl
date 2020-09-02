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

import qualified Prowl.Github.Model as P
import qualified Prowl.Config.Model as P

data ScriptFile

-- Script file (checked)
type ScriptToRun = TaggedText ScriptFile

repoHandler ::
            Monad m =>
            FileOperations m    ->
            ConsoleOperations m ->
            P.GithubOrg         ->
            P.GithubRepo        ->
            P.ProwlConfigDir    ->
            m (Maybe ScriptToRun)
repoHandler
  fileOps
  consoleOps
  (P.GithubOrg org)
  (P.GithubRepo repo)
  configDir =
    do
      writeLn consoleOps "repoHandler"
      let relativeDirs :: [DirNameTag] = mkTextTag <$> [org, repo]
          repoDirectory                = connectDirs (retagTextTag configDir) relativeDirs
          repoScriptFile               = absolute repoDirectory $ (mkTextTag P.scriptName)

      result <- findFile fileOps $ Direct repoScriptFile
      case result of
        (FileExists file) -> pure . Just . retagTextTag $ file
        FileDoesNotExist -> pure Nothing
      --testScript fileOps [(unmkTextTag configDir), org, repo, P.scriptName]

-- import Prowl.Common.Model

-- -- import Prowl.Program.Terminal (runShellCommandF, Command(..))
-- -- import System.Directory       (doesFileExist)

-- import Control.Monad          (void)
-- import Data.Bool              (bool)
-- -- import Data.Foldable          (asum, traverse_)
-- -- import Control.Exception      (catch, IOException)

-- import qualified Data.Text           as T
-- import qualified Data.Text.IO        as T

-- import qualified Prowl.Config.Model     as P
-- import qualified Prowl.Program.Model    as P
-- import qualified Prowl.Program.Terminal as P
-- import qualified Prowl.Github.Model     as P




-- data ScriptPath

-- -- Possible script file (unchecked)
-- type PathToScript = TaggedText ScriptPath

-- data LanguageIdentifier

-- type BuildFile = TaggedText LanguageIdentifier

-- data FileOperations m =
--   FileOperations {
--     doesFileExist :: T.Text -> m Bool
--   }

-- data ConsoleOperations m =
--   ConsoleOperations {
--     writeLn :: T.Text -> m ()
--   }

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

-- testScript :: Monad m => FileOperations m -> [T.Text] -> m (Maybe ScriptToRun)
-- testScript fileOps = scriptHandler fileOps . joinPath

-- joinPath :: [T.Text] -> PathToScript
-- joinPath = mkTextTag . T.intercalate slash

-- scriptHandler :: Monad m => FileOperations m -> PathToScript -> m (Maybe ScriptToRun)
-- scriptHandler fileOps pathToScript = ifM (doesScriptExitAt fileOps pathToScript) (pure . Just . retagTextTag $ pathToScript) (pure Nothing)

-- doesScriptExitAt :: FileOperations m -> PathToScript -> m Bool
-- doesScriptExitAt fileOps = doesFileExist fileOps . unmkTextTag

-- findLanguageHandler :: Monad m => FileOperations m -> ConsoleOperations m -> P.Language -> BuildFile -> P.ProwlConfigDir -> P.ProwlCheckoutDir -> m (Maybe ScriptToRun)
-- findLanguageHandler fileOps consoleOps lang buildFile configDir checkoutDir = do
--   writeLn consoleOps ("called with: " <> (T.pack . show $ lang))
--   _ <- testScript fileOps [unmkTextTag checkoutDir, unmkTextTag buildFile]
--   testScript fileOps [unmkTextTag configDir, (T.toLower . T.pack . show $ lang), P.scriptName]

-- scalaHandler :: Monad m => FileOperations m -> ConsoleOperations m -> P.ProwlConfigDir -> P.ProwlCheckoutDir -> m (Maybe ScriptToRun)
-- scalaHandler fileOps consoleOps = findLanguageHandler fileOps consoleOps P.Scala (mkTextTag "build.sbt") --specific

-- rubyHandler :: Monad m => FileOperations m -> ConsoleOperations m -> P.ProwlConfigDir -> P.ProwlCheckoutDir -> m (Maybe ScriptToRun)
-- rubyHandler fileOps consoleOps = findLanguageHandler fileOps consoleOps P.Ruby (mkTextTag "Gemfile") -- specific

-- haskellHandler :: Monad m => FileOperations m -> ConsoleOperations m -> P.ProwlConfigDir -> P.ProwlCheckoutDir -> m (Maybe ScriptToRun)
-- haskellHandler fileOps consoleOps = findLanguageHandler fileOps consoleOps P.Haskell (mkTextTag "stack.yaml") -- specific

-- genericHandler :: Monad m => FileOperations m -> ConsoleOperations m -> P.ProwlConfigDir -> m (Maybe ScriptToRun)
-- genericHandler fileOps consoleOps configDir = writeLn consoleOps "genericHandler" >> testScript fileOps [unmkTextTag configDir, P.scriptName]

-- ifM :: Monad m => m Bool -> m a -> m a -> m a
-- ifM mbool trueAction falseAction = mbool >>= bool falseAction trueAction
