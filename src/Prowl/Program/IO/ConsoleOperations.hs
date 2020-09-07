module Prowl.Program.IO.ConsoleOperations
       (
          -- Functions
          ioConsoleOperations
       ) where

import Prowl.Program.Model

import qualified Data.Text.IO as T

ioConsoleOperations :: ConsoleOperations IO
ioConsoleOperations =
  ConsoleOperations T.putStrLn

