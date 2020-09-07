module Prowl.Program.IO.ProcessOperations
       (
          -- Functions
          ioProcessOperations
       ) where

import Prowl.Program.Model

import Prowl.Program.Terminal (runShellCommandF, runCommandF)

ioProcessOperations :: ProcessOperations IO
ioProcessOperations =
  ProcessOperations
    runShellCommandF
    runCommandF
