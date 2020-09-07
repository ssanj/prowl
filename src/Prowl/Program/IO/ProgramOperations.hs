module Prowl.Program.IO.ProgramOperations
       (
          -- Functions
          ioProgramOperations
       ) where

import Prowl.Program.Model

import Prowl.Program.IO.ConsoleOperations
import Prowl.Program.IO.ProcessOperations
import Prowl.Program.IO.FileOperations

ioProgramOperations :: ProgramOperations IO
ioProgramOperations =
  ProgramOperations
    ioFileOperations
    ioConsoleOperations
    ioProcessOperations

