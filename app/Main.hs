module Main where

import qualified Prowl.ProwlApp as APP
import Prowl.Commandline.CommandlineOptions (parseArguments)

main :: IO ()
main = parseArguments >>= APP.main