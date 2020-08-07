{-# LANGUAGE OverloadedStrings #-}

module Prowl.Program.Menu
       (
          -- Functions
          createMenu
       ) where

import Data.Foldable     (traverse_)
import Text.Read         (readEither)

import qualified Data.Vector  as V
import qualified Data.Text.IO as T
import qualified Data.Text    as T

createMenu :: T.Text -> [T.Text] -> V.Vector a -> (V.Vector a -> T.Text) -> (a -> IO ()) -> IO ()
createMenu ripCord prompt results renderer handler = do
  T.putStrLn . renderer $ results
  processInput ripCord prompt results renderer handler

processInput :: T.Text -> [T.Text] -> V.Vector a -> (V.Vector a -> T.Text) -> (a -> IO ()) -> IO ()
processInput ripCord prompt results renderer handler = do
  let retry :: IO ()
      retry = processInput ripCord prompt results renderer handler

  traverse_ T.putStrLn prompt
  inputStr <- getLine
  case inputStr of
    x | x == (T.unpack ripCord) -> pure ()
    _          ->
      case (readEither inputStr :: Either String Int) of
        Left _      -> retry
        Right input ->
          if input >= 1 && input <= (V.length results) then maybe retry handler (results V.!? (input - 1))
          else retry
