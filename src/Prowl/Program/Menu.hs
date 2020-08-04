{-# LANGUAGE OverloadedStrings #-}

module Prowl.Program.Menu
       (
          -- Functions
          createMenu
       ) where

import Control.Exception (SomeException, try)
import Data.Foldable     (traverse_)

import qualified Data.Vector  as V
import qualified Data.Text.IO as T
import qualified Data.Text    as T

createMenu :: [T.Text] -> V.Vector a -> (V.Vector a -> T.Text) -> (a -> IO ()) -> IO ()
createMenu prompt results renderer handler = do
  T.putStrLn . renderer $ results
  processInput prompt results renderer handler

processInput :: [T.Text] -> V.Vector a -> (V.Vector a -> T.Text) -> (a -> IO ()) -> IO ()
processInput prompt results renderer handler = do
  let retry :: IO ()
      retry = processInput prompt results renderer handler

  traverse_ T.putStrLn prompt
  inputE <- try readLn :: IO (Either SomeException Int)
  case inputE of
    Left _      -> retry
    Right input ->
      if input >= 1 && input <= (V.length results) then maybe retry handler (results V.!? (input - 1))
      else retry
