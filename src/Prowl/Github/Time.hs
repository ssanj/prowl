{-# LANGUAGE ScopedTypeVariables #-}

module Prowl.Github.Time
       (
          -- Functions
          parseCreationDate
       ,  defaultCreationDate
       ) where

import qualified Chronos                as C
import qualified Data.Attoparsec.Text   as A
import qualified Data.Text              as T
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Text.Lazy         as LT

import Prowl.Model (ProwlCreationDate(..))

-- format: YYYY-DD-MM
parseCreationDate :: T.Text -> Maybe ProwlCreationDate
parseCreationDate creationDate =
  case A.parse (C.parser_Ymd (Just '-')) creationDate of
    A.Done _ _ -> Just . ProwlCreationDate $ creationDate -- just verify that the date is valid
    _        -> Nothing

defaultCreationDate :: IO ProwlCreationDate
defaultCreationDate =  (ProwlCreationDate . LT.toStrict . LT.toLazyText . C.builder_Ymd (Just '-') . C.dayToDate) <$> C.yesterday

