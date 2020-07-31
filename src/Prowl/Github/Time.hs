{-# LANGUAGE ScopedTypeVariables #-}

module Prowl.Github.Time
       (
          -- Functions
          parseProwlDate
       ,  yesterdayDate
       ) where

import qualified Chronos                as C
import qualified Data.Attoparsec.Text   as A
import qualified Data.Text              as T
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Text.Lazy         as LT

import Prowl.Model (ProwlDate(..))

-- format: YYYY-DD-MM
parseProwlDate :: T.Text -> Maybe ProwlDate
parseProwlDate prowlDate  =
  case A.parse (C.parser_Ymd (Just '-')) prowlDate  of
    A.Done _ _ -> Just . ProwlDate $ prowlDate  -- just verify that the date is valid
    _        -> Nothing

yesterdayDate :: IO ProwlDate
yesterdayDate =  (ProwlDate . LT.toStrict . LT.toLazyText . C.builder_Ymd (Just '-') . C.dayToDate) <$> C.yesterday
