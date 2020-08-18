module Prowl.Common.Model
       (
          -- Data types
          TaggedText

          -- Functions
       ,  mkTextTag
       ,  unmkTextTag
       ) where

import Data.Tagged (Tagged(..), untag)
import Data.Text   (Text)

type TaggedText a = Tagged a Text

mkTextTag :: Text -> TaggedText a
mkTextTag = Tagged

unmkTextTag :: TaggedText a -> Text
unmkTextTag = untag
