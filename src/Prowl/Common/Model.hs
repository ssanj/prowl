module Prowl.Common.Model
       (
          -- Data types
          TaggedText

          -- Functions
       ,  mkTextTag
       ,  unmkTextTag
       ,  retagTextTag
       ,  tCombine
       ,  combineT
       ,  (+<>)
       ,  (<>+)
       ) where

import Data.Tagged (Tagged(..), untag, retag)
import Data.Text   (Text)

type TaggedText a = Tagged a Text

mkTextTag :: Text -> TaggedText a
mkTextTag = Tagged

unmkTextTag :: TaggedText a -> Text
unmkTextTag = untag

retagTextTag :: TaggedText a -> TaggedText b
retagTextTag =  retag

tCombine :: Semigroup b => Tagged a b -> b -> b
tCombine t u = (untag t) <> u

combineT :: Semigroup b => b -> Tagged a b ->  b
combineT u t = u <> (untag t)


infixr 6 +<>
(+<>) :: Semigroup b => Tagged a b -> b -> b
(+<>) = tCombine


infixr 6 <>+
(<>+) :: Semigroup b => b -> Tagged a b ->  b
(<>+) = combineT
