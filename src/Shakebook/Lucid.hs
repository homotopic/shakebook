module Shakebook.Lucid where

import           Data.Binary
import           Development.Shake.Plus
import           Lucid
import           RIO

instance Binary (Html ()) where
  put = put . renderText
  get = fmap toHtmlRaw (get :: Get Text)

instance Hashable (Html ()) where
  hashWithSalt n x = n `hashWithSalt` renderText x

instance Eq (Html ()) where
  a == b = renderText a == renderText b

instance NFData (Html ()) where
  rnf a = seq a ()
