module Shakebook.Aeson where

import           Composite.Aeson
import Control.Comonad.Cofree
import           Control.Monad.Except
import           Data.Aeson as A
import           Data.Aeson.BetterErrors
import           RIO
import qualified RIO.Vector as V
import           Shakebook.Lucid

data WriteOnlyJsonField = WriteOnlyJsonField
  deriving Show

noRead = throwCustomError WriteOnlyJsonField

writeOnlyJsonFormat :: (a -> Value) -> JsonFormat e a
writeOnlyJsonFormat f = jsonFormatWithoutCustomError $ JsonFormat $ JsonProfunctor f noRead

htmlJsonFormat :: JsonFormat e HtmlFragment
htmlJsonFormat = writeOnlyJsonFormat $ String . unHtmlFragment

styleJsonFormat :: JsonFormat e StyleFragment
styleJsonFormat = writeOnlyJsonFormat $ String . unStyleFragment

cofreeListJsonFormat :: JsonFormat e a -> JsonFormat e (Cofree [] a)
cofreeListJsonFormat f = writeOnlyJsonFormat $ p where
                           p = \(x :< xs) -> object $ ["head" A..= toJsonWithFormat f x] <> (
                                                  case xs of
                                                    [] -> []
                                                    _  -> ["tail" A..= Array (V.fromList $ p <$> xs) ]
                                                   )
