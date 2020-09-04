module Shakebook.Aeson where

import           Composite.Aeson
import Control.Comonad.Cofree
import           Control.Monad.Except
import           Data.Aeson as A
import           Data.Aeson.BetterErrors
import           RIO
import qualified RIO.Vector as V
import           Shakebook.Lucid

newtype AesonParseException a = AesonParseException a
  deriving (Eq, Show, Ord)


instance (Typeable a, Show a) => Exception (AesonParseException a)

parseValue' :: MonadThrow m => JsonFormat Void x -> Value -> m x
parseValue' f v = do
  let a = parseValue (fromJsonWithFormat f) v
  either (throwM . AesonParseException) return a

data WriteOnlyJsonField = WriteOnlyJsonField
  deriving Show

htmlJsonFormat :: JsonFormat e HtmlFragment
htmlJsonFormat = jsonFormatWithoutCustomError $ JsonFormat $ JsonProfunctor (String . unHtmlFragment) (throwCustomError WriteOnlyJsonField)

styleJsonFormat :: JsonFormat e StyleFragment
styleJsonFormat = jsonFormatWithoutCustomError $ JsonFormat $ JsonProfunctor (String . unStyleFragment) (throwCustomError WriteOnlyJsonField)

cofreeListJsonFormat :: JsonFormat e a -> JsonFormat e (Cofree [] a)
cofreeListJsonFormat f = jsonFormatWithoutCustomError $ JsonFormat $ JsonProfunctor cofreeObjectFormat (throwCustomError WriteOnlyJsonField) where
    cofreeObjectFormat = \(x :< xs) -> object $ [
      "head" A..= toJsonWithFormat f x] <> (
        case xs of [] -> []
                   _  -> ["tail" A..= Array (V.fromList $ cofreeObjectFormat <$> xs) ])
