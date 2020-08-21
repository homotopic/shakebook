module Shakebook.Aeson where

import           Composite.Aeson
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.BetterErrors
import           RIO
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
