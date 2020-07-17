module Shakebook.Aeson where

import RIO
import Data.Aeson
import Composite.Aeson
import Lucid
import qualified RIO.Text.Lazy as LT
import Data.Aeson.BetterErrors
import Text.Pandoc.Highlighting
import qualified RIO.Text as T
import Path
import Control.Monad.Except

newtype AesonParseException a = AesonParseException a
  deriving (Eq, Show, Ord)


instance (Typeable a, Show a) => Exception (AesonParseException a)

parseValue' :: MonadThrow m => JsonFormat Void x -> Value -> m x
parseValue' f v = do
  let a = parseValue (fromJsonWithFormat f) v
  either (throwM . AesonParseException) return a


lucidJsonFormat :: JsonFormat e (Html ())
lucidJsonFormat = JsonFormat $ JsonProfunctor (String . LT.toStrict . renderText) (throwError $ InvalidJSON "foo")

styleJsonFormat :: JsonFormat e Style
styleJsonFormat = JsonFormat $ JsonProfunctor (String . T.pack . styleToCss) (throwError $ InvalidJSON "foo")

relFileJsonFormat :: JsonFormat e (Path Rel File)
relFileJsonFormat = aesonJsonFormat
