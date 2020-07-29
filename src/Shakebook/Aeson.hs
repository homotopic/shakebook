module Shakebook.Aeson where

import           Composite.Aeson
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.BetterErrors
import           Lucid
import           Path
import           RIO
import qualified RIO.Text                 as T
import qualified RIO.Text.Lazy            as LT
import           Text.Pandoc.Highlighting

newtype AesonParseException a = AesonParseException a
  deriving (Eq, Show, Ord)


instance (Typeable a, Show a) => Exception (AesonParseException a)

parseValue' :: MonadThrow m => JsonFormat Void x -> Value -> m x
parseValue' f v = do
  let a = parseValue (fromJsonWithFormat f) v
  either (throwM . AesonParseException) return a

data WriteOnlyJsonField = WriteOnlyJsonField
  deriving Show

lucidJsonFormat :: JsonFormat e (Html ())
lucidJsonFormat = jsonFormatWithoutCustomError $ JsonFormat $ JsonProfunctor (String . LT.toStrict . renderText) (throwCustomError WriteOnlyJsonField)

styleJsonFormat :: JsonFormat e Style
styleJsonFormat = jsonFormatWithoutCustomError $ JsonFormat $ JsonProfunctor (String . T.pack . styleToCss) (throwCustomError WriteOnlyJsonField)

relFileJsonFormat :: JsonFormat e (Path Rel File)
relFileJsonFormat = aesonJsonFormat

relDirJsonFormat :: JsonFormat e (Path Rel Dir)
relDirJsonFormat = aesonJsonFormat

absFileJsonFormat :: JsonFormat e (Path Abs File)
absFileJsonFormat = aesonJsonFormat

absDirJsonFormat :: JsonFormat e (Path Abs Dir)
absDirJsonFormat = aesonJsonFormat
