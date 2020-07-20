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


lucidJsonFormat :: JsonFormat e (Html ())
lucidJsonFormat = JsonFormat $ JsonProfunctor (String . LT.toStrict . renderText) (throwError $ InvalidJSON "foo")

styleJsonFormat :: JsonFormat e Style
styleJsonFormat = JsonFormat $ JsonProfunctor (String . T.pack . styleToCss) (throwError $ InvalidJSON "foo")

relFileJsonFormat :: JsonFormat e (Path Rel File)
relFileJsonFormat = aesonJsonFormat

instance DefaultJsonFormat (Html ()) where
  defaultJsonFormat = lucidJsonFormat

instance DefaultJsonFormat Style where
  defaultJsonFormat = styleJsonFormat

instance DefaultJsonFormat (Path Rel File) where
  defaultJsonFormat = relFileJsonFormat
