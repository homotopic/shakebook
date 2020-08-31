{- |
   Module     : Shakebook.Mustahce
   License    : MIT
   Stability  : experimental

Utilities from "Slick.Mustache" lifted to `MonadAction` and `FileLike`.
-}

module Shakebook.Mustache (
  renderMustache'
, compileMustacheDir'
, Text.Mustache.PName
, Text.Mustache.Template
) where

import           Composite.Aeson
import Composite.Record
import           Development.Shake.Plus hiding ((:->))
import           RIO
import qualified RIO.Text.Lazy as LT
import qualified Text.Mustache
import           Text.Mustache (Template, PName)

renderMustache' :: Template -> JsonFormat e (Record a)-> Record a -> Text
renderMustache' t f x = LT.toStrict $ Text.Mustache.renderMustache t (toJsonWithFormat f x)

compileMustacheDir' :: MonadIO m => PName -> Path b Dir -> m Template
compileMustacheDir' x f = Text.Mustache.compileMustacheDir x (toFilePath f)
