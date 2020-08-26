{- |
   Module     : Shakebook.Mustahce
   Copyright  : Copyright (C) 2020 Daniel Firth
   Maintainer : Daniel Firth <dan.firth@homotopic.tech
   License    : MIT
   Stability  : experimental

Utilities from "Slick.Mustache" lifted to `MonadAction` and `FileLike`.
-}

module Shakebook.Mustache where

import           Composite.Aeson
import Composite.Record
import           Development.Shake.Plus hiding ((:->))
import           RIO
import qualified RIO.Text.Lazy as LT
import           Text.Mustache

renderMustache' :: Template -> JsonFormat e (Record a)-> Record a -> Text
renderMustache' t f x = LT.toStrict $ renderMustache t (toJsonWithFormat f x)

compileMustacheDir' :: MonadIO m => PName -> Path b Dir -> m Template
compileMustacheDir' x f = compileMustacheDir x (toFilePath f)
