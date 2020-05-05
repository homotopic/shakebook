{-|
Module      : Shakebook.Mustache
Description : Slick mustache utilities re-exported to use `Path`
Copyright   : (c) Daniel Firth 2020
License     : MIT
-}

module Shakebook.Mustache (
  Text.Mustache.Template
, buildPageAction
, buildPageActionWithin
, compileTemplate'
) where

import Data.Aeson
import Path
import RIO
import qualified Slick.Mustache
import Development.Shake.Plus
import Text.Mustache
import Within

compileTemplate' :: MonadAction m => Path Rel File -> m Template
compileTemplate' = liftAction . Slick.Mustache.compileTemplate' . toFilePath

{-| 
  Build a single page straight from a template.
-}
buildPageAction :: MonadAction m
                => Path Rel File -- ^ The HTML templatate.
                -> Value -- ^ A JSON value.
                -> Path Rel File -- ^ The out filepath.
                -> m ()
buildPageAction template value out = do
  pageT <- compileTemplate' template
  writeFile' out $ substitute pageT value

buildPageActionWithin :: MonadAction m
                      => Within Rel File
                      -> Value
                      -> Within Rel File
                      -> m ()
buildPageActionWithin template value out = buildPageAction (fromWithin template) value (fromWithin out)
