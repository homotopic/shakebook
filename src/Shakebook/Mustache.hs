{-|
Module      : Shakebook.Mustache
Description : Slick mustache utilities re-exported to use `Path`
Copyright   : (c) Daniel Firth 2020
License     : MIT
-}

module Shakebook.Mustache (
  Text.Mustache.Template
, buildPageAction
, buildPageAction'
, compileTemplate'
) where

import           Data.Aeson
import           Development.Shake.Plus
import           RIO
import qualified Slick.Mustache
import           Text.Mustache

-- | Lifted version of `Slick.Mustache.compileTemplate'` with well-typed `Path`.
compileTemplate' :: (MonadAction m, FileLike b a) => a -> m Template
compileTemplate' = liftAction . Slick.Mustache.compileTemplate' . toFilePath . toFile

-- | Build a single page straight from a template.
buildPageAction :: (MonadAction m, FileLike b a, FileLike b' a')
                => a -- ^ The HTML templatate.
                -> Value -- ^ A JSON value.
                -> a' -- ^ The out filepath.
                -> m ()
buildPageAction template value out = do
  pageT <- compileTemplate' template
  writeFile' out $ substitute pageT value


buildPageAction' :: (MonadAction m, FileLike b a) => a -> Value -> a -> m ()
buildPageAction' = buildPageAction
