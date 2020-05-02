{-|
Module      : Shakebook.Mustache
Description : Slick mustache utilities re-exported to use `Path`
Copyright   : (c) Daniel Firth 2020
License     : MIT
-}

module Shakebook.Mustache (
  Text.Mustache.Template
, compileTemplate'
) where

import Development.Shake
import Path
import RIO
import qualified Slick.Mustache
import Text.Mustache

compileTemplate' :: Path Rel File -> Action Template
compileTemplate' = Slick.Mustache.compileTemplate' . toFilePath
