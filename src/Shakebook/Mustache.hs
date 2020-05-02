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
import Development.Shake (Action)
import Path
import RIO
import qualified Slick.Mustache
import Shakebook.Shake
import Shakebook.Within
import Text.Mustache

compileTemplate' :: Path Rel File -> Action Template
compileTemplate' = Slick.Mustache.compileTemplate' . toFilePath

{-| 
  Build a single page straight from a template.
-}
buildPageAction :: Path Rel File -- ^ The HTML templatate.
                -> Value -- ^ A JSON value.
                -> Path Rel File -- ^ The out filepath.
                -> Action ()
buildPageAction template value out = do
  pageT <- compileTemplate' template
  writeFile' out $ substitute pageT value

buildPageActionWithin :: Within Rel File
                      -> Value
                      -> Within Rel File
                      -> Action ()
buildPageActionWithin template value out = buildPageAction (fromWithin template) value (fromWithin out)
