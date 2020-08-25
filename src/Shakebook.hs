{- |
   Module     : Shakebook
   Copyright  : Copyright (C) 2020 Daniel Firth
   Maintainer : Daniel Firth <dan.firth@homotopic.tech
   License    : MIT
   Stability  : experimental

Module exports for Shakebook. Re-exports everything in this package
as well as shake-plus, aeson and comonad utilities.
-}
module Shakebook (
  module Control.Comonad.Cofree
, module Control.Comonad.Store
, module Control.Comonad.Store.Zipper
, module Control.Comonad.Zipper.Extra
, module Data.Aeson
, module Development.Shake.Plus
, module Shakebook.Aeson
, module Shakebook.Conventions
, module Shakebook.Defaults
, module Shakebook.Feed
, module Shakebook.Lucid
, module Shakebook.Mustache
, module Shakebook.Pandoc
, module Shakebook.Sitemap
, module Shakebook.Url
, module Text.Pandoc.Highlighting
) where

import Control.Comonad.Cofree
import Control.Comonad.Store
import Control.Comonad.Store.Zipper
import Control.Comonad.Zipper.Extra
import Data.Aeson
import Development.Shake.Plus
import Shakebook.Aeson
import Shakebook.Conventions
import Shakebook.Defaults
import Shakebook.Feed hiding (Link)
import Shakebook.Lucid
import Shakebook.Mustache
import Shakebook.Pandoc
import Shakebook.Sitemap
import Shakebook.Url
import Text.Pandoc.Highlighting
