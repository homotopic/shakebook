module Shakebook.Aeson (
  htmlJsonFormat
, styleJsonFormat
, cofreeListJsonFormat
, writeOnlyJsonFormat
, noRead
, WriteOnlyJsonField
) where

import           Composite.Aeson
import           Composite.Aeson.Format.CofreeList
import           Composite.Aeson.WriteOnly
import Data.Aeson
import           RIO
import           Shakebook.Lucid

htmlJsonFormat :: JsonFormat e HtmlFragment
htmlJsonFormat = writeOnlyJsonFormat $ String . unHtmlFragment

styleJsonFormat :: JsonFormat e StyleFragment
styleJsonFormat = writeOnlyJsonFormat $ String . unStyleFragment
