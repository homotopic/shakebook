{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Shakebook.Aeson where

import           Control.Lens
import           Data.Aeson      as A
import           Data.Aeson.Lens
import           RIO             hiding (view)
import qualified RIO.HashMap     as HML
import qualified RIO.Vector      as V

-- | Union two JSON values together.
withJSON :: (ToJSON a) => a -> Value -> Value
withJSON x (Object obj) = Object $ HML.union obj y
  where Object y = toJSON x
withJSON _ _ = error "Can ony add a new TOJSON object to objects"

-- | Add a String field to a JSON value.
withStringField :: Text -> Text -> Value -> Value
withStringField f v =  _Object  . at f ?~ String v

-- | Add an Array field to a JSON value.
withArrayField :: Text -> [Value] -> Value -> Value
withArrayField f v = _Object . at f ?~ Array (V.fromList v)

-- | Add an Object field to a JSON value.
withObjectField :: Text -> Value -> Value -> Value
withObjectField f v = _Object . at f ?~ v

-- | Maybe add an Object field to a JSON value.
withObjectFieldMaybe :: Text -> Maybe Value -> Value -> Value
withObjectFieldMaybe f v = _Object . at f .~ v
