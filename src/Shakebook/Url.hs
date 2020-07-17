{-# LANGUAGE TemplateHaskell #-}
module Shakebook.Url where

import RIO
import qualified RIO.Text as T
import Path

toGroundedUrl :: Path Rel b -> Text
toGroundedUrl = T.pack . toFilePath . ($(mkAbsDir "/") </>)

fromGroundedUrlD :: MonadThrow m => Text -> m (Path Rel Dir)
fromGroundedUrlD = parseAbsDir . T.unpack >=> stripProperPrefix $(mkAbsDir "/")

fromGroundedUrlF :: MonadThrow m => Text -> m (Path Rel File)
fromGroundedUrlF = parseAbsFile . T.unpack >=> stripProperPrefix $(mkAbsDir "/")
