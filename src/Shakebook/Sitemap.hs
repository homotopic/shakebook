{- |
   Module     : Shakebook.Sitemap
   License    : MIT
   Stability  : experimental

Utilities from "Web.Sitemap.Gen" lifted to `MonadAction` and `FileLike`.
-}
module Shakebook.Sitemap (
  module Web.Sitemap.Gen
, buildSitemap
)where

import           Development.Shake.Plus
import           RIO
import qualified RIO.ByteString.Lazy    as LBS
import           Web.Sitemap.Gen

buildSitemap :: MonadAction m => [SitemapUrl] -> Path b File -> m ()
buildSitemap xs out = LBS.writeFile (toFilePath out) $ renderSitemap $ Sitemap xs
