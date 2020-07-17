{- |
   Module     : Shakebook.Sitemap
   Copyright  : Copyright (C) 2020 Daniel Firth
   Maintainer : Daniel Firth <dan.firth@homotopic.tech
   License    : MIT
   Stability  : experimental

Utilities from "Web.Sitemap.Gen" lifted to `MonadAction` and `FileLike`.
-}
module Shakebook.Sitemap where

import Development.Shake.Plus
import RIO
import qualified RIO.ByteString.Lazy as LBS
import Shakebook.Conventions
import Web.Sitemap.Gen
import Composite.Record

asSitemapUrl :: (RElem FUrl xs, RElem FPosted xs) => Text -> Record xs -> SitemapUrl
asSitemapUrl baseUrl x = SitemapUrl {
                   sitemapLocation = baseUrl <> viewUrl x
                 , sitemapLastModified = Just (viewPosted x)
                 , sitemapChangeFrequency = Nothing
                 , sitemapPriority = Nothing }

buildSitemap :: (MonadAction m, FileLike b a, RElem FUrl xs, RElem FPosted xs) => Text -> [Record xs] -> a -> m ()
buildSitemap baseUrl xs out = do
  LBS.writeFile (toFilePath . toFile $ out) $ renderSitemap $ Sitemap $ fmap (asSitemapUrl baseUrl) xs
