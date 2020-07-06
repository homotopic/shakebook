module Shakebook.Sitemap where

import Data.Aeson
import Development.Shake.Plus
import RIO
import qualified RIO.ByteString.Lazy as LBS
import Shakebook.Conventions
import Shakebook.Pandoc
import Web.Sitemap.Gen

asSitemapUrl :: Text -> Value -> SitemapUrl
asSitemapUrl baseUrl x = SitemapUrl {
                   sitemapLocation = baseUrl <> viewUrl x
                 , sitemapLastModified = Just (viewPostTime x)
                 , sitemapChangeFrequency = Nothing
                 , sitemapPriority = Nothing }

buildSitemap :: (MonadAction m, FileLike b a, ToJSON v) => Text -> [v] -> a -> m ()
buildSitemap baseUrl xs out = do
  LBS.writeFile (toFilePath . toFile $ out) $ renderSitemap $ Sitemap $ fmap (asSitemapUrl baseUrl) (toJSON <$> xs)
