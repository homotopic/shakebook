module Shakebook where

import           Control.Comonad.Cofree
import           Control.Comonad.Store
import           Control.Comonad.Store.Zipper
import           Control.Lens                 hiding ((:<))
import           Data.Aeson                   as A
import           Data.Aeson.Lens
import           Data.List.Split
import           Data.Text.Time
import           Development.Shake
import           Development.Shake.FilePath
import           RIO                          hiding (view)
import qualified RIO.HashMap                  as HML
import           RIO.List
import           RIO.List.Partial
import qualified RIO.Text                     as T
import qualified RIO.Text.Lazy                as TL
import qualified RIO.Text.Partial             as T
import           RIO.Time
import qualified RIO.Vector                   as V
import           Slick
import           Slick.Pandoc
import           Text.Atom.Feed               as Atom
import           Text.Atom.Feed.Export        as Atom
import           Text.Pandoc.Highlighting
import           Text.Pandoc.Options

type ToC = Cofree [] String

paginate :: Int -> [a] -> Maybe (Zipper [] [a])
paginate x = zipper . chunksOf x

-- View the "content" field of a JSON Value.
viewContent :: Value -> Text
viewContent = view (key "content" . _String)

-- View the "date" field of a JSON Value as a UTCTime.
viewPostTime :: Value -> UTCTime
viewPostTime = parseISODateTime . view (key "date" . _String)

-- View the "date" field of a JSON Value as Text.
viewPostTimeRaw :: Value -> Text
viewPostTimeRaw = view (key "date" . _String)

-- View the "srcPath" field of a JSON Value.
viewSrcPath :: Value -> Text
viewSrcPath = view (key "srcPath" . _String)

-- View the "tags" field of a JSON Value as a list.
viewTags :: Value -> [Text]
viewTags = toListOf (key "tags" . values . _String)

-- View the "title" field of a JSON Value.
viewTitle :: Value -> Text
viewTitle = view (key "title" . _String)

-- View the "url" field of a JSON Value
viewUrl :: Value -> Text
viewUrl = view (key "url" . _String)

-- Union two JSON values together
withJSON :: (ToJSON a) => a -> Value -> Value
withJSON x (Object obj) = Object $ HML.union obj y
  where Object y = toJSON x
withJSON _ _ = error "Can ony add a new TOJSON object to objects"

-- Add a String field to a JSON value.
withStringField :: Text -> Text -> Value -> Value
withStringField f v =  _Object  . at f ?~ String v

-- Add an Array field to a JSON value.
withArrayField :: Text -> [Value] -> Value -> Value
withArrayField f v = _Object . at f ?~ Array (V.fromList v)

-- Add "baseUrl" field from input Text
withBaseUrl :: Text -> Value -> Value
withBaseUrl = withStringField "baseUrl"

-- Add "fullUrl" field  from input Text
withFullUrl :: Text -> Value -> Value
withFullUrl = withStringField "fullUrl"

-- Add "url" field from inputText
withUrl :: Text -> Value -> Value
withUrl = withStringField "url"

-- Add "highlighting-css" field from input Style
withHighlighting :: Style -> Value -> Value
withHighlighting = withStringField "highlighting-css" . T.pack . styleToCss

-- Add "pages" field of peeks within r hops either side of the Zipper target.
withPageNeighbours :: Int -> Zipper [] Value -> (Value -> Value)
withPageNeighbours r xs = withArrayField "pages" $ fmap (`peek` xs) [(max 0 (pos xs - r)) .. (min (size xs -1) (pos xs + r))]

-- Add "posts" field containing a list of posts.
withPosts :: [Value] -> Value -> Value
withPosts = withArrayField "posts"

-- Add both "next" and "previous" fields using `withPostNext` and `withPostPrevious`
withPostNeighbours :: Zipper [] Value -> (Value -> Value)
withPostNeighbours xs = withPostPrevious xs . withPostNext xs

-- Add "next" field with the peek of the Zipper's next position, if it exists.
withPostNext :: Zipper [] Value -> (Value -> Value)
withPostNext xs = _Object . at "next" .~ if pos xs < size xs-1 then Just (peeks (+1) xs) else Nothing

-- Add "previous" field with the peek of the Zipper's previous position, if it exists.
withPostPrevious :: Zipper [] Value -> (Value -> Value)
withPostPrevious xs = _Object . at "previous" .~ if pos xs > 0 then Just (peeks (+ (-1)) xs) else Nothing

-- Add "prettydate" field based on input Text.
withPrettyDate :: Text -> Value -> Value
withPrettyDate = withStringField "prettydate"

-- Explicitly add recent posts
withRecentPosts :: Int -> [Value] -> Value -> Value
withRecentPosts n xs = withArrayField "recent-posts" $ take n $ dateSortPosts xs

-- Add "subsections" field based on the immediate children of a Cofree [] Value.
withSubsections :: Cofree [] Value -> (Value -> Value)
withSubsections (x :< xs) = withArrayField "subsections" $ map extract xs

-- Add "srcPath" field based on input Text
withSrcPath :: Text -> Value -> Value
withSrcPath = withStringField "srcPath"

-- Generate tag links for "tagIndex" field based on an input list of tags as Text.
withTagIndex :: Text -> [Text] -> Value -> Value
withTagIndex prefix xs = withArrayField "tagindex" $ linkData prefix <$> xs

-- Generate tag links for "taglinks" field based on an input list of tags as Text.
withTagLinks :: Text -> [Text] -> Value -> Value
withTagLinks prefix xs = withArrayField "taglinks" $ linkData prefix <$> xs

-- Add "teaser" field based on input Text.
withTeaser :: Text -> Value -> Value
withTeaser = withStringField "teaser"

-- Add "title" field based on input Text.
withTitle :: Text -> Value -> Value
withTitle = withStringField "title"

-- Assuming a "url" field, enrich via a baseURL
enrichFullUrl :: Text -> Value -> Value
enrichFullUrl base v = withFullUrl (base <> viewUrl v) v

-- Assuming a "date" field, enrich using withPrettyDate and a format string.
enrichPrettyDate :: (UTCTime -> String) -> Value -> Value
enrichPrettyDate f v = withPrettyDate (T.pack . f . viewPostTime $ v) v

-- Assuming a "tags" field, enrich using withTagLinks and a prefix.
enrichTagLinks :: Text -> Value -> Value
enrichTagLinks p v = withTagLinks p (viewTags v) v

-- Assuming a "content" field with a spitter section, enrich using withTeaser
enrichTeaser :: Text -> Value -> Value
enrichTeaser s v = withTeaser (head (T.splitOn s (viewContent v))) v

-- Assuming a 'srcPath' field, enrich using withUrl using a typicalHTMLUrl
enrichTypicalUrl :: Value -> Value
enrichTypicalUrl v = withUrl (typicalHTMLUrl (viewSrcPath v))v

-- Typical Markdown to HTML path transformation, by dropping a directory and
-- changing the extension.
typicalHTMLPath :: String -> String
typicalHTMLPath = dropDirectory1 . (-<.> "html")

-- Typical URL transformation, dropping the first directory, chagnging the
-- extension to "html", and adding a preslash.
typicalHTMLUrl :: Text -> Text
typicalHTMLUrl = T.pack . ("/" <>) . typicalHTMLPath . T.unpack

-- Get a JSON Value of Markdown Data with markdown body as "contents" field
-- and the srcPath as "srcPath" field.
getMarkdownData :: ReaderOptions -> WriterOptions -> String -> Action Value
getMarkdownData readerOptions writerOptions srcPath = do
  docContent <- readFile' srcPath
  docData <- markdownToHTMLWithOpts readerOptions writerOptions . T.pack $ docContent
  return $ withSrcPath (T.pack srcPath) docData

-- Create link data object with fields "id" and "url" using a prefix and an id as input,
-- where url = prefix <> id
linkData :: Text -> Text -> Value
linkData prefix id = object ["id" A..= String id, "url" A..= String (prefix <> id)]

-- Create a blog navbar object for a posts section, with layers "toc1", "toc2", and "toc3".
blogNavbarData :: Text -- "Top level title, e.g "Blog"
               -> Text -- Root page, e.g "/posts"
               -> (UTCTime -> Text) -- Formatting function to a UTCTime to a title.
               -> (UTCTime -> Text) -- Formatting function to convert a UTCTime to a URL link
               -> [Value]
               -> Value
blogNavbarData a b f g xs = object [ "toc1" A..= object [
                                      "title" A..= String a
                                    , "url"   A..= String b
                                    , "toc2"  A..= Array (V.fromList $ map toc2 (partitionToMonths xs)) ]
                                   ] where
       toc2 t@(x : xs) = object [ "title" A..= String (f (viewPostTime x))
                                , "url"   A..= String (g (viewPostTime x))
                                , "toc3"  A..= Array (V.fromList t) ]

-- Create a toc navbar object for a docs section, with layers "toc1", "toc2" and "toc3".
tocNavbarData :: Cofree [] Value -> Value
tocNavbarData (x :< xs) =
  object ["toc1" A..= [_Object . at "toc2" ?~ Array (V.fromList $ map toc2 xs) $ x]] where
      toc2 (x :< xs) = (_Object . at "toc3" ?~ Array (V.fromList $ map extract xs)) x

-- Convert a Post to an Atom Entry
asAtomEntry :: Value -> Atom.Entry
asAtomEntry x = (Atom.nullEntry (viewUrl x) (Atom.TextString $ viewTitle x) (viewPostTimeRaw x)) {
                       Atom.entryContent = Just $ Atom.TextContent (viewContent x) }

-- Build an Atom Feed from a list of posts.
buildFeed :: Text -> Text -> [Value] -> FilePath -> Action ()
buildFeed title baseUrl xs out = do
  let fs = asAtomEntry <$> dateSortPosts xs
  let t = Atom.nullFeed baseUrl (Atom.TextString title) $ Atom.entryUpdated (head fs)
  let (Just a) = textFeed (t { Atom.feedEntries = fs })
  writeFile' out (T.unpack . TL.toStrict $ a)

-- View all post tags for a list of posts.
viewAllPostTags :: [Value] -> [Text]
viewAllPostTags = (>>= viewTags)

-- View all posts times for a list of posts.
viewAllPostTimes :: [Value] -> [UTCTime]
viewAllPostTimes = fmap viewPostTime

-- Filter a lists of posts by tag.
tagFilterPosts :: Text -> [Value] -> [Value]
tagFilterPosts tag = filter (elem tag . viewTags)

-- Sort a lists of posts by date.
dateSortPosts :: [Value] -> [Value]
dateSortPosts = sortOn (Down . viewPostTime)

-- Check whether two posts were posted in the same month.
sameMonth :: UTCTime -> UTCTime -> Bool
sameMonth a b = y1 == y2 && m1 == m2 where
  (y1, m1, _) = f a
  (y2, m2, _) = f b
  f = toGregorian . utctDay

-- Filter posts by month.
monthFilterPosts :: UTCTime -> [Value] -> [Value]
monthFilterPosts time = filter (sameMonth time . viewPostTime)

-- Partition a list of posts by the month they were posted.
partitionToMonths :: [Value] -> [[Value]]
partitionToMonths = groupBy (on sameMonth viewPostTime) . dateSortPosts
