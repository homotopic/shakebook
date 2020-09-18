{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{- |
   Module     : Shakebook.Conventions
   License    : MIT
   Stability  : experimental

Conventions used in Shakebook projects, common lenses, generators, and indexing wrappers over Values.
-}
module Shakebook.Conventions where

import           Composite.Aeson
import           Composite.Aeson.Path
import           Composite.Record
import qualified Composite.Record.Tuple     as C
import           Composite.TH
import           Control.Comonad.Cofree
import           Control.Comonad.Store
import           Control.Comonad.Store.Zipper
import           Data.Binary.Instances.Time ()
import           Data.Hashable.Time
import           Data.IxSet.Typed           as Ix
import           Data.Vinyl                 hiding (RElem)
import           Data.Vinyl.TypeLevel
import           Development.Shake.Plus     hiding ((:->))
import           Lucid
import           RIO
import qualified RIO.Text                   as T
import           RIO.Time
import           Shakebook.Aeson
import qualified Shakebook.Feed             as Atom
import           Shakebook.Lucid
import           Shakebook.Sitemap
import Text.Compdoc
import Control.Comonad.Zipper.Extra

withLensesAndProxies [d|
  type FId            = "id"           :-> Text
  type FUrl           = "url"          :-> Text
  |]

type Link = '[FId, FUrl]

linkJsonFormat :: JsonFormat e (Record Link)
linkJsonFormat = recordJsonFormat $ field textJsonFormat :& field textJsonFormat :& RNil

withLensesAndProxies [d|
  type FCdnImports    = "cdn-imports"  :-> HtmlFragment
  type FDescription   = "description"  :-> Text
  type FHighlighting  = "highlighting" :-> StyleFragment
  type FImage         = "image"        :-> Maybe Text
  type FItems x       = "items"        :-> [Record x]
  type FModified      = "modified"     :-> UTCTime
  type FNext          = "next"         :-> HtmlFragment
  type FPageLinks     = "page-links"   :-> HtmlFragment
  type FPageNo        = "pageno"       :-> Int
  type FPrettyDate    = "pretty-date"  :-> UTCTime
  type FPrevious      = "previous"     :-> HtmlFragment
  type FPosted        = "posted"       :-> UTCTime
  type FRecentPosts x = "recent-posts" :-> [Record x]
  type FSiteTitle     = "site-title"   :-> Text
  type FSrcPath       = "src-path"     :-> Path Rel File
  type FSocialLinks   = "social-links" :-> [Record Link]
  type FSubsections x = "subsections"  :-> [Record x]
  type FTags          = "tags"         :-> [Text]
  type FTagLinks      = "tag-links"    :-> [Record Link]
  type FTeaser        = "teaser"       :-> Text
  type FTitle         = "title"        :-> Text
  type FTocHtml       = "toc"          :-> HtmlFragment
  type FTocCF         = "toc"          :-> Cofree [] (Record Link)
  |]

sbDisplayDateTimeJsonFormat :: JsonFormat e UTCTime
sbDisplayDateTimeJsonFormat = dateTimeJsonFormat defaultTimeLocale (regularDateTimeFormat "%A, %B %d, %Y" "yyyy-mm-dd" :| [])

sbShortDateJsonFormat :: JsonFormat e UTCTime
sbShortDateJsonFormat = dateTimeJsonFormat defaultTimeLocale (regularDateTimeFormat "%F" "yyyy-mm-dd" :| [])

type RawDocMeta = FModified : FTitle : FDescription : '[]

type RawPostMeta = FPosted : FImage : FTitle : FTags : '[]

type RawSingleMeta = FImage : FTitle : '[]

type RawDoc = Compdoc RawDocMeta

type RawPost = Compdoc RawPostMeta

type RawSingle = Compdoc RawSingleMeta

type Stage1PostExtras = FPrettyDate : FTagLinks : FTeaser : '[]

type Stage1Post = Stage1PostExtras ++ RawPost

type IndexPage x = FPageLinks : FTocCF : FTitle : FItems x : FPageNo : '[]

type PostIndexPage = Routed (IndexPage (Routed Stage1Post))

type FinalDoc = FTocCF : FSubsections (Routed RawDoc) : Routed RawDoc

type FinalPost = FTocCF : Routed Stage1Post

type MainPage = FRecentPosts (Routed Stage1Post) : RawSingle

type Routed x = FUrl : x

type Enrichment = FSocialLinks : FCdnImports : FHighlighting : FSiteTitle : '[]

-- | Tag indices for a `Post` for use with `IxSet`.
newtype Tag = Tag { unTag :: Text }
  deriving stock   (Show, Eq, Ord, Data, Typeable, Generic)
  deriving newtype (Hashable, Binary, NFData)

-- | Posted index for a `Post` for use with `IxSet`.
newtype Posted = Posted { unPosted :: UTCTime }
  deriving stock   (Show, Eq, Ord, Data, Typeable, Generic)
  deriving newtype (Hashable, Binary, NFData)

-- | YearMonth (yyyy, mm) index for a `Post` for use with `IxSet`.
newtype YearMonth = YearMonth { unYearMonth :: (Integer, Int) }
  deriving stock   (Show, Eq, Ord, Data, Typeable, Generic)
  deriving newtype (Hashable, Binary, NFData)

toYearMonth :: UTCTime -> YearMonth
toYearMonth = (\(a, b, _) -> YearMonth (a, b)) . toGregorian . utctDay

fromYearMonth :: YearMonth -> UTCTime
fromYearMonth (YearMonth (y,m)) = UTCTime (fromGregorian y m 1) 0

deriveTagLink :: (Tag -> Text) -> Tag -> Record Link
deriveTagLink f x = C.toSnd (f . Tag) (unTag x)

instance (Ord (Record xs), RElem FTags xs, RElem FPosted xs) => Ix.Indexable '[Tag, Posted, YearMonth] (Record xs) where
  indices = Ix.ixList (Ix.ixFun (fmap Tag . view fTags))
                      (Ix.ixFun (pure . Posted . view fPosted))
                      (Ix.ixFun (pure . toYearMonth . view fPosted))

mostRecentPosted :: Ix.IsIndexOf Posted ixs => Int -> Ix.IxSet ixs xs -> [xs]
mostRecentPosted x = take x . Ix.toDescList (Proxy @Posted)

asSitemapUrl :: (RElem FUrl xs, RElem FPosted xs) => Text -> Record xs -> SitemapUrl
asSitemapUrl baseUrl x = SitemapUrl {
   sitemapLocation = baseUrl <> view fUrl x
 , sitemapLastModified = Just (view fPosted x)
 , sitemapChangeFrequency = Nothing
 , sitemapPriority = Nothing
}

-- | Convert a Post to an Atom Entry
asAtomEntry :: (RElem FContent xs, RElem FPosted xs, RElem FUrl xs, RElem FTitle xs) => Record xs -> Atom.Entry
asAtomEntry x = (Atom.nullEntry (view fUrl x)
                  (Atom.TextString $ view fTitle x)
                  (T.pack $ formatTime defaultTimeLocale (iso8601DateFormat Nothing) $ view fPosted x)) {
                    Atom.entryContent = Just $ Atom.TextContent (view fContent x)
                  }

renderTitleLink :: (Monad m, RElem FTitle xs, RElem FUrl xs) => Record xs -> HtmlT m ()
renderTitleLink = liftA2 renderLink (view fTitle) (view fUrl)

renderDocNav :: (Monad m, RElem FTitle xs, RElem FUrl xs) => Cofree [] (Record xs) -> HtmlT m ()
renderDocNav xs = ul_ $ li_ $ renderCofree renderTitleLink xs

renderPageLinks :: (RElem FPageNo xs, RElem FUrl xs, MonadThrow m) => Int -> Zipper [] (Record xs) -> HtmlT m ()
renderPageLinks = renderZipperWithin (liftA2 renderLink (T.pack . show . view fPageNo) (view fUrl))

type BasicFields = FContent
                 : FDescription
                 : FImage
                 : FModified
                 : FPosted
                 : FSrcPath
                 : FTags
                 : FTitle
                 : FUrl
                 : '[]

basicFields :: Rec (JsonField e) BasicFields
basicFields = (field textJsonFormat                  :: JsonField e FContent)
           :& (field textJsonFormat                  :: JsonField e FDescription)
           :& (optionalField textJsonFormat          :: JsonField e FImage)
           :& (field iso8601DateTimeJsonFormat       :: JsonField e FModified)
           :& (field sbShortDateJsonFormat           :: JsonField e FPosted)
           :& (field relFileJsonFormat               :: JsonField e FSrcPath)
           :& (field (listJsonFormat textJsonFormat) :: JsonField e FTags)
           :& (field textJsonFormat                  :: JsonField e FTitle)
           :& (field textJsonFormat                  :: JsonField e FUrl)
           :& RNil

type ExtraFields = FCdnImports
                 : FHighlighting
                 : FPageLinks
                 : FPageNo
                 : FPrettyDate
                 : FSiteTitle
                 : FSocialLinks
                 : FTagLinks
                 : FTeaser
                 : FTocHtml
                 : FTocCF
                 : '[]

extraFields :: Rec (JsonField e) ExtraFields
extraFields =  (field htmlJsonFormat                    :: JsonField e FCdnImports)
            :& (field styleJsonFormat                   :: JsonField e FHighlighting)
            :& (field htmlJsonFormat                    :: JsonField e FPageLinks)
            :& (field integralJsonFormat                :: JsonField e FPageNo)
            :& (field sbDisplayDateTimeJsonFormat       :: JsonField e FPrettyDate)
            :& (field textJsonFormat                    :: JsonField e FSiteTitle)
            :& (field (listJsonFormat linkJsonFormat)   :: JsonField e FSocialLinks)
            :& (field (listJsonFormat linkJsonFormat)       :: JsonField e FTagLinks)
            :& (field textJsonFormat                        :: JsonField e FTeaser)
            :& (field htmlJsonFormat                        :: JsonField e FTocHtml)
            :& (field (cofreeListJsonFormat linkJsonFormat) :: JsonField e FTocCF)
            :& RNil

listCastElemsFormat :: (RMap a, RecordToJsonObject a, RecordFromJson a, a <: b) => JsonFormatRecord e b -> JsonFormat e [Record a]
listCastElemsFormat = listJsonFormat . recordJsonFormat . rcast

type CompositeFields = FItems (Routed Stage1Post)
                     : FSubsections (Routed RawDoc)
                     : FRecentPosts (Routed Stage1Post)
                     : '[]

compositeFields :: Rec (JsonField e) CompositeFields
compositeFields = let x = extraFields <+> basicFields
                  in (field (listCastElemsFormat x) :: JsonField e (FItems (Routed Stage1Post)))
                  :& (field (listCastElemsFormat x) :: JsonField e (FSubsections (Routed RawDoc)))
                  :& (field (listCastElemsFormat x) :: JsonField e (FRecentPosts (Routed Stage1Post)))
                  :& RNil

type StandardFields = BasicFields ++ ExtraFields ++ CompositeFields

allFields :: Rec (JsonField e) StandardFields
allFields = basicFields <+> extraFields <+> compositeFields

rawPostMetaJsonFormat :: JsonFormat e (Record RawPostMeta)
rawPostMetaJsonFormat = recordJsonFormat $ rcast basicFields

rawDocMetaJsonFormat :: JsonFormat e (Record RawDocMeta)
rawDocMetaJsonFormat = recordJsonFormat $ rcast basicFields

rawSingleMetaJsonFormat :: JsonFormat e (Record RawSingleMeta)
rawSingleMetaJsonFormat = recordJsonFormat $ rcast basicFields

enrichmentFields :: Rec (JsonField e) Enrichment
enrichmentFields = rcast allFields

mainPageJsonFields :: Rec (JsonField e) MainPage
mainPageJsonFields = rcast allFields

finalPostJsonFields :: Rec (JsonField e) FinalPost
finalPostJsonFields = rcast allFields

finalDocJsonFields :: Rec (JsonField e) FinalDoc
finalDocJsonFields = rcast allFields

postIndexPageJsonFields  :: Rec (JsonField e) PostIndexPage
postIndexPageJsonFields = rcast allFields

enrichedRecordJsonFormat :: (RMap a, RecordToJsonObject a, RecordFromJson a) => Rec (JsonField e) a -> JsonFormat e (Record (Enrichment ++ a))
enrichedRecordJsonFormat = recordJsonFormat . (enrichmentFields <+>)

type MonadSB r m = (MonadReader r m, HasLogFunc r, MonadUnliftAction m, MonadThrow m)

indexPagesBy :: (MonadThrow m, Ix.IsIndexOf a ixs) => Proxy a -> Int -> Ix.IxSet ixs (Record xs) -> m (Zipper [] (Record (FItems xs : FPageNo : '[])))
indexPagesBy k i ixset = do
  p <- paginate' i $ Ix.toDescList k ixset
  return $ p =>> \a -> extract a :*: pos a + 1 :*: RNil

