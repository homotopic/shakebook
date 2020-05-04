module Shakebook.Shake (
  Development.Shake.Action
, Development.Shake.Rules
, Development.Shake.FilePattern
, copyFileChanged'
, getDirectoryFiles'
, getDirectoryFilesWithin'
, Development.Shake.need
, needPath
, needPathIn
, needWithin
, readFile'
, readFileIn'
, writeFile'
, phony
, MonadAction(..)
, MonadRules(..)
, RAction(..)
, runRAction
, (%>)
, putInfo
, removeFilesAfter
, withUnliftAction
, MonadUnliftAction(..)
, UnliftAction(..)
, askUnliftAction
, toAction
) where

import Control.Exception
import qualified Development.Shake
import Development.Shake (Action, Rules, FilePattern)
import Path
import RIO
import qualified RIO.Text as T
import Shakebook.Within

copyFileChanged' :: MonadAction m => Path Rel File -> Path Rel File -> m ()
copyFileChanged' x y = liftAction $ Development.Shake.copyFileChanged (toFilePath x) (toFilePath y)

getDirectoryFiles' :: MonadAction m => Path Rel Dir -> [FilePattern] -> m [Path Rel File]
getDirectoryFiles' x y = liftAction $ traverse (liftIO . parseRelFile) =<< Development.Shake.getDirectoryFiles (toFilePath x) y

getDirectoryFilesWithin' :: MonadAction m => Path Rel Dir -> [FilePattern] -> m [Within Rel File]
getDirectoryFilesWithin' x pat = do
  xs <- getDirectoryFiles' x pat
  return ((`within` x) <$> xs)

needPath :: MonadAction m => [Path Rel File] -> m ()
needPath = liftAction . Development.Shake.need . map toFilePath

needPathIn :: MonadAction m => Path Rel Dir -> [Path Rel File] -> m ()
needPathIn x ys = liftAction $ needWithin $ map (\y -> Within (x, y)) ys

needWithin :: MonadAction m => [Within Rel File] -> m ()
needWithin = needPath . map fromWithin

readFile' :: MonadAction m => Path Rel File -> m Text
readFile' = liftAction . fmap T.pack . Development.Shake.readFile' . toFilePath

readFileIn' :: MonadAction m => Path Rel Dir -> Path Rel File -> m Text
readFileIn' x y = liftAction $ readFile' $ x </> y

writeFile' :: MonadAction m => Path Rel File -> Text -> m ()
writeFile' x y = liftAction $ Development.Shake.writeFile' (toFilePath x) (T.unpack y)

class Monad m => MonadAction m where
  liftAction :: Action a -> m a

class Monad m => MonadRules m where
  liftRules :: Rules a -> m a

instance MonadAction Action where
  liftAction = id

instance MonadAction m => MonadAction (ReaderT r m) where
  liftAction = lift . liftAction

newtype UnliftAction m = UnliftAction { unliftAction :: forall a. m a -> Action a }

class MonadAction m => MonadUnliftAction m where
  {-# INLINE withRunInAction #-}
  withRunInAction :: ((forall a. m a -> Action a) -> Action b) -> m b
  withRunInAction inner = askUnliftAction >>= \u -> liftAction (inner (unliftAction u))

instance MonadUnliftAction Action where
  {-# INLINE withRunInAction #-}
  withRunInAction inner = inner id
instance MonadUnliftAction m => MonadUnliftAction (ReaderT r m) where
  {-# INLINE withRunInAction #-}
  withRunInAction inner =
    ReaderT $ \r ->
    withRunInAction $ \run ->
    inner (run . flip runReaderT r)

newtype RAction r a = RAction (ReaderT r Action a)
  deriving (Functor, Applicative, Monad, MonadReader r, MonadIO, MonadAction, MonadUnliftAction)

instance MonadThrow (RAction r) where
  throwM = liftIO . Control.Exception.throwIO

runRAction :: MonadAction m => env -> RAction env a -> m a
runRAction env (RAction (ReaderT f)) = liftAction (f env)

putInfo :: MonadAction m => String -> m ()
putInfo = liftAction . Development.Shake.putInfo

removeFilesAfter :: MonadAction m => Path Rel Dir -> [FilePattern] -> m ()
removeFilesAfter x y = liftAction $ Development.Shake.removeFilesAfter (toFilePath x) y

phony :: (MonadReader r m, MonadRules m) => String -> RAction r () -> m ()
phony x ract = ask >>= \r -> liftRules $ Development.Shake.phony x $ runRAction r ract

(%>) :: (MonadReader r m, MonadRules m) => FilePattern -> (Path Rel File -> RAction r ()) -> m ()
(%>) x ract = ask >>= \r -> liftRules $ x Development.Shake.%> (runRAction r . (ract <=< parseRelFile))

withUnliftAction :: MonadUnliftAction m => (UnliftAction m -> Action a) -> m a
withUnliftAction inner = askUnliftAction >>= liftAction . inner

askUnliftAction :: MonadUnliftAction m => m (UnliftAction m)
askUnliftAction = withRunInAction (\run -> return (UnliftAction run))

toAction :: MonadUnliftAction m => m a -> m (Action a)
toAction m = withRunInAction $ \run -> return $ run m
