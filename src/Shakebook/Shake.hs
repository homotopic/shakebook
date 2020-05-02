module Shakebook.Shake (
  Development.Shake.Action
, Development.Shake.Rules
, Development.Shake.FilePattern
, copyFileChanged'
, getDirectoryFiles'
, getDirectoryFilesWithin'
, Development.Shake.need
, needPath
, needWithin
, readFile'
, writeFile'
) where

import qualified Development.Shake
import Development.Shake (Action, Rules, FilePattern)
import Path
import RIO
import qualified RIO.Text as T
import Shakebook.Within

copyFileChanged' :: Path Rel File -> Path Rel File -> Action ()
copyFileChanged' x y = Development.Shake.copyFileChanged (toFilePath x) (toFilePath y)

getDirectoryFiles' :: Path Rel Dir -> [FilePattern] -> Action [Path Rel File]
getDirectoryFiles' x y = traverse (liftIO . parseRelFile) =<< Development.Shake.getDirectoryFiles (toFilePath x) y

getDirectoryFilesWithin' :: Path Rel Dir -> [FilePattern] -> Action [Within Rel File]
getDirectoryFilesWithin' x pat = do
  xs <- getDirectoryFiles' x pat
  return ((`within` x) <$> xs)

needPath :: [Path Rel File] -> Action ()
needPath = Development.Shake.need . map toFilePath

needWithin :: [Within Rel File] -> Action ()
needWithin = needPath . map fromWithin

readFile' :: Path Rel File -> Action Text
readFile' = fmap T.pack . Development.Shake.readFile' . toFilePath

writeFile' :: Path Rel File -> Text -> Action ()
writeFile' x y = Development.Shake.writeFile' (toFilePath x) (T.unpack y)
