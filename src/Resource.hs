{-# LANGUAGE ExistentialQuantification #-}
module Resource where

import Control.Monad
import System.Directory
import Log

data Resource =
    FileResource {
      rNormalize :: String -> String,
      rPath      :: FilePath,
      rContent   :: String
    }
  | forall a. Show a => IOResource {
      rCheck       :: IO a,
      rNeedsUpdate :: a -> Bool,
      rUpdate      :: a -> IO ()
    }
  | ManyResources {
      rOthers :: [Resource]
    }

ensureResource :: Resource -> IO ()
ensureResource FileResource {..} = do
  mf <- readFileMaybe rPath
  case mf of
    Nothing -> do
            klog $ "resource "++rPath++" missing"
            write
    Just f | rNormalize f /= rNormalize rContent -> do
            klog $ "resource "++rPath++" changed"
            write
    _ -> return ()
 where
   write = writeFile rPath rContent

ensureResource IOResource {..} = do
  a <- rCheck
  when (rNeedsUpdate a) $ rUpdate a

ensureResource (ManyResources rs) = mapM_ ensureResource rs

readFileMaybe p = do
  e <- doesFileExist p
  if e
     then Just <$> readFile p
     else return Nothing
