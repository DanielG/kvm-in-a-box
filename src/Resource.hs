{-# LANGUAGE ExistentialQuantification #-}
module Resource where

import Control.Monad
import Control.DeepSeq
import Control.Exception
import Control.Applicative
import Control.Arrow
import System.IO
import System.Directory
import System.Posix.Files
import System.Posix.Types
import System.Posix.User
import System.Process
import System.FilePath

import Log
import Utils
import Config

data Resource =
    FileResource {
      rPath      :: FilePath,
      rNormalize :: String -> String,
      rContent   :: String -> String
    }
  | MultiFileResource {
      rNormalize   :: String -> String,
      rFileContent :: [(FilePath, [(FilePath, String)] -> String -> String )]
    }
  | DirectoryResource {
      rPath      :: FilePath,
      rOwner     :: String,
      rGroup     :: String
    }
  | SymlinkResource {
      rPath      :: FilePath,
      rTarget    :: FilePath
    }
  | forall a. Show a => IOResource {
      rCheck       :: IO a,
      rUpdateMsg   :: a -> String,
      rNeedsUpdate :: a -> Bool,
      rUpdate      :: a -> IO ()
    }
  | ManyResources {
      rOthers :: [Resource]
    }

ensureResource root (DirectoryResource (rootRel root -> path) owner grp) = do
  e <- doesDirectoryExist path
  uid <- userID <$> getUserEntryForName owner
  gid <- groupID <$> getGroupEntryForName grp
  if e
     then do
       st <- getFileStatus path
       let chg = or [ fileOwner st /= uid
                    , fileGroup st /= gid
                    ]
       when chg $ do
            klog $ "directory '"++path++"' metadata changed, setting."
            create uid gid

     else do
       klog $ "directory '"++path++"' missing, creating."
       create uid gid

 where
   create uid gid = do
       createDirectoryIfMissing True path
       setOwnerAndGroup path uid gid

ensureResource root (FileResource path norm cf) = do
  ensureResource root $ MultiFileResource norm [(path, \_ -> cf)]

ensureResource r (MultiFileResource norm (map (first (rootRel r)) -> fs)) = do
  let paths = map fst fs
  mapM_ (createDirectoryIfMissing True . takeDirectory)  paths
  mfs <- mapM readFileMaybe paths
  let ctx = [ (p, f) | (p, Just f) <- paths `zip` mfs ]

  forM_ (fs `zip` mfs) $ \((path,(cf :: [(FilePath, String)] -> String -> String)), mf) ->
    case mf of
      Nothing -> do
              klog $ "resource '"++path++"' missing, creating."
              writeFile' path (cf ctx "")
      Just (force -> f) | norm f /= norm (cf ctx f) -> do
              klog $ "resource '"++path++"' changed, rewriting."
              writeFile' path (cf ctx f)
      _ -> return ()



-- ensureResource root SymlinkResource {..} = do
--   e <- linkExists rPath
--   if not e
--      then createSymbolicLink rPath rTarget
--      else do
--        same <- liftM2 (/=) (canonicalizePath rPath) (canonicalizePath rTarget)
--        if same
--          then return ()
--          else do
--            klog $ "resource '"++rPath++"' points to wrong target, recreating link."
--            removeFile rPath
--            createSymbolicLink rPath rTarget

ensureResource _ IOResource {..} = do
  a <- rCheck
  when (rNeedsUpdate a) $ do
    klog $ rUpdateMsg a
    rUpdate a

ensureResource root (ManyResources rs) = mapM_ (ensureResource root) rs

linkExists p =
  flip catch (\(SomeException _) -> return False) $ do
    getSymbolicLinkStatus p
    return True

readFileMaybe p = do
  e <- doesFileExist p
  if e
     then Just <$> readFile p
     else return Nothing
