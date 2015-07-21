{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}
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
import Types

data ResourceOwner =
    OwnerVm VmName
  | OwnerKib
  | OwnerSystem

type Owned a = (ResourceOwner, a)

data Resource =
     SimpleFileResource {
      rPath      :: FilePath,
      rOwner     :: ResourceOwner,
      rNormalize :: String -> String,
      rContent   :: String
    }

  | forall a. FileResource {
      rPath        :: FilePath,
      rNormalize   :: String -> String,
      rParse       :: String -> [Owned a],
      rUnparse     :: [a] -> String,
      rContentFunc :: [Owned a] -> [Owned a]
    }

  | forall a. MultiFileResource {
      rNormalize   :: String -> String,
      rParse       :: String -> [(ResourceOwner, a)],
      rUnparse     :: [a] -> String,
      rFileContent :: [
       ( FilePath,
         [(FilePath, String)] -> [Owned a] -> [Owned a]
       )
      ]
    }

  | DirectoryResource {
      rPath      :: FilePath,
      rPerms     :: (String, String),
      rOwner     :: ResourceOwner
    }

  | SymlinkResource {
      rPath      :: FilePath,
      rTarget    :: FilePath,
      rOwner     :: ResourceOwner
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

ensureResource root (DirectoryResource (rootRel root -> path) (owner, grp) _row) = do
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

ensureResource root (FileResource path norm parse unparse content) = do
  ensureResource root $
    MultiFileResource norm parse unparse [(path, const content)]

ensureResource r (MultiFileResource {..}) = do
  let norm  = rNormalize
  let fs    = map (first $ rootRel r) rFileContent
      paths = map fst fs

  mapM_ (createDirectoryIfMissing True . takeDirectory) paths
  mfs <- mapM readFileMaybe paths
  let ctx = [ (p, f) | (p, Just f) <- paths `zip` mfs ]

  forM_ (fs `zip` mfs) $ \((path,cf), mf) ->
    case mf of
      Nothing -> do
              klog $ "resource '"++path++"' missing, creating."
              writeFile' path $ rUnparse $ map snd $ cf ctx []
      Just (force -> f) | norm f /= norm (rUnparse $ map snd $ cf ctx $ rParse f) -> do
              klog $ "resource '"++path++"' changed, rewriting."
              writeFile' path $ rUnparse $ map snd $ cf ctx $ rParse f
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
