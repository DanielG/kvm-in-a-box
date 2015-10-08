{-# LANGUAGE TemplateHaskell, ExistentialQuantification, RankNTypes, TypeFamilies #-}
module Resource where

import Control.Monad
import Control.DeepSeq
import Control.Exception
import Control.Applicative
import Control.Arrow
import Data.List
import Data.Proxy
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
    deriving (Eq, Show)

isOwnerVm (OwnerVm _) = True
isOwnerVm _ = False

class FromOwned a where
    type Owned a
    owners :: Proxy a -> Owned a -> [ResourceOwner]
    disown :: Owned a -> a

instance FromOwned [a] where
    type Owned [a] = [(ResourceOwner, a)]
    owners _ = map fst
    disown = map snd

data Resource =
     SimpleFileResource {
      rPath      :: FilePath,
      rOwner     :: ResourceOwner,
      rNormalize :: String -> String,
      rContent   :: String
    }

  | forall a. FromOwned a => FileResource {
      rPath        :: FilePath,
      rNormalize   :: String -> String,
      rParse       :: String -> Owned a,
      rUnparse     :: a -> String,
      rContentFunc :: Maybe (Owned a) -> Owned a
    }

  | forall a. FromOwned a => MultiFileResource {
      rNormalize    :: String -> String,
      rParse        :: String -> Owned a,
      rUnparse      :: a -> String,
      rPrereqs      :: [FilePath],
      rPaths        :: [FilePath],
      rContentFuncs :: [ [(FilePath, String)] -> Maybe (Owned a) -> Owned a ]
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

isIOResource IOResource {} = True
isIOResource _ = False

ensureResource root (DirectoryResource (rootRel root -> path) (owner, grp) _row) = do
  e <- doesDirectoryExist path
  if e
     then whenRoot $ do
       uid <- getUid
       gid <- getGid
       st <- getFileStatus path
       let chg = or [ fileOwner st /= uid
                    , fileGroup st /= gid
                    ]

       when chg $ do
            klog $ "directory '"++path++"' metadata changed, setting."
            createDirectoryIfMissing True path
            setOwnerAndGroup path uid gid

     else do
       klog $ "directory '"++path++"' missing, creating."
       createDirectoryIfMissing True path
       whenRoot $ join $ setOwnerAndGroup path <$> getUid <*> getGid

 where
   getUid = userID <$> getUserEntryForName owner
   getGid = groupID <$> getGroupEntryForName grp

ensureResource root (SimpleFileResource path owner norm content) = do
  ensureResource root $
    FileResource path norm (return . (,) owner) head (const [(owner, content)])

ensureResource root (FileResource path norm parse unparse content) = do
  ensureResource root $
    MultiFileResource norm parse unparse [] [path] [const content]

ensureResource r res@MultiFileResource {rNormalize=norm} =
  void $ withMultiFileResource r res $ \ctx parse unparse cf path mf ->
    case mf of
      Nothing -> do
              klog $ "resource '"++path++"' missing, creating."
              writeFile' path $ unparse $ disown $ cf ctx Nothing
      Just (force -> f) | norm f /= norm (unparse $ disown $ cf ctx $ Just $ parse f) -> do
              klog $ "resource '"++path++"' changed, rewriting."
              writeFile' path $ unparse $ disown $ cf ctx $ Just (parse f)
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


resourcePaths (SimpleFileResource {rPath}) = [rPath]
resourcePaths (FileResource {rPath}) = [rPath]
resourcePaths (MultiFileResource {rPaths}) = rPaths
resourcePaths (DirectoryResource {rPath}) = ["dir:" ++ rPath]
resourcePaths (SymlinkResource {rPath}) = ["sym:" ++ rPath]
resourcePaths (IOResource {}) = ["<IO resource>"]
resourcePaths (ManyResources rs) = concatMap resourcePaths rs

resourceOwners :: FilePath -> Resource -> IO [ResourceOwner]
resourceOwners r (SimpleFileResource path owner norm content) = do
  resourceOwners r $
    FileResource path norm (return . (,) owner) head (const [(owner, content)])

resourceOwners r (FileResource path norm parse unparse content) = do
  resourceOwners r $
    MultiFileResource norm parse unparse [] [path] [const content]

resourceOwners r MultiFileResource{rUnparse = (rUnparse :: a -> String), ..} = do
  let paths = map (rootRel r) rPaths
  mfs <- mapM readFileMaybe paths
  let ctx = [ (p, f) | (p, Just f) <- paths `zip` mfs ]

  return $ do
    (cf, p, mf) <- zip3 rContentFuncs paths mfs

    case mf of
      Nothing -> []
      Just f -> let
                x = cf ctx $ Just $ rParse f
           in owners (Proxy :: Proxy a) x

resourceOwners r DirectoryResource {rOwner} = return [rOwner]
resourceOwners r SymlinkResource {rOwner} = return [rOwner]
resourceOwners r IOResource {} = return []
resourceOwners r (ManyResources rs) = concat <$> mapM (resourceOwners r) rs

withMultiFileResource :: FilePath -> Resource ->

                         (forall a. FromOwned a =>
                            [(FilePath, String)]
                         -> (String -> Owned a)
                         -> (a -> String)
                         -> ([(FilePath, String)] -> Maybe (Owned a) -> Owned a)
                         -> FilePath
                         -> Maybe String
                         -> IO b)

 -> IO [b]
withMultiFileResource r MultiFileResource{..} fn = do
  let paths = map (rootRel r) rPaths

  mapM_ (createDirectoryIfMissing True . takeDirectory) paths
  mfs <- mapM readFileMaybe paths
  let ctx = [ (p, f) | (p, Just f) <- rPaths `zip` mfs ]

  sequence $ zipWith5 (fn ctx) (repeat rParse) (repeat rUnparse) rContentFuncs paths mfs

linkExists p =
  flip catch (\(SomeException _) -> return False) $ do
    getSymbolicLinkStatus p
    return True

readFileMaybe p = do
  e <- doesFileExist p
  if e
     then Just <$> readFile p
     else return Nothing
