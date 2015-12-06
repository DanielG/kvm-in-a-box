{-# LANGUAGE TemplateHaskell, ExistentialQuantification, RankNTypes, TypeFamilies #-}
module Resource where

import Control.Monad
import Control.DeepSeq
import Control.Exception
import Control.Applicative
import Control.Arrow
import Data.List
import Data.Proxy
import Data.Maybe
import System.IO
import System.Directory
import System.Posix.Files
import System.Posix.Types
import System.Posix.User
import System.Process
import System.FilePath

import Log
import Utils
import Files
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

type UidGid = (Maybe String, Maybe String)
type FilePermMask = String
type FilePerms = (UidGid, Maybe FilePermMask)

defaultFilePerms = ((Nothing, Nothing), Nothing)

data Resource =
     SimpleFileResource {
      rPath      :: FilePath,
      rPerms     :: FilePerms,
      rOwner     :: ResourceOwner,
      rNormalize :: String -> String,
      rContent   :: String
    }

  | forall a. FromOwned a => FileResource {
      rPath        :: FilePath,
      rPerms       :: FilePerms,
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
      rPaths        :: [(FilePath, FilePerms)],
      rContentFuncs :: [ [(FilePath, String)] -> Maybe (Owned a) -> Owned a ]
    }

  | DirectoryResource {
      rPath      :: FilePath,
      rPerms     :: FilePerms,
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
      rUpdate      :: a -> IO ()
    }
  | ManyResources {
      rOthers :: [Resource]
    }

isIOResource IOResource {} = True
isIOResource _ = False

ensureResource root (DirectoryResource (rootRel root -> path) perms _row) = do
  e <- doesDirectoryExist path
  if e
     then do
       whenRoot $ setPerms path perms


     -- then whenRoot $ do
     --   uid <- getUid
     --   gid <- getGid
     --   st <- getFileStatus path
     --   let chg = or [ fileOwner st /= uid
     --                , fileGroup st /= gid
     --                ]

     --   when chg $ do
     --        klog $ "directory '"++path++"' metadata changed, setting."
     --        setPerms path perms

     else do
       klog $ "directory '"++path++"' missing, creating."
       createDirectoryIfMissing True path
       whenRoot $ setPerms path perms

 -- where
 --   getUid = userID <$> getUserEntryForName (fromMaybe "root" owner)
 --   getGid = groupID <$> getGroupEntryForName (fromMaybe "root" grp)

ensureResource root (SimpleFileResource path perms owner norm content) = do
  ensureResource root $
    FileResource path perms norm (return . (,) owner) head (const [(owner, content)])

ensureResource root (FileResource path perms norm parse unparse content) = do
  ensureResource root $
    MultiFileResource norm parse unparse [] [(path, perms)] [const content]

ensureResource r res@MultiFileResource {rNormalize=norm} =
  void $ withMultiFileResource r res $ \ctx parse unparse cf (path, perms) mf ->
    case mf of
      Nothing -> do
              klog $ "resource '"++path++"' missing, creating."
              writeFile'' path (Just perms) $ unparse $ disown $ cf ctx Nothing
      Just (force -> f) | norm f /= norm (unparse $ disown $ cf ctx $ Just $ parse f) -> do
              klog $ "resource '"++path++"' changed, rewriting."
              writeFile'' path (Just perms) $ unparse $ disown $ cf ctx $ Just (parse f)
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
  klog $ rUpdateMsg a
  rUpdate a

ensureResource root (ManyResources rs) = mapM_ (ensureResource root) rs


resourcePaths (SimpleFileResource {rPath}) = [rPath]
resourcePaths (FileResource {rPath}) = [rPath]
resourcePaths (MultiFileResource {rPaths}) = map fst rPaths
resourcePaths (DirectoryResource {rPath}) = ["dir:" ++ rPath]
resourcePaths (SymlinkResource {rPath}) = ["sym:" ++ rPath]
resourcePaths (IOResource {}) = ["<IO resource>"]
resourcePaths (ManyResources rs) = concatMap resourcePaths rs

resourceOwners :: FilePath -> Resource -> IO [ResourceOwner]
resourceOwners r (SimpleFileResource path perms owner norm content) = do
  resourceOwners r $
    FileResource path perms norm (return . (,) owner) head (const [(owner, content)])

resourceOwners r (FileResource path perms norm parse unparse content) = do
  resourceOwners r $
    MultiFileResource norm parse unparse [] [(path, perms)] [const content]

resourceOwners r MultiFileResource{rUnparse = (rUnparse :: a -> String), ..} = do
  let paths = map (rootRel r . fst) rPaths
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
                         -> (FilePath, FilePerms)
                         -> Maybe String
                         -> IO b)

 -> IO [b]
withMultiFileResource r MultiFileResource{..} fn = do
  let
      pathPerms = map (first $ rootRel r) rPaths
      paths = map fst pathPerms

  mapM_ (createDirectoryIfMissing True . takeDirectory) $ paths
  mfs <- mapM readFileMaybe paths
  let ctx = [ (p, f) | (p, Just f) <- map fst rPaths `zip` mfs ]

  sequence $ zipWith5 (fn ctx) (repeat rParse) (repeat rUnparse) rContentFuncs pathPerms mfs
