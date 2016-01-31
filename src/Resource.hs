{-# LANGUAGE TemplateHaskell, ExistentialQuantification, RankNTypes, TypeFamilies, DeriveDataTypeable #-}
module Resource where

import Control.Monad
import Control.DeepSeq
import Control.Exception
import Control.Applicative
import Control.Arrow
import Data.List
import Data.Maybe
import Data.Typeable hiding (Proxy)
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

data Proxy t = Proxy

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
    filterOwner :: Proxy a -> (ResourceOwner -> Bool) -> Owned a -> Owned a
    disown :: Owned a -> a

instance FromOwned [a] where
    type Owned [a] = [(ResourceOwner, a)]
    owners _ = map fst
    filterOwner _ p = filter (p . fst)
    disown = map snd

type UidGid = (Maybe String, Maybe String)
type FilePermMask = String
type FilePerms = (UidGid, Maybe FilePermMask)

defaultFilePerms = ((Nothing, Nothing), Nothing)

class ResourceC r where
    ensureResource :: FilePath -> r -> IO ()
    removeResource :: FilePath -> Maybe ResourceOwner -> r -> IO ()
    resourceOwners :: FilePath -> r -> IO [ResourceOwner]
    resourcePaths  :: r -> [FilePath]

data SimpleFileResource =
     SimpleFileResource {
      sfrPath      :: FilePath,
      sfrPerms     :: FilePerms,
      sfrOwner     :: ResourceOwner,
      sfrNormalize :: String -> String,
      sfrContent   :: String
    }

data FileResource a =
    FileResource {
      rPath        :: FilePath,
      rPerms       :: FilePerms,
      rNormalize   :: String -> String,
      rParse       :: String -> Owned a,
      rUnparse     :: a -> String,
      rContentFunc :: Maybe (Owned a) -> Owned a
    }

  | MultiFileResource {
      rNormalize    :: String -> String,
      rParse        :: String -> Owned a,
      rUnparse      :: a -> String,
      rPrereqs      :: [FilePath],
      rPaths        :: [(FilePath, FilePerms)],
      rContentFuncs :: [ [(FilePath, String)] -> Maybe (Owned a) -> Owned a ]
    }

data FileMetaResource =
    DirectoryResource {
      fmrPath      :: FilePath,
      fmrPerms     :: FilePerms,
      fmrOwner     :: ResourceOwner
    }

  | SymlinkResource {
      fmrPath      :: FilePath,
      fmrTarget    :: FilePath,
      fmrOwner     :: ResourceOwner
    }

data IOResource a = IOResource {
      rCheck       :: IO a,
      rUpdateMsg   :: a -> String,
      rUpdate      :: a -> IO ()
    }

data ManyResources = ManyResources {
      rOthers :: [SomeResource]
    }

data SomeResource = forall r. ResourceC r => SomeResource r
  deriving (Typeable)

instance ResourceC SimpleFileResource where
  ensureResource root (SimpleFileResource path perms owner norm content) = do
    ensureResource root $
      FileResource path perms norm (return . (,) owner) head (const [(owner, content)])

  removeResource root mo SimpleFileResource {sfrPath, sfrOwner} =
    case mo of
      Nothing -> removeFile (rootRel root sfrPath)
      Just owner -> when (owner == sfrOwner) $ removeFile (rootRel root sfrPath)

  resourceOwners r (SimpleFileResource path perms owner norm content) = do
    resourceOwners r $
      FileResource path perms norm (return . (,) owner) head (const [(owner, content)])

  resourcePaths (SimpleFileResource {sfrPath}) = [sfrPath]


instance FromOwned a => ResourceC (FileResource a) where
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

  removeResource root (Just owner) (MultiFileResource {rUnparse = (rUnparse :: a -> String), ..}) = do
      let ps = map fst rPaths
      forM_ ps $ \p -> do
        f <- rParse <$> readFile (rootRel root p)
        writeFile' (rootRel root p) $ rUnparse $ disown $ filterOwner (Proxy :: Proxy a) (==owner) f
  removeResource root (Just owner) (FileResource {..}) = do
      f <- rParse <$> readFile (rootRel root rPath)
      writeFile' (rootRel root rPath) $ rUnparse $ disown $ filterOwner (Proxy :: Proxy a) (==owner) f
  removeResource root Nothing res =
      forM_ paths $ \p -> removeFile (rootRel root p)
    where
      paths | MultiFileResource {..} <- res = map fst rPaths
            | FileResource {..} <- res = [rPath]

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

  resourcePaths (FileResource {rPath}) = [rPath]
  resourcePaths (MultiFileResource {rPaths}) = map fst rPaths


instance ResourceC FileMetaResource where
  ensureResource root (DirectoryResource (rootRel root -> path) perms _row) = do
    e <- doesDirectoryExist path
    if e
       then do
         unlessTesting $ setPerms path perms
       else do
         klog $ "directory '"++path++"' missing, creating."
         createDirectoryIfMissing True path
         unlessTesting $ setPerms path perms

  ensureResource root SymlinkResource {..} = do
    e <- linkExists fmrPath
    if not e
       then createSymbolicLink fmrPath fmrTarget
       else do
         same <- liftM2 (/=) (canonicalizePath fmrPath) (canonicalizePath fmrTarget)
         if same
           then return ()
           else do
             klog $ "resource '"++fmrPath++"' points to wrong target, recreating link."
             removeFile fmrPath
             createSymbolicLink fmrPath fmrTarget

  removeResource root mo (DirectoryResource (rootRel root -> path) _ owner)
      | mo == Just owner = removeDirectoryRecursive path
      | otherwise = return ()

  removeResource root mo SymlinkResource {..}
      | mo == Just fmrOwner = removeLink fmrPath
      | otherwise = return ()


  resourceOwners r DirectoryResource {fmrOwner} = return [fmrOwner]
  resourceOwners r SymlinkResource {fmrOwner} = return [fmrOwner]

  resourcePaths (DirectoryResource {fmrPath}) = ["dir:" ++ fmrPath]
  resourcePaths (SymlinkResource {fmrPath}) = ["sym:" ++ fmrPath]


instance ResourceC (IOResource a) where
  ensureResource _ IOResource {..} = unlessTesting $ do
    a <- rCheck
    klog $ rUpdateMsg a
    rUpdate a

  removeResource _ _ _ = return ()

  resourceOwners _ _ = return []
  resourcePaths (IOResource {}) = ["<IO resource>"]

instance ResourceC ManyResources where
  ensureResource root (ManyResources rs) =
      mapM_ (ensureResource root) rs
  removeResource root mo (ManyResources rs) =
      mapM_ (removeResource root mo) rs
  resourceOwners r (ManyResources rs) =
      concat <$> mapM (resourceOwners r) rs
  resourcePaths (ManyResources rs) = concatMap resourcePaths rs

instance ResourceC SomeResource where
  ensureResource root (SomeResource r) = ensureResource root r
  removeResource root mo (SomeResource r) = removeResource root mo r
  resourceOwners root (SomeResource r) = resourceOwners root r
  resourcePaths (SomeResource r) = resourcePaths r

withMultiFileResource :: FromOwned a
                      => FilePath
                      -> FileResource a
                      -> (  [(FilePath, String)]
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
