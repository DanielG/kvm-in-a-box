{-# LANGUAGE CPP #-}
module Utils where

import Control.Monad
import Control.Arrow
import Control.Applicative
import Control.Exception
import Data.Bool
import Data.List
import Data.Char
import Data.Either
import qualified Data.Foldable as F
import qualified Data.Traversable as F
import qualified Data.ByteString.Lazy as LBS
import Data.Bifunctor (Bifunctor, bimap)
import Data.Functor.Identity
import qualified Data.Map as Map
import System.Exit
import System.Process hiding (callProcess, system, rawSystem)
import System.IO.Temp
import System.IO
import System.Environment
import System.Directory
import System.FilePath
import System.Posix.User
import System.Posix.Files
import System.Posix.Types
import Prelude

system cmd = do
    (_,_,_,p) <- createProcess (shell cmd) {
#if MIN_VERSION_process(1,2,0)
                   delegate_ctlc = True,
#endif
                   close_fds = True
                 }
    waitForProcess p

rawSystem cmd args = do
    (_,_,_,p) <- createProcess (proc cmd args) {
#if MIN_VERSION_process(1,2,0)
                   delegate_ctlc = True,
#endif
                   close_fds = True
                 }
    waitForProcess p

pro (cmd:args) = do
  res <- callProcess Nothing cmd args
  case res of
    ExitSuccess -> return ()
    ExitFailure rv -> do
        hPutStrLn stderr $ "command failed '" ++ intercalate " " (map prettyShow $ cmd:args) ++ "' (exit code "++ show rv ++")"
        exitWith res

pro_ (cmd:args) = do
  res <- callProcess Nothing cmd args
  case res of
    ExitSuccess -> return ()
    ExitFailure rv ->
        hPutStrLn stderr $ "command failed '" ++ intercalate " " (map prettyShow $ cmd:args) ++ "' (exit code "++ show rv ++")"

prettyShow x | any isSpace x = show x
             | otherwise = x

callProcess :: Maybe FilePath -> FilePath -> [String] -> IO ExitCode
callProcess mwd exe args = do
  (_, _, _, h) <- createProcess (proc exe args) { cwd = mwd }
  waitForProcess h

writeFile'LBS f c = writeFile''LBS f Nothing c

writeFile''LBS f mpe c = do
     withTempFile (takeDirectory f) (takeFileName f) $ \tf h -> do
         LBS.hPutStr h c
         hClose h
         unlessTesting $ maybe (return ()) (setPerms tf) mpe
         renameFile tf f


writeFile' f c = writeFile'' f Nothing c

writeFile'' f mpe c = do
     withTempFile (takeDirectory f) (takeFileName f) $ \tf h -> do
         hPutStr h c
         hClose h
         unlessTesting $ maybe (return ()) (setPerms tf) mpe
         renameFile tf f

setPerms path (uidgid, mmask) = do
  let midmod =
        case uidgid of
          (Just uid, Just gid) -> Just $ uid ++ ":" ++ gid
          (Just uid, Nothing) -> Just $ uid
          (Nothing, Just gid) -> Just $ ":" ++ gid
          (Nothing, Nothing) -> Nothing

  maybe (return ()) (\mask -> void $ rawSystem "chmod" [mask, path]) mmask
  maybe (return ()) (\idmod -> void $ rawSystem "chown" [idmod, path]) midmod

amNotTesting :: IO Bool
amNotTesting =
  (`elem` [Nothing, Just ""]) <$> lookupEnv "KIB_TESTING"

whenTesting, unlessTesting :: IO () -> IO ()
whenTesting a = do
  nt <- amNotTesting
  when (not nt) a
unlessTesting a = do
  nt <- amNotTesting
  when nt a

linkExists p =
  flip catch (\(SomeException _) -> return False) $ do
    getSymbolicLinkStatus p
    return True

readFileMaybe p = do
  e <- doesFileExist p
  if e
     then Just <$> readFile p
     else return Nothing



fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x

flip2fst  :: (a -> b -> c -> d) -> (b -> a -> c -> d)
flip2snd  :: (a -> b -> c -> d) -> (a -> c -> b -> d)
flip2both :: (a -> b -> c -> d) -> (c -> b -> a -> d)

flip2fst  f b a c = f a b c
flip2snd  f a c b = f a b c
flip2both f c b a = f a b c

fst4 :: (a, b, c, d) -> a
snd4 :: (a, b, c, d) -> b
thd4 :: (a, b, c, d) -> c
fth4 :: (a, b, c, d) -> d

fst4 (a, b, c, d) = a
snd4 (a, b, c, d) = b
thd4 (a, b, c, d) = c
fth4 (a, b, c, d) = d

class IxFunctor i f | f -> i where
    imap :: (i -> a -> b) -> f a -> f b

class IxFoldable i t | t -> i where
    ifoldr :: (i -> a -> b -> b) -> b -> t a -> b

class IxFoldable i t => IxTraversable i t | t -> i where
    itraverse :: Applicative f => (i -> a -> f b) -> t a -> f (t b)

instance IxFunctor k (Map.Map k) where
    imap = Map.mapWithKey

instance IxFoldable k (Map.Map k) where
    ifoldr = Map.foldrWithKey

-- test_stuff = do
--   quickCheckWith args $ \(x :: T) ->
--       uncurry unmpartition (mpartition x) == x

--   quickCheckWith args $ \(x :: T) (ys :: [Int]) -> let
--         (mas, bs) = mpartition x
--       in
--         unmpartition mas (bs ++ ys) == x ++ map Right ys

--   quickCheckWith args $ \(xss :: AList Int (NonEmptyList (Int, Int))) -> let
--         xss' = nubBy ((==) `on` fst) $ map (second getNonEmpty) xss
--       in
--         curryAList (uncurryAList xss') == xss'


-- [Left 1, Left 2, Right 3, Left 4, Right 5, Right 6]
-- ->
-- [Just 1, Just 2, Nothing, Just 4, Nothing, Nothing]
-- [3, 5, 6]

prop_mpartition_append :: [Either Int Int] -> [Int] -> Bool
prop_mpartition_append xs ys =
    let (mas, bs) = mpartition xs
    in unmpartition mas (bs ++ ys) == xs ++ map Right ys

mpartition :: [Either a b] -> ([Maybe a], [b])
mpartition = map (either Just (const Nothing)) &&& rights

unmpartition :: [Maybe a] -> [b] -> [Either a b]
unmpartition (Just a  : mas)    bs  = Left  a : unmpartition mas bs
unmpartition (Nothing : mas) (b:bs) = Right b : unmpartition mas bs
unmpartition [] bs = map Right bs
unmpartition _ []  = []


type AList k v = [(k, v)]

uncurryAList :: AList k (AList kk v) -> AList (k, kk) v
uncurryAList ass = assert (all (not . null . snd) ass) $
    concat $ map (\(k,as) -> map (first (k,)) as ) ass

curryAList :: Eq k => AList (k, kk) v -> AList k (AList kk v)
curryAList ass = map (second (map (first snd))) $ groupByK (fst . fst) (==) ass

prop_curryiso :: AList (Int, Int) Int -> Bool
prop_curryiso xs = uncurryAList (curryAList xs) == xs

prop_uncurryiso :: AList Int (AList Int Int) -> Bool
prop_uncurryiso xs = curryAList (uncurryAList xs) == xs

groupByK :: (a -> b) -> (b -> b -> Bool) -> [a] -> [(b, [a])]
groupByK _ _  [] = []
groupByK f eq (x:xs) =
    (f x, (x:ys)) : groupByK f eq zs
 where (ys,zs) = span (eq (f x) . f) xs

alldifferent :: Eq a => [a] -> Bool
alldifferent (x:xs) = all (/=x) xs && alldifferent xs
alldifferent [] = True

prop_alldifferent :: [Int] -> Bool
prop_alldifferent xs = alldifferent (uniq xs)
 where uniq = map head . group . sort

unionAList :: Eq k => AList k v -> AList k v -> AList k v
unionAList xs ys = checkKeysAList $ xs ++ ys

unionsAList :: Eq k => [AList k v] -> AList k v
unionsAList = checkKeysAList . concat

unionAListWithKey :: Eq k => (k -> v -> v -> v) -> AList k v -> AList k v -> AList k v
unionAListWithKey f (x@(k,v):xs) ys
    | Just v' <- lookup k ys =
      (k, f k v v') : unionAListWithKey f xs (filter ((/=k) . fst) ys)
    | otherwise = x : unionAListWithKey f xs ys
unionAListWithKey f [] ys = ys

unionAListWith  :: Eq k => (v -> v -> v) -> AList k v -> AList k v -> AList k v
unionAListWith f = unionAListWithKey (const f)

unionsAListWithKey :: Eq k => (k -> v -> v -> v) -> [AList k v] -> AList k v
unionsAListWithKey f = foldr (unionAListWithKey f) []

unionsAListWith :: Eq k => (v -> v -> v) -> [AList k v] -> AList k v
unionsAListWith f = foldr (unionAListWith f) []

intersectionAListWith :: Eq k => (v -> v -> v) -> AList k v -> AList k v -> AList k v
intersectionAListWith f xs ys = [ (x, f xv yv)
                                | (x, xv) <- xs
                                , (y, yv) <- ys
                                , x == y
                                ]

-- unionsAListWith :: Eq k => (v -> v -> v) -> [AList k v] -> AList k v
-- unionsAListWith f xss = foldr (unionAListWith f) [] xss

checkKeysAList :: Eq a => [(a, b)] -> [(a, b)]
checkKeysAList als = assert (alldifferent $ map fst als) als

singletonAList :: k -> v -> AList k v
singletonAList k v = (:[]) (k,v)

both :: Bifunctor p => (c -> d) -> p c c -> p d d
both f = bimap f f
