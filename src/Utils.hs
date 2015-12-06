module Utils where

import Control.Monad
import Control.Exception
import Data.List
import Data.Char
import System.Exit
import System.Process hiding (callProcess)
import System.IO.Temp
import System.IO
import System.Directory
import System.FilePath
import System.Posix.User
import System.Posix.Files
import System.Posix.Types

pro (cmd:args) = do
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

writeFile' f c = writeFile'' f Nothing c

writeFile'' f mpe c = do
     withTempFile (takeDirectory f) (takeFileName f) $ \tf h -> do
         hPutStr h c
         hClose h
         amRoot <- amIRoot
         when amRoot $ maybe (return ()) (setPerms tf) mpe
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

amIRoot :: IO Bool
amIRoot = (==0) <$> getRealUserID

whenRoot :: IO () -> IO ()
whenRoot a = join $ when <$> amIRoot <*> pure a

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
