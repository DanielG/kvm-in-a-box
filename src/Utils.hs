module Utils where

import Control.Monad
import Data.List
import Data.Char
import System.Exit
import System.Process hiding (callProcess)
import System.IO.Temp
import System.IO
import System.Directory
import System.FilePath
import System.Posix.User

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

writeFile' f c = do
     withTempFile (takeDirectory f) (takeFileName f) $ \tf h -> do
         hPutStr h c
         hClose h
         renameFile tf f

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x

amIRoot :: IO Bool
amIRoot = (==0) <$> getRealUserID

whenRoot :: IO () -> IO ()
whenRoot a = join $ when <$> amIRoot <*> pure a
