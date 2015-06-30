module Utils where

import System.Exit
import System.Process hiding (callProcess)
import System.IO.Temp
import System.IO
import System.Directory
import System.FilePath

pro (cmd:args) = callProcess Nothing cmd args

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
