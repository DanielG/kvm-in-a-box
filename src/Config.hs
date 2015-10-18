module Config where

import Safe
import Control.Applicative
import Data.Char
import qualified Data.SConfig as SC
import System.Directory
import System.FilePath

import Utils
import Types
import Files
import Options
import IP

readConfig :: Options -> IO Config
readConfig opts = do
  m <- SC.readConfig <$> readFile (rootRel (oRoot opts) configFile)
  let Just cfg = Config <$> get "domain" m
                        <*> get "upstream-interface" m
                        <*> (readIPRange <$> get "public-address" m)
                        <*> (readIPRange <$> get "public-address6" m)
                        <*> (readIPRange <$> get "private-address6" m)
                        <*> (readIPRange <$> get "group-address6" m)
  return cfg

 where
   get k mp = dropWhile isSpace <$> SC.getValue k mp

readState :: Options -> IO State
readState opts = do
  let file = rootRel (oRoot opts) stateFile
  e <- doesFileExist file
  if not e
    then return defState
    else readNote "state" . head . drop 1 . lines <$> readFile file

writeState :: Options -> State -> IO ()
writeState opts s = do
    let f = rootRel (oRoot opts) stateFile
    createDirectoryIfMissing True (takeDirectory f)
    writeFile' f $
      "kvm-in-a-box state format: v2\n" ++ show s ++ "\n"

modifyState opts f = do
  s <- readState opts
  s' <- f s
  writeState opts s'
