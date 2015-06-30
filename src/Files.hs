module Files where

import Control.Applicative
import Data.Char
import qualified Data.SConfig as SC
import System.Directory
import System.FilePath

import Utils
import Types
import Config
import Options
import IP

readConfig :: Options -> IO Config
readConfig opts = do
  m <- SC.readConfig <$> readFile (rootRel (oRoot opts) configFile)
  let Just cfg = Config <$> get "domain" m
                        <*> get "interface" m
                        <*> ( (,,) <$> (readIP <$> get "address" m)
                                   <*> (readNetmask <$> get "netmask" m)
                                   <*> (readIP <$> get "gateway" m)
                            )
  return cfg

 where
   get k mp = dropWhile isSpace <$> SC.getValue k mp

readState :: Options -> IO State
readState opts = do
  let file = rootRel (oRoot opts) stateFile
  e <- doesFileExist file
  if not e
    then return defState
    else read . head . drop 1 . lines <$> readFile file

writeState :: Options -> State -> IO ()
writeState opts s = writeFile' (rootRel (oRoot opts) stateFile) $
                   "kvm-in-a-box state format: v1\n" ++ show s

modifyState opts f = do
  s <- readState opts
  s' <- f s
  writeState opts s'
