{-# LANGUAGE OverloadedStrings #-}
module Config where

import Safe
import Control.Applicative
import Data.Char
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
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
    else do
      (_hdr:state:_) <- LBS8.lines <$> LBS.readFile file
      return $ either (error . (++) "parsing state: ") id $ eitherDecode state

writeState :: Options -> State -> IO ()
writeState opts s = do
    let f = rootRel (oRoot opts) stateFile
    createDirectoryIfMissing True (takeDirectory f)
    writeFile'LBS f $ LBS.concat
      [ "kvm-in-a-box state format: v3\n", encode s, "\n" ]

modifyState opts f = do
  s <- readState opts
  s' <- f s
  writeState opts s'
