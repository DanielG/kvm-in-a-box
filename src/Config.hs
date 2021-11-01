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

readConfig :: FilePath -> IO Config
readConfig root = do
  m <- SC.readConfig <$> readFile (rootRel root configFile)
  let Just cfg = Config <$> get "domain" m
                        <*> get "upstream-interface" m
                        <*> (readIPRange <$> get "public-address" m)
                        <*> (readIPRange <$> get "public-address6" m)
                        <*> (readIPRange <$> get "private-address6" m)
                        <*> (readIPRange <$> get "group-address6" m)
                        <*> pure (get "default-volume-group" m)
                        <*> pure (get "backdoor-ssh-key" m)
  return cfg

 where
   get k mp = dropWhile isSpace <$> SC.getValue k mp

readState :: FilePath -> IO State
readState root = do
  let file = rootRel root stateFile
  e <- doesFileExist file
  if not e
    then return defState
    else do
      (_hdr:state:_) <- LBS8.lines <$> LBS.readFile file
      return $ either (error . (++) "parsing state: ") id $ eitherDecode state

writeState :: FilePath -> State -> IO ()
writeState root s = do
    let f = rootRel root stateFile
    createDirectoryIfMissing True (takeDirectory f)
    writeFile''LBS f (Just ((Nothing, Nothing), Just "644")) $ LBS.concat
      [ "kvm-in-a-box state format: v4.1\n", encode s, "\n" ]

modifyState root f = do
  s <- readState root
  s' <- f s
  writeState root s'
