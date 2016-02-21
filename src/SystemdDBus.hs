{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module SystemdDBus where

import Data.Word
import Data.Maybe
import DBus
import DBus.Client

import System.Posix.User
import System.Environment
import System.FilePath

type SystemdUnit =
    ( String
    , String
    , String
    , String
    , String
    , String
    , ObjectPath
    , Word32
    , String
    , ObjectPath)

xdgRuntimeDir = do
  maybe guess return =<< lookupEnv "XDG_RUNTIME_DIR"
 where
   guess = do
     uid <- getRealUserID
     return $ "/run/user/"++show uid

userInstanceAddress :: IO Address
userInstanceAddress = do
  rundir <- xdgRuntimeDir
  let Just addr = parseAddress $ "unix:path=" ++ (rundir </> "systemd/private")
  return addr

listUnits :: Address -> IO [SystemdUnit]
listUnits addr = do
  c <- connect addr
  resp <- call_ c (methodCall "/org/freedesktop/systemd1" "org.freedesktop.systemd1.Manager" "ListUnits")
          { methodCallDestination = Just "org.freedesktop.systemd1"
          }
  return $ fromMaybe [] $ (fromVariant $ head $ methodReturnBody resp)

getUnitActiveState :: SystemdUnit -> String
getUnitActiveState (name,_,load_state,active_state,sub_state,_,_,_,_,_) =
    active_state

getUnitName :: SystemdUnit -> String
getUnitName (name,_,load_state,active_state,sub_state,_,_,_,_,_) =
    name
