{- LANGUAGE CPP -}
module Files where

import System.FilePath

configFile = etcdir </> "kvm-in-a-box.cfg"

stateFile = varlibdir </> "state"

rootRel root path = root </> makeRelative "/" path

homedir = "/home"
varrundir = "/var/run/"
varlibdir = "/var/lib/kib/"
etcdir = "/etc"
usrsharedir = "/usr/share/kib"
