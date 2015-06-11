module Config where

import System.FilePath

configFile = etcdir </> "config"
stateFile = varlibdir </> "state"


-- varrundir = "/var/run/kib/"
varrundir = "/tmp/kib/run"

-- varlibdir = "/var/lib/kib/"
varlibdir = "/tmp/kib/lib"

-- etcdir = "/etc"
etcdir = "/tmp/kib/etc"
