module Systemd (vmInitResource) where

import Data.List
import Data.Monoid
import System.FilePath
import System.Directory

import Resource
import Files
import Types

vmInitResource :: VmName -> [String] -> ManyResources
vmInitResource vmn (cmd:args) = ManyResources [
    SomeResource $ SimpleFileResource {
    -- /etc/systemd/user
      sfrPath = etcdir </> "systemd/user/kib-" <> vmn <.> "service",
      sfrPerms = ((Nothing, Nothing), Just "644"),
      sfrOwner = OwnerVm vmn,
      sfrNormalize = id,
      sfrContent = service vmn $ "/usr/bin/env":"kib-supervise":("kib-"++vmn):cmd:args
   },
   SomeResource $ SimpleFileResource {
      sfrPath = "/var/lib/systemd/linger" </> ("kib-" ++ vmn),
      sfrPerms = defaultFilePerms,
      sfrOwner = OwnerVm vmn,
      sfrNormalize = const "nonempty",
      sfrContent = ""
   }
  ]

service vmn (cmd:args) = "\
 \[Unit]\n\
 \Description=Kvm-in-a-box VM: "++vmn++"\n\
 \After=kvm-in-a-box.target\n\
 \\n\
 \[Service]\n\
 \ExecStart="++cmd++" "++(intercalate " " $ map (("'"++) . (++"'")) args)++"\n\
 \Restart=on-failure\n\
 \RuntimeDirectory=kib-"++vmn++"\n\
 \StandardOutput=journal\n\
 \StandardError=journal\n\
 \\n\
 \[Install]\n\
 \WantedBy=default.target\n"

-- \User=kib-"++vmn++"\n\
