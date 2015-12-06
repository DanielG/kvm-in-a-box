module Systemd (vmInitResource) where

import Data.List
import Data.Monoid
import System.FilePath
import System.Directory

import Resource
import Files
import Types

vmInitResource :: VmName -> [String] -> Resource
vmInitResource vmn (cmd:args) = ManyResources [
    SimpleFileResource {
    -- /etc/systemd/user
      rPath = etcdir </> "systemd/user/kib-" <> vmn <.> "service",
      rPerms = ((Nothing, Nothing), Just "644"),
      rOwner = OwnerVm vmn,
      rNormalize = id,
      rContent = service vmn $ "/usr/bin/env":"kib-supervise":("kib-"++vmn):cmd:args
   },
   SimpleFileResource {
      rPath = "/var/lib/systemd/linger" </> ("kib-" ++ vmn),
      rPerms = defaultFilePerms,
      rOwner = OwnerVm vmn,
      rNormalize = const "nonempty",
      rContent = ""
   }
  ]

service vmn (cmd:args) = "\
 \[Unit]\n\
 \Description=Kvm-in-a-box VM: "++vmn++"\n\
 \After=kvm-in-a-box.target\n\
 \\n\
 \[Service]\n\
 \User=kib-"++vmn++"\n\
 \ExecStart="++cmd++" "++(intercalate " " $ map (("'"++) . (++"'")) args)++"\n\
 \Restart=on-failure\n\
 \RuntimeDirectory=kib-"++vmn++"\n\
 \\n\
 \[Install]\n\
 \WantedBy=multi-user.target\n"
