module Systemd (vmInitResource) where

import Data.List
import Data.Monoid
import System.FilePath
import System.Directory

import Resource
import Files
import Types

vmInitResource :: VmName -> [String] -> Resource
vmInitResource vmn (cmd:args) = do
  SimpleFileResource {
    rPath = etcdir </> "systemd/system/kib-" <> vmn <.> "service",
    rOwner = OwnerVm vmn,
    rNormalize = id,
    rContent = service vmn $ "/usr/bin/env":"kib-supervise":("kib-"++vmn):cmd:args
 }

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
