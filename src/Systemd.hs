module Systemd (vmInitResource) where

import Data.List
import Data.Monoid
import System.FilePath
import System.Directory

import Resource
import Files
import Types

vmInitResource :: VmName -> [String] -> ManyResources
vmInitResource vmn qemu_args = ManyResources [
    SomeResource $ SimpleFileResource {
    -- /etc/systemd/user
      sfrPath = etcdir </> "systemd/user/kib-" <> vmn <.> "service",
      sfrPerms = ((Nothing, Nothing), Just "644"),
      sfrOwner = OwnerVm vmn,
      sfrNormalize = id,
      sfrContent = runUnit vmn $ "/usr/sbin/kib-supervise":qemu_args
   },
    SomeResource $ SimpleFileResource {
      sfrPath = etcdir </> "systemd/user/kib-" <> vmn <> "-install@.service",
      sfrPerms = ((Nothing, Nothing), Just "644"),
      sfrOwner = OwnerVm vmn,
      sfrNormalize = id,
      sfrContent = installUnit vmn $ [ "/usr/sbin/kib-install", "%i" ]
   },
   SomeResource $ SimpleFileResource {
      sfrPath = "/var/lib/systemd/linger" </> ("kib-" ++ vmn),
      sfrPerms = defaultFilePerms,
      sfrOwner = OwnerVm vmn,
      sfrNormalize = const "nonempty",
      sfrContent = ""
   }
  ]
 where

   runUnit vmn (cmd:args) = "\
     \[Unit]\n\
     \Description=Kvm-in-a-box VM: "++vmn++"\n\
     \After=kvm-in-a-box.target\n\
     \\n\
     \[Service]\n\
     \ExecStart="++cmd++" "++(intercalate " " $ map (("'"++) . (++"'")) args)++"\n\
     \KillMode=mixed\n\
     \Restart=on-failure\n\
     \RuntimeDirectory=kib-"++vmn++"\n\
     \StandardOutput=journal\n\
     \StandardError=journal\n\
     \\n\
     \[Install]\n\
     \WantedBy=default.target\n"

   installUnit vmn (cmd:args) = "\
     \[Unit]\n\
     \Description=Kvm-in-a-box VM installation: "++vmn++"\n\
     \After=kvm-in-a-box.target\n\
     \Conflicts=kib-"++vmn++".service\n\
     \\n\
     \[Service]\n\
     \Type=simple\n\
     \ExecStart="++cmd++" "++(intercalate " " $ map (("'"++) . (++"'")) args)++"\n\
     \KillMode=mixed\n\
     \RuntimeDirectory=kib-"++vmn++"\n\
     \StandardOutput=journal\n\
     \StandardError=journal\n"
