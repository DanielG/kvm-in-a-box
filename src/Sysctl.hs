module Sysctl where

import Resource

sysctlResource :: SomeResource
sysctlResource = SomeResource $ SimpleFileResource {
  sfrPath      = "/etc/sysctl.d/50-kib.conf",
  sfrPerms     = ((Nothing, Nothing), Just "644"),
  sfrOwner     = OwnerKib,
  sfrNormalize = id,
  sfrContent   = unlines [
                  "net.ipv4.ip_forward=1",
                  "net.ipv6.conf.all.forwarding=1"
                 ]
  }
