module Sysctl where

import Resource

sysctlResource :: Resource
sysctlResource = SimpleFileResource {
                   rPath      = "/etc/sysctl.d/50-kib.conf",
                   rPerms     = ((Nothing, Nothing), Just "644"),
                   rOwner     = OwnerKib,
                   rNormalize = id,
                   rContent   = unlines [
                                 "net.ipv4.ip_forward=1",
                                 "net.ipv6.conf.all.forwarding=1"
                                ]
                 }
