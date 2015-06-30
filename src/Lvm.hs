module Lvm where

import Utils

lvcreate mb vm vg =
    callProcess Nothing "lvcreate" [ "-L", mb ++ "M", "-n", vm, vg ]
