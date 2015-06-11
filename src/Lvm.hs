module Lvm where

import System.Process

lvcreate mb vm vg = callProcess "lvcreate" [ "-L", mb ++ "M", "-n", vm, vg ]
