module Utils where

import System.Process

pro (cmd:args) = callProcess cmd args

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x
