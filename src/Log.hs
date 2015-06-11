module Log where

import System.IO

klog msg = hPutStrLn stderr msg
