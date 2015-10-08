module Main where

import Iptables

main = do
  x <- filterKib . parse <$> getContents
  print x

  putStrLn "------"

  putStr $ unparse x
