{-# LANGUAGE TemplateHaskell #-}
import FlagTH

data B = B deriving (Show)
data C = C deriving (Show)

flagTH [d|
    data A = A {
          b :: B,
          c :: C
        } deriving (Show)
 |]

asdf0 = A B C
asdf1 = AFlags (Just B) Nothing
asdf2 = AFlags Nothing (Just C)
asdf3 = combineAFlags asdf1 asdf2

main = do
  print asdf0
  print asdf1
  print asdf2
  print asdf3
  print (unAFlags asdf3)
