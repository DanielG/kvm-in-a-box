module Console where

import Network.Socket hiding (sendAll, recv)
import Network.Socket.ByteString
import Data.ByteString
import System.IO

console f = do
  s <- socket AF_UNIX PF_DGRAM defaultProtocol
  connect s $ SockAddrUnix f

  hSetBuffering stdin NoBuffering

  forkIO $
    forever $ do
      d <- recv s 1024
      putStr d

  forkIO $
    forever $ do
      bs <- hGetSome
      sendAll s bs
      send s [c]
