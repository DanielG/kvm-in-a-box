{-# LANGUAGE OverloadedStrings #-}
import System.IO
import System.Exit
import System.Process
import System.Environment
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar

import Data.Map as Map
import Text.DeadSimpleJSON as Json hiding (True, False)
import Data.Monoid

slog msg = hPutStrLn stderr msg

main :: IO ()
main = do
  cmd <- getArgs
  case cmd of
    [] -> hPutStrLn stderr "Usage: kib-supervise qemu-syetem-ARCH [ARGS..]"
    _ -> supervise cmd

supervise :: [String] -> IO ()
supervise (cmd:args) = go 0
 where
   go i = do
     slog $ "Spawning '"<>unwords (cmd:args)

     (Just si, Just so, Just se, ph) <- createProcess cp

     shouldRespawnMv <- newEmptyMVar

     et <- forkIO $ errorReader se
     iot <- forkIO $ qmp si so shouldRespawnMv

     rv <- waitForProcess ph

     shouldRespawn <- isEmptyMVar shouldRespawnMv

     killThread et
     killThread iot

--     mapM_ hClose [si, so, se]

     let msg = "Process exited (rv = "<>show rv<>")"
     if rv == ExitSuccess || not shouldRespawn
       then
         slog $ msg <> ", done supervising"

       else do
         slog $ msg <> ", respawning"
         -- max 8 sec
         threadDelay ((2^i) * 1000) >> go (min 14 (i+1))

   errorReader se = swallow $
       forever $ hGetLine se >>= slog . (("stderr: ")<>)

   qmp si so mv = swallow $ do
     ehelo <- parseQmp <$> hGetLine so
     if ehelo /= Just Greeting
        then slog $ "Process returned invalid qmp greeting"
        else do
          hPutStrLn si "{ \"execute\": \"qmp_capabilities\" }"
          eres <- parseQmp <$> hGetLine so
          if eres /= Just Return
             then slog $ "Process returned invalid ack"
             else forever $ do
              eres <- parseQmp <$> hGetLine so
              case eres of
                Just (Event "SHUTDOWN") -> print "SHUTDOWN" >> putMVar mv ()
                _ -> return ()


   cp = CreateProcess {
          cmdspec = RawCommand cmd args,
          cwd = Just "/",
          env = Nothing,
          std_in = CreatePipe,
          std_out = CreatePipe,
          std_err = CreatePipe,
          close_fds = True,
          create_group = False,
          delegate_ctlc = False
        }

swallow a = catch a (\(SomeException _) -> return ())

data QMPRes = Greeting
            | Return
            | Event String
            | Other Value
              deriving (Eq)

parseQmp :: String -> Maybe QMPRes
parseQmp = fmap qmpResponse . Json.parse'

qmpResponse (Object m)
    | Just _ <- Map.lookup "QMP" m = Greeting
    | Just _ <- Map.lookup "return" m = Return
    | Just (String ev) <- Map.lookup "event" m = Event ev
    | otherwise = Other (Object m)
