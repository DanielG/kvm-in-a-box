{-# LANGUAGE OverloadedStrings #-}
import System.IO
import System.Exit
import System.Process
import System.Environment
import System.Directory
import System.Posix.User
import Control.Monad
import Control.Exception
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar

import Data.Monoid
import qualified Data.HashMap.Strict as HMap
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

slog msg = hPutStrLn stderr msg

main :: IO ()
main = do
  cmd <- getArgs
  case cmd of
    []   -> usage
    _:[] -> usage
    user:cmd -> supervise cmd
 where
   usage = do
       hPutStrLn stderr "Usage: kib-supervise USER qemu-syetem-ARCH [ARGS..]"
       exitWith $ ExitFailure 1

supervise :: [String] -> IO ()
supervise (cmd:args) = go 0
 where
   go i = do
     slog $ "Spawning '"<>unwords (map (('"':) . (++"\"")) $ (cmd:args))<>"'"

     (Just si, Just so, Nothing {- Just se -}, ph) <- createProcess cp

     shouldRespawnMv <- newEmptyMVar

--     et <- forkIO $ errorReader se
     iot <- forkIO $ qmp si so shouldRespawnMv

     rv <- waitForProcess ph

     shouldRespawn <- isEmptyMVar shouldRespawnMv

--     killThread et
     killThread iot

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
     case ehelo of
       Left err -> slog $ "Process returned invalid json: "++err
       Right Greeting -> do
         hPutStrLn si "{ \"execute\": \"qmp_capabilities\" }"
         eres <- parseQmp <$> hGetLine so
         case eres of
           Left err -> slog $ "Process returned invalid json: "++err
           Right Return -> forever $ do
              eres <- parseQmp <$> hGetLine so
              case eres of
                Right (Event "SHUTDOWN") -> print "SHUTDOWN" >> putMVar mv ()
                _ -> return ()
           Right _ -> slog $ "Process returned invalid ack"
       Right _ -> slog $ "Process returned invalid qmp greeting"


   cp = (proc cmd args) {
          cwd = Just "/",
          env = Nothing,
          std_in = CreatePipe,
          std_out = CreatePipe,
          std_err = Inherit
        }

swallow a = catch a (\(SomeException _) -> return ())

data QMPRes = Greeting
            | Return
            | Event String
            | Other Value
              deriving (Eq)

parseQmp :: String -> Either String QMPRes
parseQmp = fmap qmpResponse . eitherDecodeStrict . T.encodeUtf8 . T.pack

qmpResponse (Object (HMap.toList -> m))
    | Just _ <- lookup "QMP" m = Greeting
    | Just _ <- lookup "return" m = Return
    | Just (String ev) <- lookup "event" m = Event $ T.unpack ev
    | otherwise = Other (Object $ HMap.fromList m)
