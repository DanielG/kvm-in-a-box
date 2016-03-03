{-# LANGUAGE OverloadedStrings, TupleSections #-}
import System.IO
import System.Exit
import System.Process hiding (system, rawSystem)
import System.Environment
import System.Directory
import System.Posix.User
import System.Posix.Signals
import Control.Monad
import Control.Exception
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan

import Data.Monoid
import qualified Data.HashMap.Strict as HMap
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

slog msg = hPutStrLn stdout msg

main :: IO ()
main = do
  cmd <- getArgs
  case cmd of
    []   -> usage
    _:[] -> usage
    cmd -> supervise cmd
 where
   usage = do
       hPutStrLn stderr "Usage: kib-supervise qemu-syetem-ARCH [ARGS..]"
       exitWith $ ExitFailure 1

debug = True

supervise :: [String] -> IO ()
supervise (cmd:args) = do
  slog $ "Spawning '"<>unwords (map (('"':) . (++"\"")) $ (cmd:args))<>"'"

  ev_chan <- newChan
  cmd_chan <- newChan

  installHandler sigTERM (Catch $ writeChan cmd_chan "{ \"execute\": \"system_powerdown\" }") Nothing

  (Just si, Just so, _, ph) <-
      createProcess $ (proc cmd args) {
                          cwd = Just "/",
                          env = Nothing,
                          std_in = CreatePipe,
                          std_out = CreatePipe,
                          std_err = Inherit
                        }


  hSetBuffering si NoBuffering
  hSetBuffering so NoBuffering

  qmp cmd_chan ev_chan si so

  (rv, exitCode) <- monitor ev_chan cmd_chan ph

  slog $ "Process exited (rv = "<>show rv<>")"

  exitWith exitCode

qmp cmd_chan ev_chan si so = do
  ehelo <- parseQmp <$> hGetLine so
  case ehelo of
    Left err -> error $ "Process returned invalid greeting: "++err
    Right Greeting -> do
      hPutStrLn si "{ \"execute\": \"qmp_capabilities\" }"
      eres <- parseQmp <$> hGetLine so
      case eres of
        Left err -> error $ "Process returned invalid json: "++err
        Right Return -> do
          _ <- forkIO $ qmp_read so ev_chan
          _ <- forkIO $ qmp_write si cmd_chan
          return ()
        Right _ -> error $ "Process returned invalid ack"
    Right _ -> error $ "Process returned invalid qmp greeting"

qmp_read so ev_chan = swallow $ forever $ do
  eres <- parseQmp <$> hGetLine so
  when debug $ slog $ "qmp_read: " <> show eres
  case eres of
    Left err -> writeChan ev_chan eres
    Right _ -> writeChan ev_chan eres

qmp_write si cmd_chan = forever $ do
  cmd <- readChan cmd_chan
  hPutStrLn si cmd
  when debug $ slog $ "qmp_write: " <> cmd

monitor ec cc ph = do
  eres <- readChan ec
  let exit = do
        writeChan cc "{ \"execute\": \"quit\" }"
        waitForProcess ph
  case eres of
    Left err -> error $ "Process returned invalid json for event: "++err
    Right (Event "SHUTDOWN") -> slog "monitor: SHUTDOWN" >> (,ExitSuccess) <$> exit
    Right (Event "RESET")    -> slog "monitor: RESET"    >> (,ExitFailure 1) <$> exit
    Right _ -> monitor ec cc ph

swallow a = catch a (\(SomeException _) -> return ())

data QMPRes = Greeting
            | Return
            | Event String
            | Other Value
              deriving (Eq, Show)

parseQmp :: String -> Either String QMPRes
parseQmp = fmap qmpResponse . eitherDecodeStrict . T.encodeUtf8 . T.pack

qmpResponse (Object (HMap.toList -> m))
    | Just _ <- lookup "QMP" m = Greeting
    | Just _ <- lookup "return" m = Return
    | Just (String ev) <- lookup "event" m = Event $ T.unpack ev
    | otherwise = Other (Object $ HMap.fromList m)
