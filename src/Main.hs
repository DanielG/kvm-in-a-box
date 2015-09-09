module Main where

import Options.Applicative
import Options.Applicative.Types
import System.Directory
import System.Process
import System.Environment
import System.Posix.User
import System.FilePath
import System.IO
import Control.Monad
import Control.DeepSeq
import Control.Exception
import Control.Concurrent
import Data.Functor.Identity
import qualified Data.Traversable as T
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char
import Data.Monoid
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import Text.Read
import Text.Show.Pretty

import Types
import Options
import Iface
import Resource
import Lvm
import Utils
import Qemu
import Config
import Systemd
import Passwd
import Ssh
import Files
import MAC
import Dnsmasq
import Udev
import Log

list :: Config -> Options -> State -> IO State
list cfg opts s@State {..} = do
  mapM putStrLn (Map.keys sVms)
  return s

infoCmd :: VmName -> Config -> Options -> State -> IO State
infoCmd vmn cfg opts s@State {..} = do
  putStrLn $ ppShow $ Map.lookup vmn sVms
  return s

create :: VmName -> VmFlags -> Config -> Options -> State -> IO State
create vmn vmf0 cfg opts s@State { sVms=sVms0 }
    | not $ vmn `Map.member` sVms0 = do
        let vm = unVmFlags vmn $ combineVmFlags vmf0 defVmFlags
            sVms1 = Map.insert vmn vm sVms0
        sNet <- ensure cfg opts sVms1
        return $ s { sVms = sVms1, sNet = sNet }
    | otherwise =
        error $ "VM '"++vmn++"' already exists."

destroy :: VmName -> Config -> Options -> State -> IO State
destroy vmn cfg opts s@State { sVms=sVms0 } = do
    let sVms1 = Map.filter ((/=vmn) . vName) sVms0
    sNet <- ensure cfg opts sVms1
    return $ s { sVms = sVms1, sNet = sNet }

change :: VmName -> VmFlags -> Config -> Options -> State -> IO State
change vmn vmf cfg opts s@State {sVms=sVms0} = do
  case Map.lookup vmn sVms0 of
    Nothing -> error $ "VM '"++vmn++"' does not exist."
    Just vm0 -> do
      let vm1 = unVmFlags vmn $ combineVmFlags vmf (mkVmFlags vm0)
          sVms1 = Map.insert vmn vm1 sVms0
      sNet1 <- ensure cfg opts sVms1
      return $ s { sVms = sVms1, sNet = sNet1 }

start :: VmName -> Config -> Options -> State -> IO State
start vmn cfg opts s@State {..} = error "TODO: systemctl start ..."

stop :: VmName -> Config -> Options -> State -> IO State
stop = error "TODO: systemctl start ..."

console :: VmName -> Config -> Options -> State -> IO State
console vmn cfg opts s@State {sVms=sVms0} = do
  case Map.lookup vmn sVms0 of
    Nothing -> error $ "VM '"++vmn++"' does not exist."
    Just vm0 -> do
      console' vmn
      return s

console' vmn = do
  hPutStrLn stderr "Type ^] to exit console."
  pro [ "socat",  "STDIO,raw,echo=0,escape=0x1d",  "UNIX-CONNECT:" ++ (varrundir </> vmn </> "ttyS0.unix") ]


resources cfg@Config {..} Options {..} kibGrp hosts vms = do
    systemdR <- mkSystemdR hosts
    return $ ManyResources [ passwdR kibGrp, pubIfR, systemdR, dnsmasqR, dirsR
                           , lvmOwnerResources $ Map.elems vms ]
 where
   vmns = Map.keys vms
   passwdR grp = passwdResource (Map.keys vms) grp
   pubIfR = interfaceResource (mkIface "kipubr") cAddress (Map.keys vms)
   mkSystemdR hosts = ManyResources
                  . Map.elems
                 <$> (T.sequence
                  $ Map.mapWithKey vmInitResource
                  $ Map.map (\(vm, (mac,_ip)) -> qemu varrundir vm mac)
                  $ Map.intersectionWith (,) vms (Map.fromList hosts))

   dnsmasqR = ManyResources $
       [vmDnsDhcpResource cfg, vmHostLeaseResource cAddress vmns]
   dirsR = ManyResources $ map (qemuRunDirsResource varrundir) vmns

listResources :: Config -> Options -> State -> IO State
listResources cfg@Config {..} opts@Options {..} s@State { sVms=vms } = do
    kibGrp <- getGroupEntryForName "kib"
    hosts <- allocateHosts oRoot cAddress vmns

    rs <- resources cfg opts kibGrp hosts vms

    mapM putStrLn $ resourcePaths rs

    putStrLn "" >> putStrLn ""

    -- TODO: root
    mapM (putStrLn . show) =<< resourceOwners oRoot rs

    return s

 where
   vmns = Map.keys vms

ensure cfg@Config {..} opts@Options {..} vms = do
    kibGrp <- getGroupEntryForName "kib"
    hosts <- allocateHosts oRoot cAddress vmns

    rs <- resources cfg opts kibGrp hosts vms

    mapM putStrLn $ resourcePaths rs

    swallow $ ensureResource oRoot rs

    return $ Map.fromList hosts

 where
   vmns = Map.keys vms
   swallow ma = do
     eexa <- try $ evaluate =<< ma
     case eexa of
       Left (SomeException ex) -> hPutStrLn stderr $ "ensure: " ++ show ex
       Right a -> return a


setup :: Config -> Options -> State -> IO State
setup cfg opts s = do
    ensureResource (oRoot opts) sshdResource
    return s

commands :: Parser (Config -> Options -> State -> IO State)
commands = subparser $ mconcat
  [ command "list" $ withInfo "Print names of all VMs" $
      pure list

  , command "list-resources" $ withInfo "Print all resource paths" $
      pure listResources

  , command "info" $ withInfo "Print details of a single VM" $
      infoCmd <$> strArgument (metavar "NAME")

  , command "console" $ withInfo "Connect to VM console (socat)" $
      console <$> strArgument (metavar "NAME")

  , command "create" $ withInfo "Create a new VM" $
      create <$> strArgument (metavar "name") <*> vmP

  , command "destroy" $ withInfo "Destroy an existing VM" $
      destroy <$> strArgument (metavar "NAME")

  , command "change" $ withInfo "Change an existing VM" $
      change <$> strArgument (metavar "NAME") <*> vmP

  , command "start" $ withInfo "Start an existing VM" $
      start <$> strArgument (metavar "NAME")

  , command "stop" $ withInfo "Stop an existing VM" $
      stop <$> strArgument (metavar "NAME")

  , command "setup" $ withInfo "Perform initial envirnment configuration" $
      pure setup
  ]

withInfo :: String -> Parser a -> ParserInfo a
withInfo desc opts = info opts $ progDesc desc

-- TODO: exec dat shit?
adminConsole user = do
  let 'k':'i':'b':'-':vm = user
  putStr "> "
  l <- dropWhileEnd isSpace . dropWhile isSpace <$> getLine
  case l of
    "console" -> console' vm
    "reset" -> do
        pro [ "killall", "-SIGQUIT", "qemu-system-x86_64" ]
        threadDelay (500 * 1000)
        console' vm
    _ -> putStrLn "Unknown command" >> adminConsole user

main = do
  prog <- getProgName
  case prog of
    x | x == "kib-console" || x == "-kib-console"  -> do
      hSetBuffering stdout NoBuffering

      putStrLn ""
      putStrLn "kvm-in-a-box VM admin console"
      putStrLn "Commands:"
      putStrLn "  > console"
      putStrLn "  > reset"
      putStrLn ""

      adminConsole =<< getEnv "USER"

    "kib" -> do
      (opts, f) <- execParser $ info ((,) <$> optionsP <*> commands <**> helper) fullDesc
      cfg <- readConfig opts
      modifyState opts (f cfg opts)
      return ()
