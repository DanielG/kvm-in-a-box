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
import Data.Functor.Identity
import qualified Data.Traversable as T
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char
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

ensure cfg@Config {..} Options {..} vms = do
    let root = oRoot
    kibGrp <- getGroupEntryForName "kib"
    hosts <- allocateHosts root cAddress vmns
    print hosts

    swallow $ ensureResource root $ passwdR kibGrp
    swallow $ ensureResource root $ pubIfR
    swallow $ ensureResource root =<< systemdR hosts
    swallow $ ensureResource root $ dnsmasqR
    swallow $ ensureResource root dirsR

    return $ Map.fromList hosts

 where
   swallow ma = do
     eexa <- try $ evaluate =<< ma
     case eexa of
       Left (SomeException ex) -> hPutStrLn stderr $ "ensure: " ++ show ex
       Right a -> return a

   vmns = Map.keys vms
   passwdR grp = passwdResource (Map.keys vms) grp
   pubIfR = interfaceResource (mkIface "kipubr") cAddress (Map.keys vms)
   systemdR hosts = ManyResources
                  . Map.elems
                 <$> (T.sequence
                  $ Map.mapWithKey vmInitResource
                  $ Map.map (\(vm, (mac,_ip)) -> qemu varrundir vm mac)
                  $ Map.intersectionWith (,) vms (Map.fromList hosts))

   dnsmasqR = ManyResources $
       [vmDnsDhcpResource cfg, vmHostLeaseResource cAddress vmns]
   dirsR = ManyResources $ map (qemuRunDirsResource varrundir) vmns

setup :: Config -> Options -> State -> IO State
setup cfg opts s = do
    ensureResource (oRoot opts) sshdResource
    return s

commands :: Parser (Config -> Options -> State -> IO State)
commands = subparser $ mconcat
  [ command "list" $ withInfo "Print names of all VMs" $
      pure list

  , command "info" $ withInfo "Print names of all VMs" $
      infoCmd <$> strArgument (metavar "NAME")

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

main = do
  (opts, f) <- execParser $ info ((,) <$> optionsP <*> commands) fullDesc
  cfg <- readConfig opts
  modifyState opts (f cfg opts)
  return ()
