{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Language.Haskell.TH.Syntax as TH (qRunIO, lift)
import Options.Applicative
import Options.Applicative.Types
import System.Directory
import System.Process
import System.Environment
import System.Posix.User
import System.Posix.Files
import System.Process
import System.FilePath
import System.IO
import System.IO.Temp
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
import Data.IP
--import Distribution.VcsRevision.Git
import Text.Read
import Text.Show.Pretty
import Text.URI

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
import Sysctl
import Iptables

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

authorize :: VmName -> String -> Config -> Options -> State -> IO State
authorize vmn key cfg opts s@State {sVms=sVms0} = do
  case Map.lookup vmn sVms0 of
    Nothing -> error $ "VM '"++vmn++"' does not exist."
    Just vm0 -> do
      let dotssh = homedir </> "kib-" ++ vmn </> ".ssh"
      let authorized_keys = dotssh </> "authorized_keys"
      createDirectoryIfMissing True dotssh
      mls <- lines . fromMaybe "" <$> readFileMaybe authorized_keys

      writeFile' authorized_keys $ unlines $ nub $ sort $ mls ++ [key]

      uid <- userID <$> getUserEntryForName ("kib-" ++ vmn)
      gid <- groupID <$> getGroupEntryForName "kib"
      setOwnerAndGroup authorized_keys uid gid
      setOwnerAndGroup dotssh uid gid

      return s

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
  hPutStrLn stderr "Type RET to get a prompt (serial console)"
  hPutStrLn stderr "Type ^] (ASCII GS) to exit console."
  pro [ "socat",  "STDIO,raw,echo=0,escape=0x1d",  "UNIX-CONNECT:" ++ (varrundir </> "kib-" ++ vmn </> "ttyS0.unix") ]


resources cfg@Config {..} Options {..} kibGrp hosts vms = do
    notTesting <- amNotTesting
    return $ ManyResources [ passwdR kibGrp
                           , pubIfR notTesting
                           , privIfR notTesting
                           , mkSystemdR hosts
                           , dnsmasqR
                           , lvmOwnerResources $ Map.elems vms
                           ]
 where
   vmns = Map.keys vms

   filterVMNs p = map vName $ filter p $ Map.elems vms

   pubVms   = filterVMNs (vPublicIf . vVS)
   privVms  = filterVMNs (vPrivateIf . vVS)
   groupVms = filterVMNs (not . Set.null . vGroupIfs . vVS)

   caddr = cAddress
   caddr6 = cAddress6
   cpriv6 = cPrivate6
   -- cgrp6 = cGroup6

   pubIfR _ | null pubVms = ManyResources []
   pubIfR amRoot =
    interfaceResource (mkIface "kipubr") (Just caddr) caddr6 pubVms amRoot

   privIfR _ | null privVms = ManyResources []
   privIfR amRoot =
    -- priv interface doesn't get an IPv4 address since that would be silly
    -- complicated
    interfaceResource (mkIface "kiprivbr") Nothing cpriv6 privVms amRoot

   passwdR grp =
       passwdResource (Map.keys vms) grp

   mkSystemdR hosts = ManyResources
                  $ Map.elems
                  $ Map.mapWithKey vmInitResource
                  $ Map.map (\(vm, (mac,_ip)) -> qemu varrundir (Qemu [] False vm) mac)
                  $ Map.intersectionWith (,) vms (Map.fromList hosts)

   dnsmasqR = ManyResources $
       [vmDnsDhcpResource [(mkIface "kipubr"), (mkIface "kiprivbr")] cfg, vmHostLeaseResource cAddress vmns]

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

ensure :: Config -> Options -> Map VmName Vm -> IO (Map VmName (MAC, IPv4))
ensure cfg@Config {..} opts@Options {..} vms = do
    kibGrp <- getGroupEntryForName "kib"
    hosts <- allocateHosts oRoot cAddress vmns

    rs <- resources cfg opts kibGrp hosts vms

    -- mapM putStrLn $ resourcePaths rs

    swallow $ ensureResource oRoot rs

    unlessTesting $ void $ system "systemctl daemon-reload"

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
    ensureResource (oRoot opts) sysctlResource
    return s

setupIptables :: Config -> Options -> State -> IO State
setupIptables cfg opts s = do
    ensureResource (oRoot opts) undefined -- ip6tablesResource
    return s

commands :: Parser (Config -> Options -> State -> IO State)
commands = subparser $ mconcat
  [
    -- command "git-version" $ withInfo "Print version intormation" $
    --   pure $ const $ const $ \s -> do
    --     putStrLn $(do
    --       v <- TH.qRunIO getRevision
    --       TH.lift $ case v of
    --         Nothing           -> "<none>"
    --         Just (hash,True)  -> hash ++ " (with local modifications)"
    --         Just (hash,False) -> hash)
    --     return s

    command "list" $ withInfo "Print names of all VMs" $
      pure list

  , command "list-resources" $ withInfo "Print all resource paths" $
      pure listResources

  , command "info" $ withInfo "Print details of a single VM" $
      infoCmd <$> strArgument (metavar "NAME")

  , command "console" $ withInfo "Connect to VM serial console (socat)" $
      console <$> strArgument (metavar "NAME")

  , command "create" $ withInfo "Create a new VM" $
      create <$> strArgument (metavar "name") <*> vmP

  , command "destroy" $ withInfo "Destroy an existing VM" $
      destroy <$> strArgument (metavar "NAME")

  , command "change" $ withInfo "Change an existing VM" $
      change <$> strArgument (metavar "NAME") <*> vmP

  , command "authorize" $ withInfo "Add an authorized SSH public key to a VM" $
      authorize <$> strArgument (metavar "NAME") <*> strArgument (metavar "KEY")

  , command "start" $ withInfo "Start an existing VM" $
      start <$> strArgument (metavar "NAME")

  , command "stop" $ withInfo "Stop an existing VM" $
      stop <$> strArgument (metavar "NAME")

  -- TODO: no need for a seperate command
  , command "setup" $ withInfo "Perform initial envirnment configuration" $
      pure setup

  , command "setup-iptables" $ withInfo "Internal test command" $
      pure setupIptables

  ]

withInfo :: String -> Parser a -> ParserInfo a
withInfo desc opts = info opts $ progDesc desc

-- TODO: exec dat shit?
adminConsole user = do
  let 'k':'i':'b':'-':vmn = user
  putStr "> "
  l <- dropWhileEnd isSpace . dropWhile isSpace <$> getLine
  case words l of
    [""] -> console' vmn
    ["console"] -> console' vmn
    ["reset"] -> do
        pro [ "killall", "-SIGQUIT", "qemu-system-x86_64" ]
        threadDelay (500 * 1000)
        console' vmn
    "install":"debian":[] ->
        installDebian vmn Nothing
    "install":"debian":"auto":[] ->
        installDebian vmn $ Just $ usrsharedir </> "preseed.cfg"
    -- "install":"debian":preseed:[] ->
    --     installDebian vmn $ Just preseed
    _ -> putStrLn "Unknown command" >> adminConsole user

installDebian :: VmName -> Maybe String -> IO ()
installDebian vmn mfile = void $ do
  let iso = varlibdir </> "di.iso"

  withSystemTempDirectory "kib" $ \tmp -> do
    preseed <-
      case mfile of
        Just file -> do
          rawSystem "cp" [file, tmp]
          return $ " auto=true priority=critical url=tftp://10.0.2.2/" ++ takeFileName file
        Nothing -> return ""

    rawSystem "7z" ["-o"++tmp, "x", iso, "install.amd"]
    exists <- doesDirectoryExist $ tmp </> "install.amd"
    when (not exists) $ error "Extracting Debian installer kernel+inird failed"
    let
        cmd:args' = flip (qemu varrundir) undefined
                  $ Qemu [("tftp", tmp)] True
                  $ Vm vmn defVmSS
                  $ VmVS 2 1024 "x86_64" True False False Set.empty

        args = args' ++ [ "-kernel", tmp </> "install.amd/vmlinuz"
                        , "-initrd", tmp </> "install.amd/initrd.gz"
                        , "-append", "console=ttyS0,9600" ++ preseed
                        , "-cdrom", iso
                        , "-boot", "d"
                        ]

    hPutStrLn stderr $ unwords $ cmd:args
    rawSystem cmd args

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
      putStrLn "  > install debian auto"
      putStrLn "  > install debian [PRESEED_URL]"
      putStrLn ""

      adminConsole =<< getEnv "USER"

    "kib" -> do
      (opts, f) <- execParser $ info ((,) <$> optionsP <*> commands <**> helper) fullDesc
      cfg <- readConfig opts
      modifyState opts (f cfg opts)
      return ()
