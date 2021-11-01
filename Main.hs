{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Language.Haskell.TH.Syntax as TH (qRunIO, lift)
import Options.Applicative
import Options.Applicative.Types
import System.Directory
import System.Process hiding (system, rawSystem)
import System.Environment
import System.Posix.User
import System.Posix.Files
import System.FilePath
import System.IO
import System.IO.Temp
import System.Exit
import Control.Monad
import Control.Arrow
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
import SystemdDBus
import MapJoin

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
        let vm = unVmFlags vmn $ combineVmFlags vmf0 (defVmFlags cfg)
            sVms1 = Map.insert vmn vm sVms0
        sNet <- ensureResources cfg opts sVms1
        return $ s { sVms = sVms1, sNet = sNet }
    | otherwise =
        error $ "VM '"++vmn++"' already exists."

remove :: VmName -> Config -> Options -> State -> IO State
remove vmn cfg opts s@State { sVms=sVms0, sNet=sNet0 }
    | vmn `Map.member` sVms0 = do
        let Just vm = Map.lookup vmn sVms0
            sVms1 = Map.delete vmn sVms0

        removeResources cfg opts sVms0 sNet0 vmn
        sNet1 <- ensureResources cfg opts sVms1

        return $ s { sVms = sVms1, sNet = sNet1 }

    | otherwise =
        error $ "VM '"++vmn++"' does not exist."


change :: VmName -> VmFlags -> Config -> Options -> State -> IO State
change vmn vmf cfg opts s@State {sVms=sVms0} = do
  case Map.lookup vmn sVms0 of
    Nothing -> error $ "VM '"++vmn++"' does not exist."
    Just vm0 -> do
      let vm1 = unVmFlags vmn $ combineVmFlags vmf (mkVmFlags vm0)
          sVms1 = Map.insert vmn vm1 sVms0
      sNet1 <- ensureResources cfg opts sVms1
      return $ s { sVms = sVms1, sNet = sNet1 }

-- authorize :: VmName -> String -> Config -> Options -> State -> IO State
-- authorize vmn key cfg opts s@State {sVms=sVms0} = do
--   case Map.lookup vmn sVms0 of
--     Nothing -> error $ "VM '"++vmn++"' does not exist."
--     Just vm0 -> do
--       let dotssh = homedir </> "kib-" ++ vmn </> ".ssh"
--       let authorized_keys = dotssh </> "authorized_keys"
--       createDirectoryIfMissing True dotssh
--       mls <- lines . fromMaybe "" <$> readFileMaybe authorized_keys

--       writeFile' authorized_keys $ unlines $ nub $ sort $ mls ++ [key]

--       uid <- userID <$> getUserEntryForName ("kib-" ++ vmn)
--       gid <- groupID <$> getGroupEntryForName "kib"
--       setOwnerAndGroup authorized_keys uid gid
--       setOwnerAndGroup dotssh uid gid

--       return s

lvcreate vmn size msub s@State {..} =
  case Map.lookup vmn sVms of
    Nothing -> error $ "VM '"++vmn++"' does not exist."
    Just Vm { vSysCfg = VmSysCfg{..} } ->
        let vol = fromMaybe vmn $ ((vmn ++ "-") ++) <$> msub in
        exitWith =<< rawSystem "lvcreate" ["-n", vol, "-L", size, vVg]

lvremove vmn msub s@State {..} =
  case Map.lookup vmn sVms of
    Nothing -> error $ "VM '"++vmn++"' does not exist."
    Just Vm { vSysCfg = VmSysCfg{..} } ->
        let vol = fromMaybe vmn $ ((vmn ++ "-") ++) <$> msub in
        exitWith =<< rawSystem "lvremove" ["/dev" </> vVg </> vol]

lvextend :: VmName -> String -> Maybe String -> State -> IO State
lvextend vmn size msub s@State {..} =
  case Map.lookup vmn sVms of
    Nothing -> error $ "VM '"++vmn++"' does not exist."
    Just Vm { vSysCfg = VmSysCfg{..} } ->
        let vol = fromMaybe vmn $ ((vmn ++ "-") ++) <$> msub in
        exitWith =<< rawSystem "lvcreate" ["-L", "/dev" </> vVg </> vol]


systemctl :: String -> [String] -> VmName -> Config -> Options -> State -> IO State
systemctl cmd args vmn cfg opts s@State {..} =
  case Map.lookup vmn sVms of
    Nothing -> error $ "VM '"++vmn++"' does not exist."
    Just _ -> do
      let user = "kib-" ++ vmn
      uid <- userID <$> getUserEntryForName user
      let sudo = [ "sudo", "-u", user, "XDG_RUNTIME_DIR=" ++ varrundir uid ]
      pro $ sudo ++ [ "systemctl", "--user", "daemon-reload" ]
      pro $ sudo ++ [ "systemctl", "--user", cmd, user ] ++ args
      return s

start :: VmName -> Config -> Options -> State -> IO State
start = systemctl "start" []

stop :: VmName -> Config -> Options -> State -> IO State
stop = systemctl "stop" []

status :: VmName -> Config -> Options -> State -> IO State
status = systemctl "status" ["--full"]

console :: VmName -> State -> IO ()
console vmn State {sVms=sVms0} = do
  case Map.lookup vmn sVms0 of
    Nothing -> error $ "VM '"++vmn++"' does not exist."
    Just vm0 -> do
      console' vmn

console' vmn = do
  uid <- userID <$> getUserEntryForName ("kib-" ++ vmn)
  let socket = (varrundir uid </> "kib-" ++ vmn ++ "-ttyS0.unix")

  e <- doesFileExist socket
  when (not e) $ hPutStrLn stderr $ "Waiting for " ++ socket ++ " to appear..."

  handleJust (\ex@UserInterrupt -> Just ex) (\_ -> exitFailure) $
    waitForFile socket

  hPutStrLn stderr "Type RET to get a prompt (serial console)"
  hPutStrLn stderr "Type ^] (ASCII GS) to exit console."
  pro [ "socat", "STDIO,raw,echo=0,escape=0x1d",  "UNIX-CONNECT:" ++ (varrundir uid </> "kib-" ++ vmn ++ "-ttyS0.unix") ]

 where
   waitForFile file = do
     e <- doesFileExist file
     if e
       then return ()
       else do
         threadDelay 500000
         waitForFile file


resources :: Config -> Options -> GroupEntry -> [(VmName, (MAC, IPv4))] -> Map VmName Vm -> IO ManyResources
resources cfg@Config {..} Options {..} kibGrp hosts vms = do
    notTesting <- amNotTesting
    return $ ManyResources $ concat
               [ [lvmOwnerResources $ Map.elems vms]
               , map authorizedKeysResource $ Map.elems vms
               , map SomeResource $
                [ passwdR kibGrp
                , pubIfR notTesting
                , privIfR notTesting
                , dnsmasqR
                , iptablesResource cfg (mkIface "kpu") (Map.elems vms) (Map.fromList hosts)
                ] ++ mkSystemdR hosts
               ]
 where
   vmns = Map.keys vms

   filterVMs p = Map.filter p vms
   vmHost vms = Map.toList $ Map.fromList hosts `leftSemiJoin` vms

   pubVms   = filterVMs (vPublicIf . vNetCfg)
   privVms  = filterVMs (vPrivateIf . vNetCfg)
   groupVms = filterVMs (not . Set.null . vGroupIfs . vNetCfg)

   caddr = cAddress
   caddr6 = cAddress6
   cpriv6 = cPrivate6
   -- cgrp6 = cGroup6

   pubIfR amRoot =
    interfaceResource (mkIface "kpu") (Just caddr) caddr6 (vmHost pubVms) amRoot

   privIfR amRoot =
    -- priv interface doesn't get an IPv4 address since that would be silly
    -- complicated
    interfaceResource (mkIface "kpr") Nothing cpriv6 (vmHost privVms) amRoot

   passwdR grp =
       passwdResource (Map.keys vms) grp

   mkSystemdR hosts =
                    Map.elems
                  $ Map.mapWithKey vmInitResource
                  $ Map.map (\(vm, (mac,_ip)) -> qemu "%t" mac (Qemu [] False vm))
                  $ Map.intersectionWith (,) vms (Map.fromList hosts)

   dnsmasqR = ManyResources $
       [ vmDnsDhcpResource (map mkIface ["kpu"]) cfg -- "kiprivbr" disabled until off-link option becomes available in debian version or we do a backport or something
       , vmHostLeaseResource cAddress vmns
       ]

listResources :: Config -> Options -> State -> IO State
listResources cfg@Config {..} opts@Options {..} s@State { sVms=vms } = do
    kibGrp <- getGroupEntryForName "kib"
    hosts <- allocateHosts oRoot cAddress vmns

    rs <- resources cfg opts kibGrp hosts vms
    os <- resourceOwners oRoot rs

    mapM_ print $ resourcePaths rs `zip` os

    return s

 where
   vmns = Map.keys vms

ensureResources :: Config -> Options -> Map VmName Vm -> IO (Map VmName (MAC, IPv4))
ensureResources cfg@Config {..} opts@Options {..} vms = do
    kibGrp <- getGroupEntryForName "kib"
    hosts <- allocateHosts oRoot cAddress vmns

    rs <- resources cfg opts kibGrp hosts vms

    swallow $ ensureResource oRoot rs

    unlessTesting $ do
      void $ system "/etc/init.d/netfilter-persistent reload"
      void $ system "/etc/init.d/dnsmasq reload"

    return $ Map.fromList hosts

 where
   vmns = Map.keys vms
   swallow ma = do
     eexa <- try $ evaluate =<< ma
     case eexa of
       Left (SomeException ex) ->
           hPutStrLn stderr $ "ensureResources: " ++ show ex
       Right a -> return a

removeResources :: Config -> Options -> Map VmName Vm -> Map VmName (MAC, IPv4) -> VmName -> IO ()
removeResources cfg opts@Options {..} vms hosts vmn = do
    kibGrp <- getGroupEntryForName "kib"
    rs <- resources cfg opts kibGrp (Map.toList hosts) vms

    let actions = removeResource oRoot (Just $ OwnerVm vmn) rs
    mapM_ (handle (\(ex :: SomeException) -> hPutStrLn stderr $ show ex)) actions

setup :: Config -> Options -> State -> IO State
setup cfg opts s = do
    unlessTesting $ pro_ ["etckeeper", "commit", "kib: before 'kib setup'"]
    ensureResource (oRoot opts) sshdResource
    ensureResource (oRoot opts) sysctlResource
    unlessTesting $ void $ system "addgroup --system kib"
    unlessTesting $ void $ system "addgroup --system kib-admin"
    unlessTesting $ pro ["etckeeper", "commit", "kib: after 'kib setup'"]
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
      stateless $ console <$> strArgument (metavar "NAME")

  , command "create" $ withInfo "Create a new VM" $
      create <$> strArgument (metavar "name") <*> vmP

  , command "remove" $ withInfo "Remove a VM's configuration (not disk state)" $
      remove <$> strArgument (metavar "NAME")

  , command "change" $ withInfo "Change an existing VM" $
      change <$> strArgument (metavar "NAME") <*> vmP

  , command "lv" $ withInfo "volume management" $ (helper <*>) $
      subparser $ mconcat [
        command "create" $ withInfo "Create logical volume" $ (helper <*>) $
          stateless $ lvcreate <$> strArgument (metavar "NAME")
                               <*> strArgument (metavar "SIZE")
                               <*> subvolOpt
      , command "extend" $ withInfo "Resize logical volume" $ (helper <*>) $
          stateless $ lvextend <$> strArgument (metavar "NAME")
                               <*> strArgument (metavar "SIZE")
                               <*> subvolOpt
      , command "remove" $ withInfo "Remove logical volume" $ (helper <*>) $
          stateless $ lvremove <$> strArgument (metavar "NAME")
                               <*> subvolOpt
      ]

  , command "lvs" $ withInfo "logical volume information" $
      stateless $ pure $ const $ exitWith =<< rawSystem "lvs" []

  -- See "--ssh-key" option
  -- , command "authorize" $ withInfo "Add an authorized SSH public key to a VM" $
  --     authorize <$> strArgument (metavar "NAME") <*> strArgument (metavar "KEY")

  , command "start" $ withInfo "Start an existing VM" $
      start <$> strArgument (metavar "NAME")

  , command "stop" $ withInfo "Stop an existing VM" $
      stop <$> strArgument (metavar "NAME")

  , command "status" $ withInfo "Show VM status" $
      status <$> strArgument (metavar "NAME")

  -- TODO: no need for a seperate command
  , command "setup" $ withInfo "Perform initial envirnment configuration" $
      pure setup

  ]
 where
   stateless :: Parser (State -> IO a)
             -> Parser (Config -> Options -> State -> IO State)
   stateless mf = (\f _cfg _opts s -> (const s) <$> f s) <$> mf

   subvolOpt = optional $ strOption $
                    long "subvol"
               <<>> metavar "SUBVOL"
               <<>> help "Operate on a subvolume instead of the main"

withInfo :: String -> Parser a -> ParserInfo a
withInfo desc opts = info opts $ progDesc desc

-- TODO: exec dat shit?
adminConsole user = do
  pro [ "systemctl", "--user", "daemon-reload" ]
  let 'k':'i':'b':'-':vmn = user
  putStr "> "
  l <- dropWhileEnd isSpace . dropWhile isSpace <$> getLine
  case words l of
    ["console"] -> do
        handle (\(_ :: ExitCode) -> return ()) $ console' vmn
    ["reset"] -> do
        pro [ "systemctl", "--user", "restart", user ]
        console' vmn
    ["status"] -> do
        pro_ [ "systemctl", "--user", "status", "--full", user ]
        adminConsole user
    ["start"] -> do
        pro [ "systemctl", "--user", "start", user ]
        pro_ [ "systemctl", "--user", "status", user ]
        adminConsole user
    ["stop"] -> do
        pro [ "systemctl", "--user", "stop", user ]
        pro_ [ "systemctl", "--user", "status", user ]
        adminConsole user
    ["kill"] -> do
        pro [ "systemctl", "--user", "kill", user ]
        pro_ [ "systemctl", "--user", "status", user ]
        adminConsole user
    ["disable"] -> do
        pro [ "systemctl", "--user", "disable", user ]
        adminConsole user
    ["enable"] -> do
        pro [ "systemctl", "--user", "enable", user ]
        adminConsole user

    "install":"debian":[] -> do
        checkUnitConflict vmn
        pro [ "systemctl", "--user", "start", user ++ "-install@debian-manual" ]
        console' vmn
    "install":"debian":"auto":[] -> do
        checkUnitConflict vmn
        pro [ "systemctl", "--user", "start", user ++ "-install@debian-auto" ]
        console' vmn
    "install":"debian":preseed:[] -> do
        checkUnitConflict vmn
        pro [ "systemctl", "--user", "start", user ++ "-install@debian-" ++ preseed ]
        console' vmn
    ["status-install"] -> do
        pro_ [ "systemctl", "--user", "status", user ++ "-install@*" ]
        adminConsole user
    ["kill-install"] -> do
        pro [ "systemctl", "--user", "stop", user ++ "-install@*" ]
        pro_ [ "systemctl", "--user", "status", user ++ "-install@*" ]
        adminConsole user

    _ -> putStrLn "Unknown command" >> adminConsole user


checkUnitConflict vmn = do
    let vmUnit = "kib-"++vmn
        installUnit = "kib-"++vmn++"-install@"

    addr <- userInstanceAddress

    units <- listUnits addr

    let fail = do
         hPutStrLn stderr "VM is running, refusing to let you shoot yourself in the foot."
         exitFailure

    case find ((==vmUnit) . getUnitName) units of
      Just (getUnitActiveState -> "active") -> fail
      _ -> return ()

    case find ((installUnit `isPrefixOf`) . getUnitName) units of
      Just (getUnitActiveState -> "active") -> fail
      _ -> return ()

install user = do
  pro [ "systemctl", "--user", "daemon-reload" ]
  let 'k':'i':'b':'-':vmn = user
  args <- getArgs
  case args of
    ["debian-manual"] -> installDebian vmn Nothing
    ["debian-auto"] -> installDebian vmn $ Just $ usrsharedir </> "preseed.cfg"
    [str] | Just file <- stripPrefix "debian-" str -> do
      when (any (=='/') file) $ do
        hPutStrLn stderr "invalid preeseed file name"
        exitFailure

      installDebian vmn $ Just $ "/home" </> user </> file

installDebian :: VmName -> Maybe String -> IO ()
installDebian vmn mfile = void $ do
  let iso = varlibdir </> "di.iso"

  withSystemTempDirectory "kib" $ \tmp -> do
    preseed <-
      case mfile of
        Just file -> do
          Config {cSshKey} <- readConfig "/"
          preseed_cfg <- readFile file
          -- TODO: reading this instead of passing the information is a hack
          root_keys_cmd <- constructAuthorizedKeysCmd $
            "/etc/ssh/authorized_keys/kib-" ++ vmn
          let replace_kib_keys_cmd
                | Just keys <- cSshKey =
                  replaceStr "true KIB_SSH_KEY_COMMAND_PLACEHOLDER" $
                  installSshKeysCmd $ lines keys
                | otherwise = id
          writeFile (tmp </> takeFileName file) $
            replace_kib_keys_cmd $
            replaceStr "true ROOT_SSH_KEY_COMMAND_PLACEHOLDER" root_keys_cmd $
                       preseed_cfg
          return $ unwords [ ""
                           , "auto=true"
                           , "priority=critical"
                           , "url=tftp://10.0.2.2/" ++ takeFileName file
                           ]
        Nothing -> return ""

    rawSystem "7z" [ "-o"++tmp, "x", iso, "install.amd" ]
    exists <- doesDirectoryExist $ tmp </> "install.amd"

    when (not exists) $
      error "Extracting Debian installer kernel+inird failed"

    uid <- getRealUserID

    State { sVms, sNet } <- readState "/"

    let Just net@(mac, ipv4) = Map.lookup vmn sNet
        Just vm' = Map.lookup vmn sVms
        vm  = vm' { vNetCfg = defVmNetCfg { vUserIf = True } }

        cmd:args' = (qemu (varrundir uid)) mac $ Qemu [("tftp", tmp)] False vm

        args = args' ++ [ "-kernel", tmp </> "install.amd/vmlinuz"
                        , "-initrd", tmp </> "install.amd/initrd.gz"
                        , "-append", "console=ttyS0,9600" ++ preseed
                        , "-cdrom", iso
                        , "-boot", "d"
                        ]

    hPutStrLn stderr $ unwords $ cmd:args
    rawSystem "kib-supervise" $ cmd:args

  where
    replaceStr old new = intercalate new . splitOn old
    constructAuthorizedKeysCmd keys_file = do
      auth_keys <- readFile keys_file
      return $ installSshKeysCmd $ lines auth_keys
    installSshKeysCmd keys =
        intercalate "; " $
          map (\key -> "echo '"++key++"' >> /root/.ssh/authorized_keys") $
          keys

main = do
  prog <- getProgName
  case prog of
    x | x == "kib-console" || x == "-kib-console"  -> do
      hSetBuffering stdout NoBuffering

      putStrLn ""
      putStrLn "kvm-in-a-box VM admin console"
      putStrLn "Commands:"
      putStrLn "  > status"
      putStrLn "  > console"
      putStrLn "  > reset"
      putStrLn "  > start"
      putStrLn "  > stop"
      putStrLn "  > kill"
      putStrLn "  > enable"
      putStrLn "  > disable"
      putStrLn "  > install debian auto"
      putStrLn "  > install debian [PRESEED_URL]"
      putStrLn "  > status-install"
      putStrLn "  > kill-install"
      putStrLn ""

      adminConsole =<< getLoginName

    x | x == "kib-install" || x == "-kib-install"  -> do
      hSetBuffering stdout NoBuffering
      install =<< getEnv "USER"

    "kib" -> do
      (opts, f) <- execParser $ info ((,) <$> optionsP <*> commands <**> helper) fullDesc
      cfg <- readConfig (oRoot opts)
      modifyState (oRoot opts) (f cfg opts)
      return ()
