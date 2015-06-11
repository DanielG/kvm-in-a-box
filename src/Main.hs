{-# LANGUAGE RecordWildCards, NamedFieldPuns, ViewPatterns,
  NoMonomorphismRestriction #-}
module Main where

import Options.Applicative
import Options.Applicative.Types
import System.Directory
import System.Process
import System.Environment
import System.Posix.User
import System.FilePath
import Control.Monad.Trans.Except
import Control.Monad
import Data.Functor.Identity
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Bool
import qualified Data.SConfig as SC
import Text.Read
import Text.Show.Pretty

import Types
import Iface
import Resource
import Lvm
import Utils
import Qemu
import Config


data Config = Config {
      cInterface :: String,
      cAddress   :: Address
    }

readConfig :: IO Config
readConfig = do
  m <- SC.readConfig <$> readFile configFile
  let Just cfg = Config <$> get "interface" m
                        <*> ( (,,) <$> get "address" m
                                   <*> get "netmask" m
                                   <*> get "gateway" m
                            )
  return cfg

 where
   get k mp = dropWhile isSpace <$> SC.getValue k mp


readState :: IO State
writeState :: State -> IO ()

readState = do
  e <- doesFileExist stateFile
  if not e
    then return defState
    else read . head . drop 1 . lines <$> readFile stateFile

writeState s = writeFile stateFile $ "kvm-in-a-box state format: v1\n" ++ show s

modifyState f = readState >>= f >>= writeState

list :: Config -> State -> IO State
list cfg s@State {..} = do
  mapM putStrLn (Map.keys sVms)
  return s

infoCmd :: VmName -> Config -> State -> IO State
infoCmd vmn cfg s@State {..} = do
  mapM (putStrLn . ppShow) (Map.lookup vmn sVms)
  return s

create :: Vm -> VmVolatileState -> Config -> State -> IO State
create vm@Vm {vName=vmn} vmvs cfg s@State {..}
    | not $ vName vm `Map.member` sVms = do
        ensure cfg sVms
        return $ s { sVms = Map.insert vmn (vm,  vmvs, VmSolidState) sVms }
    | otherwise =
        error $ "VM '"++vName vm++"' already exists."

destroy :: VmName -> Config -> State -> IO State
destroy vmn cfg s@State {..} =
    return s { sVms = Map.filter ((/=vmn) . vName . fst3) sVms }

change :: VmName -> VmVolatileState -> Config -> State -> IO State
change vmn vmvs cfg s@State {..} = do
  case Map.lookup vmn sVms of
    Nothing -> error $ "VM '"++vmn++"' does not exist."
    Just vmi@(vm, vmvs', vmss') -> do

      let vmi = ( vm
                , vmvs `combineVmVolatileStates` vmvs'
                , VmSolidState
                )
          sVms1 = Map.insert vmn vmi sVms

      ensure cfg sVms1
      return $ s { sVms = sVms1 }

combineVmVolatileStates a b = VmVolatileState {
      vUp        = combine vUp,
      vPublicIf  = combine vPublicIf,
      vPrivateIf = combine vPrivateIf,
      vGroupIfs  = combine vGroupIfs
    }
 where combine f = f a `mplus` f b

start :: VmName -> Config -> State -> IO State
start vmn cfg s@State {..} = do
  case Map.lookup vmn sVms of
    Nothing -> error $ "VM '"++vmn++"' does not exist."
    Just (vm, vmvs, vmss) -> do
--        forkProcess
--        createDirectory $ varrundir </> vmn
--        dropPriviledges $ "kib-" ++ vmn
        print $ qemu (varrundir </> vmn) vm vmvs
        return s -- TODO!!!

stop :: VmName -> Config -> State -> IO State
stop = undefined

ensure Config {..} vms =
    ensureResource $ publicInterfaceResource (mkIface cInterface) cAddress (Map.keys vms)

dropPriviledges user = do
  setCurrentDirectory "/"
  UserEntry {userID, userGroupID } <- getUserEntryForName $ user
  setGroupID userGroupID
  setUserID userID

exceptP :: Either String a -> ReadM a
exceptP (Left s)  = readerError s
exceptP (Right a) = return a

intOption :: Mod OptionFields Int -> Parser Int
intOption = option (exceptP . readEither =<< readerAsk)

macOption :: Mod OptionFields MAC -> Parser MAC
macOption = option (exceptP . parseMac =<< readerAsk)

defaultOpt x f = fromMaybe x <$> optional f

vmP :: Parser Vm
vmP = Vm
     <$> strArgument (metavar "NAME")
     <*> defaultOpt 1 $$ intOption $$
               long "cpus"
            <> metavar "CORES"
            <> help "How many cpu cores to give the VM"
     <*> defaultOpt 512 $$ intOption $$
               long "mem"
            <> metavar "MB"
            <> help "How many [MB] of memory to give the VM"
     <*> defaultOpt "x86_64" $$ strOption $$
               long "arch"
            <> metavar "ARCH"
            <> help "Which CPU architecture the VM should use"
 where
   infixr 5 $$
   ($$) = ($)

genMac = MAC "<generate>"

vmVolatileStateP :: Parser VmVolatileState
vmVolatileStateP = VmVolatileState
     <$> ( optional $ (switch $$ long "up") <|> (not <$> switch $$ long "down") )

     <*> optional (bool Nothing (Just genMac) <$$> switch $$
                 long "public"
            <<>> help "Enable the public network interface"
       <||>
         Just <$$> macOption $$
                 long "public"
            <<>> metavar "XX:XX:XX:XX:XX:XX"
            <<>> help "Enable the public network interface with this MAC address")

     <*> optional (bool Nothing (Just genMac) <$$> switch $$
                 long "private"
            <<>> help "Enable the private network interface"
       <||>
         Just <$$> macOption $$
                 long "private"
            <<>> metavar "XX:XX:XX:XX:XX:XX"
            <<>> help "Enable the private network interface with this MAC address")
     <*> pure Nothing

 where
   infixl 6 <||>
   infixl 7 <$$>
   infixr 8 $$
   infixr 9 <<>>

   ($$) = ($)
   (<||>) = (<|>)
   (<<>>) = (<>)
   (<$$>) = (<$>)

     -- <*> optional $$ intOption $$
     --             long "disk"
     --        <<>> metavar "SIZE"
     --        <<>> help "Disk size in MB"


commands :: Parser (Config -> State -> IO State)
commands = subparser $

  command "list" $$ withInfo "Print names of all VMs" $$
      pure list   <<>>

  command "info" $$ withInfo "Print names of all VMs" $$
      infoCmd <$> strArgument (metavar "NAME")  <<>>

  command "create" $$ withInfo "Create a new VM" $$
      create <$> vmP <*> vmVolatileStateP   <<>>

  command "destroy" $$ withInfo "Destroy an existing VM" $$
      destroy <$> strArgument (metavar "NAME")   <<>>

  -- command "import" $$ withInfo "Destroy an existing VM" $$
  --     importCmd <$> strArgument (metavar "NAME") <*> strArgument (metavar "FILE")   <<>>

  command "change" $$ withInfo "Change an existing VM" $$
      change <$> strArgument (metavar "NAME") <*> vmVolatileStateP   <<>>

  command "start" $$ withInfo "Start an existing VM" $$
      start <$> strArgument (metavar "NAME")   <<>>

  command "stop" $$ withInfo "Stop an existing VM" $$
      stop <$> strArgument (metavar "NAME")

 where
   infixr 0 <<>>
   infixr 1 $$

   (<<>>) = (<>)
   ($$) = ($)

withInfo :: String -> Parser a -> ParserInfo a
withInfo desc opts = info (helper <*> opts) $ progDesc desc

main = do
  x <- execParser opts
  cfg <- readConfig
  modifyState (x cfg)
  return ()
 where
   opts = info (helper <*> commands) $
        fullDesc
