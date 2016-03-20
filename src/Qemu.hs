module Qemu where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List
import System.FilePath
import System.Posix.User
import System.Posix.Files

import Types
import MAC
import Resource

data Qemu = Qemu {
      qUserIfOpts :: [(String,String)],
      qStdioConsole :: Bool,
      qVm :: Vm
    }

qemu :: FilePath -> MAC -> Qemu -> [String]
qemu rundir mac Qemu { qVm = Vm { vName, vCfg = VmCfg {..}, vSysCfg = VmSysCfg {..}, vNetCfg = VmNetCfg {..}, vQCfg = VmQCfg {..} }, .. } = concat $ [
  [arch vArch],
  ["-sandbox", "on"], -- seccomp yey
  ["-cpu", "host"],
  ["-machine", "pc,accel=kvm"],
  ["-nographic"],
  ["-vga", "none"],
  ["-option-rom", "/usr/share/qemu/sgabios.bin"],
  case qStdioConsole of
    True -> []
    False -> concat [
      ["-monitor", "unix:"++ (rundir </> "kib-" ++ vName </> "monitor.unix") ++ ",server,nowait"],
      ["-serial", "unix:"++ (rundir </> "kib-" ++ vName </> "ttyS0.unix") ++ ",server,nowait"],
      ["-qmp", "stdio"]
      ],
  smp vCpus,
  mem vMem,
  concat $ map (\(i, n) -> disk i ("/dev" </> vVg </> n) Nothing) $ [0..] `zip` (vName : map ((vName ++ "-")++) vAddDisks),
  ["-net", "none"],
  vUserIf    ==> userNet "virtio" 2 qUserIfOpts,
  vPublicIf  ==> net ("kpu-"++vName) "virtio" 0 mac,
  vPrivateIf ==> net ("kpr-"++vName) "virtio" 0 mac
   -- TODO: in theory MAC addresses only have to be unique per segment, since
   -- the private net is a seperate bridge we should be fine but who knows TODO:

   -- TODO: Group interfaces
 ]
 where
   True  ==> f = f
   False ==> _ = []

smp :: Int -> [String]
smp n = [ "-smp", show n ]

mem :: Int -> [String]
mem b = [ "-m", show b]

arch a = "/usr/bin/qemu-system-" ++ a

disk i file mfmt =
    [ "-drive",  opts [ ("file", file)
                      , ("id", "hdd" ++ show i)
                      , ("format", fromMaybe "raw" mfmt)
                      ]
    , "-device", "virtio-scsi-pci"
    ]

net ifname model vlan mac =
    [ "-net", "nic," ++ opts [ ("vlan",    show vlan)
                             , ("macaddr", showMAC mac)
                             , ("model",   model)
                             ]
    , "-net", "tap," ++ opts [ ("vlan",   show vlan)
                             , ("ifname", ifname)
                             , ("script", "no")
                             , ("downscript", "no")
                             ]
    ]

userNet model vlan adopts =
  [ "-net", "nic," ++ opts [ ("vlan",  show vlan)
                           , ("model", model)
                           ]
  , "-net", "user," ++ opts (("vlan", show vlan):adopts)
  ]

opts = intercalate "," . map (\(k,v) -> k++"="++v)
