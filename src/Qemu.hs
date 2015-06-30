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

qemuRunDirsResource vardir vmn = DirectoryResource {
                                   rPath  = vardir </> vmn,
                                   rOwner = "kib-" ++ vmn,
                                   rGroup = "kib"
                                 }

qemu vardir Vm { vName, vSS = VmSS {..}, vVS = VmVS {..} } mac = concat $ [
  [arch vArch],
  ["-cpu", "host"],
  ["-machine", "pc,accel=kvm"],
  ["-nographic"],
  ["-vga", "none"],
  ["-option-rom", "/usr/share/qemu/sgabios.bin"],
  ["-monitor", "unix:"++ (vardir </> vName </> "monitor.unix") ++ ",server,nowait"],
  ["-serial", "unix:"++ (vardir </> vName </> "ttyS0.unix") ++ ",server,nowait"],
  ["-qmp", "stdio"],
  smp vCpus,
  mem vMem,
  disk 0 ("/dev" </> vVg </> vName) Nothing,
  vPublicIf  ==> net "virtio" ("kipubr-"++vName) 0 mac
--  vPrivateIf ==> net 0 vMac "virtio" ("kiprbr-"++vName),
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

net model ifname vlan mac =
    [ "-net nic," ++ opts [ ("vlan", show vlan)
                          , ("macaddr", showMAC mac)
                          , ("model",  model)
                          ]
    , "-net tap," ++ opts [ ("vlan", show vlan)
                          , ("ifname", ifname)
                          , ("script", "no")
                          , ("downscript", "no")
                          ]
    ]

opts = intercalate "," . map (\(k,v) -> k++"="++v)
