module Qemu where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List
import System.FilePath

import Types

smp :: Int -> [String]
smp n = [ "-smp", show n ]

mem :: Int -> [String]
mem b = [ "-m", show b]

arch a = "qemu-system-" ++ a

disk i file mfmt =
    [ "-drive",  opts [ ("file", file)
                      , ("id", "hdd" ++ show i)
                      , ("format", fromMaybe "raw" mfmt)
                      ]
    , "-device", "virtio-scsi-pci"
    ]

net model ifname vlan mac =
    [ "-net nic," ++ opts [ ("vlan", show vlan)
                          , ("macaddr", mac)
                          , ("model",  model)
                          ]
    , "-net tap," ++ opts [ ("vlan", show vlan)
                          , ("ifname", ifname)
                          , ("script", "no")
                          , ("downscript", "no")
                          ]
    ]

opts = intercalate "," . map (\(k,v) -> k++"="++v)

qemu vardir Vm {..} VmVolatileState {..} = concat $ [
  [arch vArch], -- qemu command
  ["-cpu", "host"],
--  ["-machine", "pc,accel=kvm"],
  ["-nographic"],
  ["-vga", "none"],
  ["-option-rom", "/usr/share/qemu/sgabios.bin"],
  ["-monitor", ""],
  ["-serial", "unix:"++ (vardir </> "ttyS0.unix") ++ "server,nowait"],
  smp vCpus,
  mem vMem,
  disk 0 ("/dev/kib" </> vName) Nothing,
  join vPublicIf  ==> net "virtio" ("kipubr-"++vName) 0
--  vPrivateIf ==> net 0 vMac "virtio" ("kiprbr-"++vName),
-- TODO: Group interfaces
 ]

 where
   Just mac  ==> f = f (unMac mac)
   Nothing   ==> _ = []
