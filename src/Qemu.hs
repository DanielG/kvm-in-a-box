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
      ["-monitor", "unix:"++ (rundir </> "kib-" ++ vName ++ "-monitor.unix") ++ ",server,nowait"],
      ["-serial", "unix:"++ (rundir </> "kib-" ++ vName ++ "-ttyS0.unix") ++ ",server,nowait"],
      ["-qmp", "stdio"]
      ],
  smp vCpus,
  mem vMem,
  case vDriveDevice of
    "scsi-hd" -> ["-device", "virtio-scsi-pci"]
    _ -> [],
  concat $ map (\(i, n) -> disk i ("/dev" </> vVg </> n) vDriveDevice Nothing) $ [0..] `zip` (vName : map ((vName ++ "-")++) vAddDisks),
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

disk i file device mfmt =
    [ "-drive",  opts [ ("file", file)
                      , ("id", "hdd" ++ show i)
                      , ("format", fromMaybe "raw" mfmt)
                      , ("if", "none")
                      ]
    , "-device", device ++ "," ++ opts [ ("drive", "hdd" ++ show i) ]
    ]

net ifname model vlan mac =
    [ "-nic", "tap," ++
        opts [ ("mac", showMAC mac)
             , ("model",   model)
             , ("ifname", ifname)
             , ("script", "no")
             , ("downscript", "no")
             ]
    ]

userNet model vlan adopts =
  [ "-nic", "user," ++ opts (("model", model) : adopts)
  ]

opts = intercalate "," . map (\(k,v) -> k++"="++v)
