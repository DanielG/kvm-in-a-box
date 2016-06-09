module Udev where

import Types
import Resource
import Control.Applicative
import Data.List
import Data.List.Split
import Data.Maybe


lvmOwnerResources :: [Vm] -> SomeResource
lvmOwnerResources vms = SomeResource $ FileResource {
    rPath = "/etc/udev/rules.d/60-kib.rules",
    rPerms = ((Nothing, Nothing), Just "644"),
    rNormalize = unlines . sort . lines,
    rParse = map own . parse,
    rUnparse = unparse,
    rContentFunc = const $ map own $ concatMap rs vms
  }

 where
   rs Vm { vName=vmn, vSysCfg=VmSysCfg {vVg=vg, vAddDisks=addDisks} } =
       rule vg vmn Nothing : map (rule vg vmn) (map Just addDisks)

   rule vg vmn mdisk =
       [ ("ENV{DM_VG_NAME}==", qt vg)
       , ("ENV{DM_LV_NAME}==", qt $ vmn ++ fromMaybe "" (("-"++) <$> mdisk))
       , ("OWNER=", qt $ "kib-" ++ vmn)
       ]

splitKV = split (condense $ endsWith "=")

qt str = "\"" ++ str ++ "\""

own :: [(String, String)] -> (ResourceOwner, [(String, String)])
own x
    | Just vg <- lookup "ENV{DM_VG_NAME}" x
    , Just lv <- lookup "ENV{DM_LV_NAME}" x
    , Just owner <- lookup "OWNER" x
    , "kib":vmn:_ <- splitOn "-" owner
    = (OwnerVm vmn, x)

-- @(map snd -> [vg, lv, 'k':'i':'b':'-':vmn]) = (OwnerVm vmn, x)
own x = (OwnerSystem, x)

parse :: String -> [[(String, String)]]
parse = map (map var . words) . lines

unparse :: [[(String, String)]] -> String
unparse = unlines . map (unwords . map unvar)

var str = let [k,v] = splitKV str in (k,v)
unvar (k,v) = k ++ v
