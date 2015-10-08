module Udev where

import Types
import Resource
import Data.List
import Data.List.Split
import Data.Maybe

lvmOwnerResources :: [Vm] -> Resource
lvmOwnerResources vms = FileResource {
    rPath = "/etc/udev/rules.d/99-kib.rules",
    rNormalize = unlines . sort . lines,
    rParse = map own . parse,
    rUnparse = unparse,
    rContentFunc = \ls -> nub $ fromMaybe [] ls ++ map (own . rs) vms
  }

 where
   rs Vm { vName=vmn, vSS=VmSS {vVg=vg} } =
       [ ("ENV{DM_VG_NAME}==", qt vg)
       , ("ENV{DM_LV_NAME}==", qt vmn)
       , ("OWNER=", qt $ "kib-" ++ vmn)
       ]

{-

  | forall a. FileResource {
      rPath        :: FilePath,
      rNormalize   :: String -> String,
      rParse       :: String -> [Owned a],
      rUnparse     :: [a] -> String,
      rContentFunc :: [Owned a] -> [Owned a]
    }

-}


splitKV = split (condense $ endsWith "=")

qt str = "\"" ++ str ++ "\""

own :: [(a, String)] -> (ResourceOwner, [(a, String)])
own x@(map snd -> [vg, lv, 'k':'i':'b':'-':vmn]) = (OwnerVm vmn, x)
own x = (OwnerSystem, x)

parse :: String -> [[(String, String)]]
parse = map (map var . words) . lines


unparse :: [[(String, String)]] -> String
unparse = unlines . map (unwords . map unvar)

var str = let [k,v] = splitKV str in (k,v)
unvar (k,v) = k ++ v
