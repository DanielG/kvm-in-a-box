module Passwd (passwdResource) where

import Control.Arrow
import Data.List
import Data.List.Split
import Data.Either
import Data.Word
import Data.Maybe

import System.FilePath
import System.Posix.Types
import System.Posix.User hiding (GroupEntry(..))
import qualified System.Posix.User as PX


import Types
import Resource
import Config

data PasswdEntry = PasswdEntry {
      peLoginName :: String,
      pePassword  :: String,
      peUID       :: CUid,
      peGID       :: CGid,
      peName      :: String,
      peHome      :: FilePath,
      peShell     :: String
    }

data ShadowEntry = ShadowEntry {
      seLoginName         :: String,
      seEncryptedPassword :: String,
      seLastChange        :: String,
      seMinAge            :: String,
      seMaxAge            :: String,
      seWarningPeriod     :: String,
      seInactivityPeriod  :: String,
      seExpirationDate    :: String,
      seReserved          :: String
    }

data GroupEntry = GroupEntry {
      geName     :: String,
      gePassword :: String,
      geGID      :: String,
      geUserList :: [String]
    }

passwdResource :: [VmName] -> PX.GroupEntry -> Resource
passwdResource vmns kibGrp = ManyResources [
  FileResource {
    rNormalize =
        \str -> unparsePwd $ filter (isKibUser peLoginName) $ parsePwd str,
    rPath = etcdir </> "passwd",
    rContent = passwdDb vmns kibGrp
  },
  FileResource {
    rNormalize =
        \str -> unparseShd $ filter (isKibUser seLoginName) $ parseShd str,
    rPath = etcdir </> "shadow",
    rContent = shadowDb vmns
  },
  FileResource {
    -- TODO: filter out kib users from group members
    rNormalize =
        \str -> unparseGrp $ filter ((=="kvm") . geName) $ parseGrp str,
    rPath = etcdir </> "group",
    rContent = groupDb vmns
  }
 ]

isKibUser u = ("kib-" `isPrefixOf`) . u

unUid (CUid x) = x
unGid (CGid x) = x

passwdDb vmns kibGrp str = let
    db = parsePwd str

    nextUid' :: UserID
    nextUid' = (+1) $ foldr max 4999
                    $ filter (< 65534)
                    $ filter (>=5000)
                    $ map peUID db

    nextUid = if nextUid' >= 65533
                then error "Out of UIDs!"
                else nextUid'

    others :: [PasswdEntry]
    kibs :: [(VmName, CUid)]
    (others, kibs) = partitionEithers $ map kibUser db

    newVms = filter (isNothing . flip lookup kibs) vmns

    newUsers = map (uncurry passwd) $ newVms `zip` [nextUid..]
    oldUsers = map (uncurry passwd) kibs

  in
    unparsePwd $ (others ++ oldUsers ++ newUsers)
 where
   kibUser e@(peLoginName -> u)
       | ('k':'i':'b':'-':vmn) <- u = Right (vmn, peUID e)
       | otherwise = Left e

   passwd :: VmName -> CUid -> PasswdEntry
   passwd vmn uid  =
       PasswdEntry {
           peLoginName = ("kib-"++vmn),
           pePassword = "x",
           peUID = uid,
           peGID = PX.groupID kibGrp,
           peName = "",
           peHome = varlibdir </> vmn,
           -- TODO: make shell be socat wrapper
           peShell = "/bin/false"
       }


shadowDb vmns str = let
    db = parseShd str
    (others, _kib) = partitionEithers $ map kibUser db
  in
    unparseShd $ others ++ map shadow vmns
 where
   kibUser e@(seLoginName -> u)
       | ('k':'i':'b':'-':vmn) <- u = Right vmn
       | otherwise = Left e

   shadow :: VmName -> ShadowEntry
   shadow vmn =
       ShadowEntry {
           seLoginName         = "kib-"++vmn,
           seEncryptedPassword = "*",
           seLastChange        = "",
           seMinAge            = "",
           seMaxAge            = "",
           seWarningPeriod     = "",
           seInactivityPeriod  = "",
           seExpirationDate    = "",
           seReserved          = ""
       }

groupDb vmns str = let
    ([kvm], others) = partition ((=="kvm") . geName) $ parseGrp str
  in
    unparseGrp $ others ++ [kvm { geUserList = nub $ geUserList kvm ++ map ("kib-"++) vmns }]

unparse :: (a -> String) -> [a] -> String
unparse fn = unlines . map fn

unparsePwd = unparse unpwent
unparseShd = unparse unshent
unparseGrp = unparse ungrent

unpwent (PasswdEntry l p u g n h s) =
    intercalate ":" [l, p, showUid u, showGid g, n, h, s]
 where
   showUid (CUid x) = show x
   showGid (CGid x) = show x

unshent (ShadowEntry l p c mi ma w i e r) =
    intercalate ":" [l, p, c, mi, ma, w, i, e, r]

ungrent (GroupEntry n p i us) =
    intercalate ":" [n, p, i, intercalate "," us]

parse :: (String -> a) -> String -> [a]
parse pl str = map pl $ lines str

parsePwd = parse parsePwdLine
parseShd = parse parseShdLine
parseGrp = parse parseGrpLine

parsePwdLine :: String -> PasswdEntry
parsePwdLine li = let [l, p, u, g, n, h, s] = splitOn ":" li
                  in PasswdEntry l p (CUid $ read u) (CGid $ read g) n h s

parseShdLine :: String -> ShadowEntry
parseShdLine li = let [l, p, c, mi, ma, w, i, e, r] = splitOn ":" li
                  in ShadowEntry l p c mi ma w i e r

parseGrpLine :: String -> GroupEntry
parseGrpLine li = let [n, p, i, splitOn "," -> us] = splitOn ":" li
                  in GroupEntry n p i us

notExists l = null . filter ((==l) . peLoginName)
