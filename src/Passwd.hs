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
    } deriving (Show)

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
    } deriving (Show)

data GroupEntry = GroupEntry {
      geName     :: String,
      gePassword :: String,
      geGID      :: String,
      geUserList :: [String]
    } deriving (Show)

passwdResource :: [VmName] -> PX.GroupEntry -> Resource
passwdResource vmns kibGrp = ManyResources [
  FileResource {
    rPath = etcdir </> "passwd",
    rNormalize =
        \str -> unparsePwd $ filter (isKibUser peLoginName) $ parsePwd str,

    rParse = map markPwd . parsePwd,
    rUnparse = unparsePwd,

    rContentFunc = map markPwd . passwdDb vmns kibGrp . map snd
  },
  FileResource {
    rPath = etcdir </> "shadow",
    rNormalize =
        \str -> unparseShd $ filter (isKibUser seLoginName) $ parseShd str,
    rParse = map markShd . parseShd,
    rUnparse = unparseShd,

    rContentFunc = map markShd . shadowDb vmns . map snd
  },
  FileResource {
    rPath = etcdir </> "group",
    -- TODO: filter out kib users from group members
    rNormalize =
        \str -> unparseGrp $ filter ((=="kvm") . geName) $ parseGrp str,
    rParse = map markGrp . parseGrp,
    rUnparse = unparseGrp,

    rContentFunc = map markGrp . groupDb vmns . map snd
  }
 ]

markPwd x
    | isKibUser peLoginName x = (OwnerKib, x)
    | otherwise = (OwnerSystem, x)

markShd x
    | isKibUser seLoginName x = (OwnerKib, x)
    | otherwise = (OwnerSystem, x)

markGrp x
    | geName x == "kvm" = (OwnerKib, x)
    | otherwise = (OwnerSystem, x)


isKibUser u = ("kib-" `isPrefixOf`) . u

unUid (CUid x) = x
unGid (CGid x) = x

passwdDb vmns kibGrp db = let
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
    oldVms = [ (vmn, uid)
             | (vmn, uid) <- kibs
             , vmn' <- vmns
             , vmn == vmn'
             ]

    newUsers = map (uncurry passwd) $ newVms `zip` [nextUid..]
    oldUsers = map (uncurry passwd) oldVms

  in
    (others ++ oldUsers ++ newUsers)
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
           peShell = "/usr/sbin/kib-console"
       }

shadowDb vmns db = let
    (others, _kib) = partitionEithers $ map kibUser db
  in
    others ++ map shadow vmns
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

groupDb vmns db = let
    ([kvm], others) = partition ((=="kvm") . geName) db
  in
    others ++ [kvm { geUserList = map ("kib-"++) vmns }]

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
