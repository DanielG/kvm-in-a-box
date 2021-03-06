module Passwd (passwdResource) where

import Safe
import Control.Arrow
import Data.List
import Data.List.Split
import Data.Either
import Data.Word
import Data.Char
import Data.Maybe
import Data.Function
import Data.Functor

import System.FilePath
import System.Posix.Types
import System.Posix.User hiding (GroupEntry(..))
import qualified System.Posix.User as PX

import Types
import Resource
import Files

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
      geGID      :: CGid,
      geUserList :: [String]
    } deriving (Show)

passwdResource :: [VmName] -> PX.GroupEntry -> ManyResources
passwdResource vmns kibGrp = ManyResources $ [
  SomeResource $ FileResource {
    rPath = etcdir </> "passwd",
    rPerms = ((Nothing, Nothing), Just "644"),
    rNormalize =
        \str -> unparsePwd $ filter (isKibUser peLoginName) $ parsePwd str,

    rParse = map markPwd . parsePwd,
    rUnparse = unparsePwd :: [PasswdEntry] -> String,

    rContentFunc = map markPwd . makePasswdDb vmns kibGrp . map snd . fromJustNote "passwdResource:passwd file missing"
  },
  SomeResource $ FileResource {
    rPath = etcdir </> "shadow",
    rPerms = ((Nothing, Nothing), Just "600"),
    rNormalize =
        \str -> unparseShd $ filter (isKibUser seLoginName) $ parseShd str,
    rParse = map markShd . parseShd,
    rUnparse = unparseShd :: [ShadowEntry] -> String,

    rContentFunc = map markShd . makeShadowDb vmns . map snd . fromJustNote "passwdResource:shadow file missing"
  },
  SomeResource $ FileResource {
    rPath = etcdir </> "group",
    rPerms = ((Nothing, Nothing), Just "644"),
    -- TODO: filter out kib users from group members
    rNormalize =
        \str -> unparseGrp $ filter ((=="kvm") . geName) $ parseGrp str,
    rParse = map markGrp . parseGrp,
    rUnparse = unparseGrp :: [GroupEntry] -> String,

    rContentFunc = map markGrp . makeGroupDb vmns . map snd . fromJustNote "passwdResource:group file missing"
  }
 ] ++ map homeDirectoryResource vmns

homeDirectoryResource :: VmName -> SomeResource
homeDirectoryResource vmn =
    SomeResource $ DirectoryResource {
      fmrPath = homedir </> "kib-" ++ vmn,
      fmrPerms = ((Just $ "kib-" ++ vmn, Just "kib"), Just "755"),
      fmrOwner = OwnerVm vmn
    }

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

checkId :: (Num a, Ord a) => a -> a
checkId uid =
    if uid >= 65533
      then error "Out of U/GIDs!"
      else uid

nextKibId = nextId
nextId uids = checkSystemId uids $
    (+1) $ foldr max 4999
         $ filter (< 65534)
         $ filter (>=5000) uids

checkSystemId :: (Num a, Ord a) => [a] -> a -> a
checkSystemId uids uid =
    if uid >= 65533 || uid `elem` uids
      then error "Out of system U/GIDs!"
      else uid

nextSystemId uids = checkId $
    (+1) $ foldr max 99
         $ filter (< 1000)
         $ filter (>=100) uids

makePasswdDb vmns kibGrp db = let
    nextUid :: UserID
    nextUid = nextId $ map peUID db

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
           peHome = homedir </> "kib-" ++ vmn,
           peShell = "/usr/sbin/kib-console"
       }

makeShadowDb vmns db = let
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

makeGroupDb :: [String] -> [GroupEntry] -> [GroupEntry]
makeGroupDb vmns db = let
    gid0 = nextSystemId $ map geGID db
    gid1 = nextSystemId $ map geGID (db ++ [kvmGrp])

    defKvmGrp = GroupEntry "kvm" "x" gid0 []
    kvmGrp = fromMaybe defKvmGrp $ clearMembers <$> find ((=="kvm") . geName) db

    defKibGrp = GroupEntry "kib" "x" gid1 []
    kibGrp = fromMaybe defKibGrp $ clearMembers <$> find ((=="kib") . geName) db


    clearMembers = modifyGeUsers $ filter (not . ("kib-" `isPrefixOf`))
    addMembers = modifyGeUsers $ \users -> nub $ users ++ map ("kib-"++) vmns

  in
    reverse $ nubBy ((==) `on` geName) $ reverse (map addMembers [kvmGrp, kibGrp]) ++ reverse db

  --   gid0 = nextId $ map geGID db
  --   groups0 = [ \gid -> GroupEntry "kvm" "" gid []
  --             , \gid -> GroupEntry "kib" "" gid []
  --             ]
  --   groups1 = map (uncurry ($)) $ groups0 `zip` (map checkId $ iterate (+1) gid0)
  --   groups2 = modifyGeElems addMembers ["kvm", "kib"] groups1
  -- in
  --   reverse $ nubBy ((==) `on` geName) $ reverse groups2 ++ reverse db

modifyGeUsers f ge = ge { geUserList = f (geUserList ge) }

modifyGeElems :: (GroupEntry -> GroupEntry) -> [String] -> [GroupEntry] -> [GroupEntry]
modifyGeElems f ks ges = map snd $ modifyElems f ks $ map (geName &&& id) ges

modifyElems :: Eq k => (a -> a) -> [k] -> [(k,a)] -> [(k,a)]
modifyElems f ks (ka@(k,a):kas)
    | [k] == ks = (k, f a):kas
    | k `elem` ks = (k, f a) : modifyElems f (drop 1 ks) kas
    | otherwise = ka : modifyElems f ks kas

unparse :: (a -> String) -> [a] -> String
unparse fn = unlines . map fn

unparsePwd = unparse unpwent
unparseShd = unparse unshent
unparseGrp = unparse ungrent

unpwent (PasswdEntry l p u g n h s) =
    intercalate ":" [l, p, showUid u, showGid g, n, h, s]

showUid (CUid x) = show x
showGid (CGid x) = show x

unshent (ShadowEntry l p c mi ma w i e r) =
    intercalate ":" [l, p, c, mi, ma, w, i, e, r]

ungrent (GroupEntry n p i us) =
    intercalate ":" [n, p, showGid i, intercalate "," us]

parse :: (String -> a) -> String -> [a]
parse pl str = map pl $ filter (not . all isSpace) $ lines str

parsePwd = parse parsePwdLine
parseShd = parse parseShdLine
parseGrp = parse parseGrpLine

parsePwdLine :: String -> PasswdEntry
parsePwdLine li = let [l, p, u, g, n, h, s] = splitOn ":" li
                  in PasswdEntry l p (CUid $ readNote "CUid" u) (CGid $ readNote "CGid" g) n h s

parseShdLine :: String -> ShadowEntry
parseShdLine li = let [l, p, c, mi, ma, w, i, e, r] = splitOn ":" li
                  in ShadowEntry l p c mi ma w i e r

parseGrpLine :: String -> GroupEntry
parseGrpLine li = let [n, p, i, splitOn "," -> us] = splitOn ":" li
                  in GroupEntry n p (readNote "CGid" i) us

notExists l = null . filter ((==l) . peLoginName)
