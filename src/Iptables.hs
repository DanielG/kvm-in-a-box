{-# LANGUAGE FlexibleContexts, FlexibleInstances,
    ScopedTypeVariables, TypeFamilies,
    DeriveDataTypeable, DeriveFunctor #-}
module Iptables where

import Control.Applicative (Applicative, (<$>), (<*>), (<*), (*>), (<$))
import Control.Monad
import Control.Arrow
import Control.Exception.Base
import Data.IP
import Data.Bool
import Data.Function
import Data.Traversable (traverse)
import Data.Data
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word
import Data.Either
-- import Data.Witherable (wither)
import Data.Bifunctor hiding (first, second)
import Text.Parsec hiding (parse)
import Text.Parsec.Char
import Text.Parsec.Prim hiding (parse)
import Text.Parsec.Combinator
import System.FilePath

import Debug.Trace

import ParserUtils
import MapJoin
import Resource
import Utils
import Files
import Types
import MAC
import IP

type Tables r = TAList (CAList ISPolicy, CAList r)

data IptablesSave r = IptablesSave {
      isComments :: [Maybe String]
    , isTables   :: Tables r
    }
    deriving (Eq, Show, Data, Functor, Typeable)

data ISPolicy = ISP (Maybe String) Integer Integer
    deriving (Eq, Show, Data, Typeable)

type ISRule = [String]

type Ix = ((String, String), Maybe ISPolicy)

instance IxFunctor Ix IptablesSave where
    imap f (IptablesSave cs tm) =
        IptablesSave cs $ flip map tm $ \(tn, (pm, cm)) ->
          (tn,) $ (pm,) $ flip map cm $ \(cn, rs) ->
            (cn,) $ f ((tn, cn), lookup cn pm) rs

instance IxFoldable Ix IptablesSave where
    ifoldr f b (IptablesSave cs tm) =
      flip2both foldr tm b $ \(tn, (pm, cm)) b ->
        flip2both foldr cm b $ \(cn, a) b' ->
          f ((tn, cn), lookup cn pm) a b'

instance FromOwned (IS r) where
    type Owned (IS r) = IS (ResourceOwner, r)
    disown = fmap snd
    owners _ (IS is) = map fst $ concat $ ifoldr (const (:)) [] is
    filterOwner _ p (IS (IptablesSave cs ts)) =
        IS $ IptablesSave cs $
           flip map ts $ second $ second $ map $ second $ filter (p . fst)

newtype IS r = IS { unIS :: IptablesSave [r] }
    deriving (Functor)

iptablesResource :: Config -> Interface -> [Vm] -> Map VmName (MAC, IPv4) -> ManyResources
iptablesResource cfg pubif vms hosts =
    ManyResources [ table IPvv4, table IPvv6 ]

 where
   table ipv = SomeResource $ FileResource {
                 rNormalize = id, --unparse . parse,
                 rPath = etcdir </> ("iptables/rules." ++ sipv),
                 rPerms = ((Just "root", Just "root"), Just "7755"),
                 rParse = markOurs . IS . parse,
                 rUnparse = unparse . unIS,
                 rContentFunc = markOurs . updateIS . fmap disown
               }
       where
         sipv = case ipv of IPvv4 -> "v4"; IPvv6 -> "v6"
         updateIS :: Maybe (IS ISRule) -> IS ISRule
         updateIS =
           IS . updateTables ipv cfg pubif vms hosts . fromMaybe (IptablesSave [] defaultTables) . fmap unIS

updateTables :: IPv
             -> Config
             -> Interface
             -> [Vm]
             -> Map VmName (MAC, IPv4)
             -> IptablesSave [ISRule]
             -> IptablesSave [ISRule]
updateTables ipv Config {..} pubif vms hosts = insertKib . removeKib
 where
  insertKib :: IptablesSave [ISRule] -> IptablesSave [ISRule]
  insertKib = insertKibJumps . insertKibChains

  insertKibJumps :: IptablesSave [ISRule] -> IptablesSave [ISRule]
  insertKibJumps = flip insertAcceptRulesIntoTables jumpTable

  jumpTable :: Tables [ISRule]
  jumpTable = defaultTables' jumpPolicy jumpRule
    where
      jumpRule c = (c, [["-A", c, "-j", "KIB_" ++ c]])
      jumpPolicy c = Just ("KIB_" ++ c, ISP Nothing 0 0)

  -- insertKibChainPolicies :: IptablesSave [ISRule] -> IptablesSave [ISRule]
  -- insertKibChainPolicies = flip replaceOrInsertPolicy $
  --   where
  --     jumpRule c = ("KIB_" ++ c, [])
  --     jumpPolicy c = ("KIB_" ++ c, Just $ ISP Nothing 0 0)

  insertKibChains :: IptablesSave [ISRule] -> IptablesSave [ISRule]
  insertKibChains = case ipv of
    IPvv4 -> flip replaceOrInsertChain $ [
        ("nat",    [ ("KIB_PREROUTING", prerouting_chain "KIB_PREROUTING")
                   , ("KIB_POSTROUTING", [[ "-A", "KIB_POSTROUTING", "-s", "10.0.0.0/16", "-o", cInterface, "-j", "MASQUERADE" ]])
                   ]),
        ("filter", [ ("KIB_FORWARD", forwards_chain "KIB_FORWARD")
                   , ("KIB_INPUT", input_chain "KIB_INPUT")
                   ])
      ]
    IPvv6 -> flip replaceOrInsertChain $ [
        ("filter", [("KIB_FORWARD", forwards_chain "KIB_FORWARD")])
      ]

  prerouting_chain :: String -> [ISRule]
  prerouting_chain chain = flip concatMap vms $ \Vm { vNetCfg = VmNetCfg {..}, .. } ->
    flip map vForwardedPorts4 $ \(unProto -> proto, (iport,eport)) -> let
        Just ip = snd <$> Map.lookup vName hosts
      in
        [ "-A", chain, "-i", cInterface, "-p", proto, "-m", proto, "--dport", show eport
        , "-j", "DNAT", "--to-destination", showIP ip ++ ":" ++ show iport ]

  forwards_chain :: String -> [ISRule]
  forwards_chain chain = flip map forwards $ \(inif, outif) ->
    [ "-A", chain, "-i", inif, "-o", outif, "-j", "ACCEPT" ]

  forwards :: [(String, String)]
  forwards = [ (cInterface, unIface pubif)
             , (unIface pubif, cInterface)
             ]

  input_chain :: String -> [ISRule]
  input_chain chain = zipWith (input_rule chain) (repeat (unIface pubif)) inputs_from_pub

  input_rule chain iface (unProto -> proto, port) =
    [ "-A", chain
    , "-i", iface
    , "-p", proto, "-m", proto
    , "--dport", show port
    , "-j", "ACCEPT"
    ]

  -- allow dns and DHCP
  inputs_from_pub :: [(Proto, String)]
  inputs_from_pub = [ (UDP, "67:68")
                    , (UDP, "53")
                    ]

defaultTables = defaultTables' accept empty
 where
   accept c = Just $ (c, ISP (Just "ACCEPT") 0 0)
   empty  c = (c, [])


defaultTables' :: (String -> Maybe (Chain, ISPolicy))
               -> (String -> (Chain, [r]))
               -> Tables [r]
defaultTables' pf rf =
    [ ("nat"   , ( mapMaybe pf nat_chains
                 , map rf  nat_chains
                 )
      )

    -- , ("mangle", ( mapMaybe pf mangle_chains
    --              , map rf  mangle_chains
    --              )
    --   )

    , ("filter", ( mapMaybe pf filter_chains
                 , map rf  filter_chains
                 )
      )
    ]
 where
   nat_chains    = [ "INPUT"
                   , "OUTPUT"
                   , "POSTROUTING"
                   , "PREROUTING"
                   ]

   -- mangle_chains = [ "PREROUTING"
   --                 , "INPUT"
   --                 , "FORWARD"
   --                 , "OUTPUT"
   --                 , "POSTROUTING"
   --                 ]

   filter_chains = [ "INPUT"
                   , "FORWARD"
                   , "OUTPUT"
                   ]

isKibChain = ("KIB_" `isPrefixOf`)

markOurs :: IS ISRule -> IS (ResourceOwner, ISRule)
markOurs is = ft <$> is
 where
   isKib' r | isKibChain (extractChain r) = True
            | otherwise = False

   ft r | isKib' r  = (OwnerKib, r)
        | otherwise = (OwnerSystem, r)

removeKib :: IptablesSave [ISRule] -> IptablesSave [ISRule]
removeKib = imap (\ix -> filter (not . isKib ix))

justKib :: IptablesSave [ISRule] -> IptablesSave [ISRule]
justKib = imap (\ix -> filter (isKib ix))

isKib :: ((String, String), Maybe ISPolicy) -> ISRule -> Bool
isKib ((_, cn), _) r =
  isKibChain cn || fromMaybe False (isKibChain <$> extractTarget r)

parse :: String -> IptablesSave [ISRule]
parse str =
    case runParser (parseIptablesSave <* eof) () "<iptables-save>" str of
      Left err -> error (show err)
      Right a -> a

replaceOrInsertPolicy :: IptablesSave [ISRule] -> TCAList ISPolicy -> IptablesSave [ISRule]
replaceOrInsertPolicy (IptablesSave cs ts) tcal = IptablesSave cs $
    unionAListWith (\(ps, _) (ps', _) -> (unionAListWith (\p p' -> p') ps ps', [])) ts (map (second (,[])) tcal)

replaceOrInsertChain :: IptablesSave [ISRule] -> TCAList [ISRule] -> IptablesSave [ISRule]
replaceOrInsertChain (IptablesSave cs ts) tcal = IptablesSave cs $
    unionAListWith (\(ps, cs) (_, cs') -> (ps, unionAListWith (\c c' -> c') cs cs')) ts (map (second ([],)) tcal)

replaceOrInsertChains :: IptablesSave [ISRule] -> TCAList [ISRule] -> IptablesSave [ISRule]
replaceOrInsertChains (IptablesSave cs ts) tcal = IptablesSave cs $
    unionAListWith (\(ps, cs) (_, cs') -> (ps, cs')) ts (map (second ([],)) tcal)

-- (second (unionAListWith (++)))

prop_replaceOrInsertChains1 :: Bool
prop_replaceOrInsertChains1 =
    replaceOrInsertChains tc1 tc2 == tc3
 where
   tc1 = IptablesSave [] $ [("filter", ([], [("INPUT", [["-A", "INPUT", "-j", "REJECT"]])]))]

   tc2 = [("filter", [("INPUT", [["-A", "INPUT", "-j", "ACCEPT"]])])]
   tc3 = IptablesSave [] $ [("filter", ([], [("INPUT", [["-A", "INPUT", "-j", "ACCEPT"]])]))]

prop_replaceOrInsertChains2 :: Bool
prop_replaceOrInsertChains2 =
    replaceOrInsertChains tc1 tc2 == tc3
 where
   tc1 = IptablesSave [] [ ("filter", ([], [("INPUT",  [["-A", "INPUT", "-j", "TEST1"]])]))
                         , ("nat"   , ([], [("OUTPUT", [["-A", "OUTPUT", "-j", "TEST2"]])]))
                         ]

   tc2 =                 [ ("nat", [ ("OUTPUT", [["-A", "OUTPUT", "-j", "TEST3"]])
                                   , ("NOEX", [["-A", "NOEX", "-j", "TEST4"]])
                                   ])]

   tc3 = IptablesSave [] [ ("filter", ([], [("INPUT",  [["-A", "INPUT", "-j", "TEST1"]])]))
                         , ("nat"   , ([], [ ("OUTPUT", [["-A", "OUTPUT", "-j", "TEST3"]])
                                           , ("NOEX", [["-A", "NOEX", "-j", "TEST4"]])]))
                         ]

insertAcceptRulesIntoTables
    :: IptablesSave [ISRule] -> Tables [ISRule] -> IptablesSave [ISRule]
insertAcceptRulesIntoTables (IptablesSave cs ts) ts' =
    IptablesSave cs $ unionAListWith (\(ps, cs) (ps', cs') -> (unionAListWith (\p p' -> p') ps ps', insertAcceptRulesIntoChains cs cs')) ts ts'

prop_insertAcceptRulesIntoTables :: Bool
prop_insertAcceptRulesIntoTables =
    insertAcceptRulesIntoTables c1 c2 == c3

 where
   c1 = IptablesSave [] $ (:[]) $ ("filter",) $ ([],) $ (:[]) $ (,) "INPUT" $ [
         ["-A", "INPUT", "-j", "ACCEPT"],
         ["-A", "INPUT", "-j", "ACCEPT"],
         ["-A", "INPUT", "-j", "DROP"],
         ["-A", "INPUT", "-j", "REJECT"]
        ]

   c2 = (:[]) $ ("filter",) $ ([],) $ (:[]) $ (,) "INPUT" $ [
         ["-A", "INPUT", "-j", "KIB_INPUT"]
        ]

   c3 = IptablesSave [] $ (:[]) $ ("filter",) $ ([],) $ (:[]) $ (,) "INPUT" $ [
         ["-A", "INPUT", "-j", "ACCEPT"],
         ["-A", "INPUT", "-j", "ACCEPT"],
         ["-A", "INPUT", "-j", "KIB_INPUT"],
         ["-A", "INPUT", "-j", "DROP"],
         ["-A", "INPUT", "-j", "REJECT"]
        ]


insertAcceptRulesIntoChains
    :: CAList [ISRule] -> CAList [ISRule] -> CAList [ISRule]
insertAcceptRulesIntoChains = unionAListWith mergeRules
 where
   mergeRules :: [ISRule] -> [ISRule] -> [ISRule]
   mergeRules rs rs' = let
       (accept, nonAccept) = span isAcceptOrNop rs
     in
       accept ++ rs' ++ nonAccept

   isAcceptOrNop args =
       extractTarget args `elem` [Just "ACCEPT", Nothing]

prop_insertAcceptRulesIntoChains :: Bool
prop_insertAcceptRulesIntoChains =
    insertAcceptRulesIntoChains c1 c2 == c3

 where
   c1 = (:[]) $ (,) "INPUT" $ [
         ["-A", "INPUT", "-j", "ACCEPT"],
         ["-A", "INPUT", "-j", "ACCEPT"],
         ["-A", "INPUT", "-j", "DROP"],
         ["-A", "INPUT", "-j", "REJECT"]
        ]

   c2 = (:[]) $ (,) "INPUT" $ [
         ["-A", "INPUT", "-j", "KIB_INPUT"]
        ]

   c3 = (:[]) $ (,) "INPUT" $ [
         ["-A", "INPUT", "-j", "ACCEPT"],
         ["-A", "INPUT", "-j", "ACCEPT"],
         ["-A", "INPUT", "-j", "KIB_INPUT"],
         ["-A", "INPUT", "-j", "DROP"],
         ["-A", "INPUT", "-j", "REJECT"]
        ]

extractTarget ("-j":chain:rest) = Just chain
extractTarget ("-g":chain:rest) = Just chain
extractTarget (arg:rest) = extractTarget rest
extractTarget [] = Nothing

type Chain = String
type Table = String

type TCAList a = AList Table (AList Chain a)

type TAList a = AList Table a
type CAList a = AList Chain a

unparse :: IptablesSave [ISRule] -> String
unparse (IptablesSave mcs ts) =
    unlines $ concatMap (either goCM (concatMap goT)) $ unmpartition mcs $ map (:[]) ts
 where
   goCM s = lines s

   goT :: (Table, (CAList ISPolicy, CAList [ISRule])) -> [String]
   goT (tn, (ps, cs)) = concat
                    [ ['*':tn]
                    , map (uncurry goP) ps
                    , concatMap (map unwords) $ map snd cs
                    , ["COMMIT"]
                    ]

   goP :: Chain -> ISPolicy -> String
   goP c (ISP mp rx tx) =
       unwords [ ':':c
               , fromMaybe "-" mp
               , concat [ "[", show rx, ":", show tx, "]"]
               ]

parseIptablesSave :: Parsec String u (IptablesSave [ISRule])
parseIptablesSave = do
    let
        parseTable :: Parsec String u (Tables [ISRule])
        parseTable = do
          tn <- parseTableHead
          ps <- parsePolicies
          rs <- parseRules
          return [(tn, (ps, rs))]

    ects :: [Either String (Tables [ISRule])]
       <- many (  (Left  <$> parseComments)
              <|> (Right <$> parseTable)
               )
    let
        mcs :: [Maybe String]
        ts  :: [Tables [ISRule]]
        (mcs, ts) = mpartition ects
        ts' :: Tables [ISRule]
        ts' = unionsAList ts

    return $ IptablesSave mcs ts'

twace :: Show a => String -> a -> a
twace l a = trace (l ++ ": " ++ show a) a


parseComments = concat <$> many1
  (char '#' *> ((++"\n") . ('#':) <$> many nonNewline) <* newline)

parseTableHead :: Parsec String u String
parseTableHead =
  char '*' *> many1 parseChainIdentChar <* newline

parsePolicies :: Parsec String u (AList Chain ISPolicy)
parsePolicies = many1 parsePolicy

parseRules :: Parsec String u (AList Chain [ISRule])
parseRules = do
  rs <- catMaybes <$> many ((Just <$> parseRule) <|> (Nothing <$ parseComments))
  parseCommit
  return $ map (second (map snd)) $ groupByK fst (==) $ map (extractChain &&& id) rs

extractChain ("-A":chain:rest) = chain
extractChain (arg:rest) = extractChain rest
extractChain [] = error "extractChain: no chain found"

parsePolicy :: Parsec String u (String, ISPolicy)
parsePolicy = do
  char ':'
  chain <- many1 parseChainIdentChar
  nnspaces
  mpolicy <- (Just <$> many1 alphaNum) <|> (Nothing <$ char '-')
  nnspaces
  char '['
  rx <- readNote "parsePolicy: digit1" <$> many1 digit
  char ':'
  tx <- readNote "parsePolicy: digit2" <$> many1 digit
  char ']'
  skipMany nonNewline
  newline
  return $ (chain, ISP mpolicy rx tx)

parseRule :: Parsec String u ISRule
parseRule = do
  nnspaces
  rule <- (:) <$> char '-' <*> many1 nonNewline
  newline
  return $ splitOn " " rule

parseCommit :: Parsec String u ()
parseCommit = do
  nnspaces
  string "COMMIT"
  newline
  return ()

parseChainIdentChar = alphaNum <|> Text.Parsec.Char.oneOf "-_"
