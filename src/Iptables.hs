{-# LANGUAGE FlexibleContexts, FlexibleInstances,
    ScopedTypeVariables, TypeFamilies,
    DeriveDataTypeable, DeriveFunctor #-}
module Iptables where

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Control.Monad
import Control.Arrow
import Control.Exception.Base
import Data.Bool
import Data.Function
import Data.Data
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Map (Map)
import Data.Word
import Data.Either
-- import Data.Witherable (wither)
import Data.Bifunctor hiding (first, second)
import qualified Data.Map as Map
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

data IptablesSave r = IptablesSave {
      isComments :: [Maybe String]
    , isTables   :: TAList (CAList ISPolicy, CAList r)
    }
    deriving (Eq, Show, Data, Functor)

data ISPolicy = ISP (Maybe String) Integer Integer
    deriving (Eq, Show, Data)

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

-- instance IxTraversable Ix IptablesSave where
--     itraverse f = iwither (\i a -> Just <$> f i a)

-- instance IxWitherable Ix IptablesSave where
--     iwither f (IptablesSave cs tm) = IptablesSave cs <$>
--       flip itraverseAL tm (\tn (pm, cm) -> (pm,) <$>
--         wither (\(cn, rs) -> sequenceA . (cn,) <$>
--           (f ((tn, cn), lookup cn pm) rs)) cm)

itraverseAL :: Applicative f => (i -> a -> f b) -> AList i a -> f (AList i b)
itraverseAL f am = traverse (\(i, a) -> (i,) <$> f i a) am

instance FromOwned (IS r) where
    type Owned (IS r) = IS (ResourceOwner, r)
    disown = fmap snd
    owners _ (IS is) = map fst $ concat $ ifoldr (const (:)) [] is

newtype IS r = IS { unIS :: IptablesSave [r] }
    deriving (Functor)

ip6tablesResource :: Resource
ip6tablesResource = undefined -- FileResource {
    -- rNormalize = id, --unparse . parse,
    -- rPath = etcdir </> "iptables/rules.v6",
    -- rParse = markOurs . IS . parse,
    -- rUnparse = unparse . unIS,
    -- rContentFunc = markOurs . updateIS . fmap disown

-- markOurs . ( _ :: Maybe (IptablesSave [(ResourceOwner, ISRule)]) -> IptablesSave [(ResourceOwner, ISRule)]) . fmap disown

           -- $ fromMaybe (IptablesSave [] []) mois

-- IptablesSave $
--         updateISSTable "filter" (fromMaybe [] $ unIptablesSave <$> mis) $
--             Map.insert "KIB_INPUT" kib_input
--           . flip insertAcceptRulesIntoChains redir
--           . map removeKib
-- }
-- where

updateIS :: Maybe (IS ISRule) -> IS ISRule
updateIS =
    IS . updateTables . fromMaybe (IptablesSave [] defaultTables) . fmap unIS

updateTables :: IptablesSave [ISRule] -> IptablesSave [ISRule]
updateTables = insertKib . removeKib

insertKib :: IptablesSave [ISRule] -> IptablesSave [ISRule]
insertKib = insertKibJumps . insertKibChains

insertKibJumps :: IptablesSave [ISRule] -> IptablesSave [ISRule]
insertKibJumps = flip insertAcceptRulesIntoTables jumpTable

jumpTable :: TCAList [ISRule]
jumpTable = map (second snd) $ defaultTables' jumpPolicy jumpRule
  where
    jumpRule c = (c, [["-A", c, "-j", "KIB_" ++ c]])
    jumpPolicy _ = Nothing

insertKibChains :: IptablesSave [ISRule] -> IptablesSave [ISRule]
insertKibChains = id

kib_chains = []

   -- redir = Map.singleton "INPUT" $ ISC Nothing [
   --          ["-A", "INPUT", "-j", "KIB_INPUT"]
   --         ]
   -- kib_input = ISC Nothing [
   --              ["-A", "KIB_INPUT", "-j", "RETURN"]
   --             ]


defaultTables = defaultTables' accept empty
 where
   accept c = Just $ (c, ISP (Just "ACCEPT") 0 0)
   empty  c = (c, [])


defaultTables' :: (String -> Maybe (Chain, ISPolicy))
               -> (String -> (Chain, [r]))
               -> TAList (CAList ISPolicy, CAList [r])
defaultTables' pf rf =
    [ ("nat"   , ( mapMaybe pf nat_chains
                 , map rf  nat_chains
                 )
      )

    , ("mangle", ( mapMaybe pf mangle_chains
                 , map rf  mangle_chains
                 )
      )

    , ("filter", ( mapMaybe pf filter_chains
                 , map rf  filter_chains
                 )
      )
    ]
 where
   nat_chains    = [ "INPUT"
                   , "OUTPUT"
                   , "POSTROUTING"
                   ]

   mangle_chains = [ "PREROUTING"
                   , "INPUT"
                   , "FORWARD"
                   , "OUTPUT"
                   , "POSTROUTING"
                   ]

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

-- | @insertAcceptRulesIntoChains d s@. Rules in @s@ must not have a jump
-- target or have the target ACCEPT for the semantics of d to be
-- preserved. Assumes rules from @s@ are not already in @d@.


type TCCAList r = TAList (CAList ISPolicy, CAList r)

replaceOrInsertChains :: IptablesSave [ISRule] -> TCAList [ISRule] -> IptablesSave [ISRule]
replaceOrInsertChains (IptablesSave cs ts) tcal =
    IptablesSave cs $ unionAListWith (\(ps, cs) (_, cs') -> (ps, cs')) ts (map (second ([],)) tcal)

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
    :: IptablesSave [ISRule] -> TCAList [ISRule] -> IptablesSave [ISRule]
insertAcceptRulesIntoTables (IptablesSave ps ts) ts' =
    IptablesSave ps $ unionAListWith (\(ps, cs) (ps', cs') -> (ps, insertAcceptRulesIntoChains cs cs')) ts (map (second ([],)) ts')

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

   c2 = (:[]) $ ("filter",) $ (:[]) $ (,) "INPUT" $ [
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
   goCM s = [s]

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
        parseTable :: Parsec String u (TAList (CAList ISPolicy, CAList [ISRule]))
        parseTable = do
          tn <- parseTableHead
          ps <- parsePolicies
          rs <- parseRules
          return [(tn, (ps, rs))]

    ects :: [Either String (TAList (CAList ISPolicy, CAList [ISRule]))]
       <- many (  (Left  <$> parseComments)
              <|> (Right <$> parseTable)
               )
    let
        mcs :: [Maybe String]
        ts  :: [(TAList (CAList ISPolicy, CAList [ISRule]))]
        (mcs, ts) = mpartition ects
        ts' :: TAList (CAList ISPolicy, CAList [ISRule])
        ts' = unionsAList ts

    return $ IptablesSave mcs ts'

twace :: Show a => String -> a -> a
twace l a = trace (l ++ ": " ++ show a) a


parseComments = concat <$> many1
  (char '#' *> (('#':) <$> many nonNewline) <* newline)

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
  rx <- read <$> many1 digit
  char ':'
  tx <- read <$> many1 digit
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
