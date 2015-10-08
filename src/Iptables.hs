{-# LANGUAGE ViewPatterns, FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}
module Iptables where

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Control.Monad
import Control.Arrow
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Map (Map)
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

newtype IptablesSave r = IptablesSave [IS r]

data IS r = ISComment String | ISTable (ISTable r)
                  deriving (Eq, Show)
data ISTable r = IST { istName :: String, istChains :: [ISChain r] }
                  deriving (Eq, Show)
data ISChain r = ISC { iscName :: String, iscPolicy :: (Maybe ISPolicy), iscRules :: [r] }
                  deriving (Eq, Show)
data ISPolicy = ISP String String Integer Integer
                  deriving (Eq, Show)
type ISRule = [String]


instance FromOwned (IptablesSave ISRule) where
    type Owned (IptablesSave ISRule) = IptablesSave (ResourceOwner, ISRule)
    disown iss = [ go t | IS t <- iss ]
     where
       go x@ISComment {} = x
       go ISTable (IST tn cs) =
           ISTable (IST tn [ ISC cn p [ r | (_o, r) <- rs ]
                           | ISC cn p rs <- cs ])
    owners = []

-- type instance Owned ISTable = [(ResourceOwner, a)]

ip6tablesResource :: Resource
ip6tablesResource = FileResource {
    rNormalize = \str -> unparse $ filterKib $ parse str,
    rPath = etcdir </> "iptables/rules.v6",
    rParse = map markOurs . parse,
    rUnparse = concatMap unparse,
    rContentFunc = addOrReplace (isOurs . snd) (OwnerKib, cfg)
  }

filterKib = catMaybes . map go
 where
   go (ISComment _) =
       Nothing
   go (ISTable (IST t cs)) =
       Just $ ISTable $ IST t
          [ ISC cn p (if isKibChain cn then rs else rs')
          | ISC cn p rs <- cs
          , let rs' = filter ( fromMaybe False
                             . fmap isKibChain
                             . extractTarget
                             ) rs

          , isKibChain cn || not (null rs')
          ]

   isKibChain = ("KIB_" `isPrefixOf`)


parse :: String -> [IptablesSave]
parse str =
    case runParser (parseIptablesSave <* eof) () "<iptables-save>" str of
      Left err -> error (show err)
      Right a -> a

unparse :: [IptablesSave] -> String
unparse = unlines . concat . map go
 where
   go :: IptablesSave -> [String]
   go (ISComment s) = ['#':s]

   go (ISTable (IST n cs)) =
       concat [ ['*':n]
              , catMaybes $ map (fmap goP . iscPolicy) cs
              , concat (map goC cs)
              , ["COMMIT"]
              ]

   goC (ISC _ _ rs) =
       map goR rs

   goP :: ISPolicy -> String
   goP (ISP c p rx tx) =
       unwords [':':c, p, concat [ "[", show rx, ":", show tx, "]"]]

   goR :: ISRule -> String
   goR as = unwords as


-- | @insertAccpetRulesIntoChains d s@. Rules in @s@ must not have a jump
-- target or have the target ACCEPT for the semantics of d to be
-- preserved. Assumes rules from @s@ are not already in @d@.

--
-- >>> test_insertAccpetRulesIntoChains
-- True
insertAccpetRulesIntoChains :: [ISChain] -> [ISChain] -> [ISChain]
insertAccpetRulesIntoChains d s  = let
    dm, sm :: Map String ISChain
    dm = Map.fromList $ map (iscName &&& id) d
    sm = Map.fromList $ map (iscName &&& id) s
  in
    Map.elems $ Map.unionWith mergeChain dm sm

 where
   mergeChain :: ISChain -> ISChain -> ISChain
   mergeChain (ISC n mp rs) (ISC _ _ rs') = -- left biased
                  ISC n mp $ mergeRules rs rs'

   mergeRules rs rs' = let
       (accept, nonAccept) = span isAcceptOrNop rs
     in
       accept ++ rs' ++ nonAccept

   isAcceptOrNop args =
       extractTarget args `elem` [Just "ACCEPT", Nothing]


extractTarget ("-j":chain:rest) = Just chain
extractTarget ("-g":chain:rest) = Just chain
extractTarget (arg:rest) = extractTarget rest
extractTarget [] = Nothing


test_insertAccpetRulesIntoChains :: Bool
test_insertAccpetRulesIntoChains =
    insertAccpetRulesIntoChains [c1] [c2] == [c3]

 where
   c1 = ISC "INPUT" Nothing [
         ["-A", "INPUT", "-j", "ACCEPT"],
         ["-A", "INPUT", "-j", "ACCEPT"],
         ["-A", "INPUT", "-j", "DROP"],
         ["-A", "INPUT", "-j", "REJECT"]
        ]

   c2 = ISC "INPUT" Nothing [
         ["-A", "INPUT", "-j", "KIB_INPUT"]
        ]

   c3 = ISC "INPUT" Nothing [
         ["-A", "INPUT", "-j", "ACCEPT"],
         ["-A", "INPUT", "-j", "ACCEPT"],
         ["-A", "INPUT", "-j", "KIB_INPUT"],
         ["-A", "INPUT", "-j", "DROP"],
         ["-A", "INPUT", "-j", "REJECT"]
        ]


parseIptablesSave :: Parsec String u [IptablesSave]
parseIptablesSave = many ((ISComment <$> parseComment) <|> (ISTable <$> parseTable))

parseComment =
  char '#' *> (many nonNewline) <* newline

parseTable :: Parsec String u ISTable
parseTable = do
  IST <$> parseTableDecl <*> parseChains

parseTableDecl = do
  char '*' *> many1 parseChainIdentChar <* newline

parseChains :: Parsec String u [ISChain]
parseChains = do
  ps <- many1 parsePolicy
  rs <- many parseRule
  parseCommit

  let
      -- CHAIN -> [RULE]
      crsm = Map.fromListWith (flip (++))
           $ fmap (second (:[]))
           $ map extractChain rs `zip` rs
      -- CHAIN -> POLICY
      cpm  = Map.fromList $ map (\p@(ISP c _ _ _) -> (c, p)) ps
      cm :: Map.Map String (Maybe ISPolicy, Maybe [ISRule])
      cm = cpm ⟗ crsm
      cs = Map.elems $ Map.mapWithKey mkISC cm
      mkISC c (mp, fromMaybe [] -> rs) = ISC c mp rs

  return cs

 where
   extractChain ("-A":chain:rest) = chain
   extractChain (arg:rest) = extractChain rest
   extractChain [] = error "extractChain: no chain found"

parsePolicy :: Parsec String u ISPolicy
parsePolicy = do
  char ':'
  chain <- many1 parseChainIdentChar
  nnspaces
  policy <- many1 alphaNum
  nnspaces
  char '['
  rx <- read <$> many1 digit
  char ':'
  tx <- read <$> many1 digit
  char ']'
  skipMany nonNewline
  newline
  return $ ISP chain policy rx tx

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
