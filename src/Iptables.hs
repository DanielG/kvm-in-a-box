{-# LANGUAGE ViewPatterns, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeFamilies, TupleSections, DeriveDataTypeable, DeriveFunctor #-}
module Iptables where

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Control.Monad
import Control.Arrow
import Control.Exception.Base
import Data.Function
import Data.Either
import Data.Data
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Map (Map)
import Data.Word
import qualified Data.Map as Map
import Data.MonoTraversable
import Data.Bifunctor
import Data.Bitraversable
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
      isComments :: [((Maybe String, Maybe String), String)]
    , isTables   :: Map String (IST r)
    }
    deriving (Eq, Show, Data, Functor)

unIptablesSave :: IptablesSave r -> [Either String (String, (IST r))]
unIptablesSave = undefined

newtype IST r = IST (Map String (ISChain r))
    deriving (Eq, Show, Data, Functor)

data ISChain r = ISC { iscPolicy :: (Maybe ISPolicy),
                       iscRules :: [r]
                     }
    deriving (Eq, Show, Data, Functor)

data ISPolicy = ISP String (Maybe String) Integer Integer
    deriving (Eq, Show, Data)

type ISRule = [String]

instance FromOwned (IptablesSave r) where
    type Owned (IptablesSave r) = IptablesSave (ResourceOwner, r)
    disown is = fmap gogo is
      where
       gogo :: (ResourceOwner, r) -> r
       gogo (_o, r) = r

    owners _ _ = []

-- type instance Owned IST = [(ResourceOwner, a)]

data Firewall = Firewall {
      fwPorts :: [(Word16, Word16)]

    }

-- ip6tablesResource :: Resource
-- ip6tablesResource = FileResource {
--     rNormalize = \str -> unparse $ parse str,
--     rPath = etcdir </> "iptables/rules.v6",
--     rParse = markOurs . parse,
--     rUnparse = unparse,
--     rContentFunc = \mois -> IptablesSave $ doStuff $ fromMaybe [] $ unIptablesSave <$> mois

-- -- IptablesSave $
-- --         updateISSTable "filter" (fromMaybe [] $ unIptablesSave <$> mis) $
-- --             Map.insert "KIB_INPUT" kib_input
-- --           . flip insertAccpetRulesIntoChains redir
-- --           . map removeKib
--   }
--  where
--    doStuff :: Endo [IST (ResourceOwner, [String])]
--    doStuff = map (_)
--                . map removeKib


--    redir = Map.singleton "INPUT" $ ISC Nothing [
--             ["-A", "INPUT", "-j", "KIB_INPUT"]
--            ]
--    kib_input = ISC Nothing [
--                 ["-A", "KIB_INPUT", "-j", "RETURN"]
--                ]

type Fold a b = a -> Endo b
type Fold1 a = a -> a
type Endo a = a -> a

-- foldIST :: (String -> Endo b)
--        -> (String -> String -> ISChain r -> Endo b)
--        -> Fold (IST r) b
-- foldIST fisc fist (ISComment c) b =
--     fisc c b
-- foldIST fisc fist (IST tn cm) b =
--     Map.foldrWithKey (fist tn) b cm

-- foldL f xs b = foldr f b xs

-- flatten :: IST (ResourceOwner, r) -> [Either String (String, String, Maybe ISPolicy, (ResourceOwner,r))]
-- flatten a = foldIS
--   (\s -> (.(Left s:)))
--   (\tn cn (ISC mp rs) -> flip foldL rs
--     (\r ->
--       (.(Right (tn, cn, mp, r):))
--     )
--   ) a id []

-- unflatten :: forall r. [Either String (String, String, Maybe ISPolicy, r)]
--           -> [IST r]
-- unflatten xs = let
--     siss :: [Either String (Map String (Map String (Maybe ISPolicy, [r])))]
--     siss = flip map xs $ bimap id $ \(tn, cn, mp, r) -> Map.singleton tn (Map.singleton cn (mp, [r]))

--     merge ::Endo [Either String (Map String (Map String (Maybe ISPolicy, [r])))]
--     merge xs = flip2both foldr xs [] $ flip kernel

--     kernel (Left _ : b) (Right rm) =
--         Right rm:b
--     kernel (Right m : b) r@(Right rm) =
--         Right (Map.unionWith (Map.unionWith with) m rm) : b
--      where
--        with (mp, rs) (mp', rs') = (assert (mp == mp') mp, rs ++ rs')
--     kernel b v =
--         v:b

--     fromMap :: (Map String (Map String (Maybe ISPolicy, [r]))) -> [(String, (Map String (ISChain r)))]
--     fromMap m = flip2both Map.foldrWithKey m [] $ \k mm -> (:) $ (,) k $
--                   flip Map.map mm $ \(mp, rs) -> ISC mp rs
--   in
--     concatMap (either ((:[]) . ISComment) (map (uncurry IST) . fromMap)) $ merge siss

-- isKibChain = ("KIB_" `isPrefixOf`)

-- updateISSTable :: String -> [IST r] -> (Endo (Map String (ISChain r))) -> [IST r]
-- updateISSTable qtn is fm
--     | null $ filter ((==qtn). istName) is = is ++ [IST qtn (fm Map.empty)]
--     | otherwise = map f is
--  where
--    f c@ISComment{} = c
--    f t@(IST tn cm)
--        | tn == qtn = IST tn $ fm cm
--        | otherwise = t

-- modifyIST :: forall r s.
--             (String -> Maybe String)
--          -> ((String, String, Maybe ISPolicy, (ResourceOwner, r)) -> Maybe (String, String, Maybe ISPolicy, (ResourceOwner, s)))
--          -> IST (ResourceOwner, r)
--          -> IST (ResourceOwner, s)
-- modifyIST fc ft is  = let
--     fl :: [Either (Maybe String) (Maybe (String, String, Maybe ISPolicy, (ResourceOwner, s)))]
--     fl = map (bimap fc ft) $ flatten is

--     a = flip2both foldr fl [] c

--     c :: Either (Maybe a) (Maybe b) -> [Either a b] -> [Either a b]
--     c emmab b = maybeToList (bisequence emmab) ++ b
--   in
--     undefined

-- modifyISP :: ((String, String, Maybe ISPolicy, (ResourceOwner, r)) -> Bool) -> IST (ResourceOwner, r) -> IST (ResourceOwner, r)
-- modifyISP p = modifyIST Just (\x -> if p x then Just x else Nothing)

-- markOurs :: IptablesSave ISRule -> IptablesSave (ResourceOwner, ISRule)
-- markOurs is = ft <$> is
--  where
--    isKib' r | isKibChain (extractChain r) = True
--             | otherwise = False

--    ft r | isKib' r  = (OwnerKib, r)
--         | otherwise = (OwnerSystem, r)

-- removeKib :: IST (ResourceOwner, ISRule) -> IST (ResourceOwner, ISRule)
-- removeKib = modifyISP (not . isKib)

-- justKib :: IST (ResourceOwner, ISRule) -> IST (ResourceOwner, ISRule)
-- justKib = modifyISP (isKib)

-- isKib :: (String, String, Maybe ISPolicy, (ResourceOwner, ISRule)) -> Bool
-- isKib (_, cn, _, (_, r)) =
--   isKibChain cn || fromMaybe False (isKibChain <$> extractTarget r)

parse :: String -> IptablesSave ISRule
parse str =
    case runParser (parseIptablesSave <* eof) () "<iptables-save>" str of
      Left err -> error (show err)
      Right a -> a

mapWithKeyL :: (a -> b -> c) -> Map a b -> [c]
mapWithKeyL f m = Map.elems $ Map.mapWithKey f m

unparse :: IptablesSave ISRule -> String
unparse = unlines . concat . map (either goCM goT) . unIptablesSave
 where
   goCM s = [s]

   goT :: (String, IST [String]) -> [String]
   goT (tn, (IST cm)) = concat $
       [ ['*':tn]
       , catMaybes $ map (fmap goP) (Map.elems $ Map.map iscPolicy cm)
       , map (concat . goC) $ Map.elems cm
       , ["COMMIT"]
       ]

   goC (ISC _ rs) =
       map goR rs

   goP :: ISPolicy -> String
   goP (ISP c mp rx tx) =
       unwords [ ':':c
               , fromMaybe "-" mp
               , concat [ "[", show rx, ":", show tx, "]"]
               ]

   goR :: [String] -> String
   goR as = unwords as


-- | @insertAccpetRulesIntoChains d s@. Rules in @s@ must not have a jump
-- target or have the target ACCEPT for the semantics of d to be
-- preserved. Assumes rules from @s@ are not already in @d@.

--
-- >>> test_insertAccpetRulesIntoChains
-- True
insertAccpetRulesIntoChains ::
    forall r t. (r ~ [String], t ~ (Map String (ISChain r))) => t -> t -> t
insertAccpetRulesIntoChains dst src  =
    Map.unionWith mergeChain dst src

 where
   mergeChain :: ISChain r -> ISChain r -> ISChain r
   mergeChain (ISC mp rs) (ISC mp' rs') =
     assert (mp == mp') $ ISC mp $ mergeRules rs rs'

   mergeRules :: [r] -> [r] -> [r]
   mergeRules rs rs' = let
       (accept, nonAccept) = span isAcceptOrNop rs
     in
       accept ++ rs' ++ nonAccept

   isAcceptOrNop args =
       extractTarget args `elem` [Just "ACCEPT", Nothing]

test_insertAccpetRulesIntoChains :: Bool
test_insertAccpetRulesIntoChains =
    insertAccpetRulesIntoChains c1 c2 == c3

 where
   c1 = Map.singleton "INPUT" $ ISC Nothing [
         ["-A", "INPUT", "-j", "ACCEPT"],
         ["-A", "INPUT", "-j", "ACCEPT"],
         ["-A", "INPUT", "-j", "DROP"],
         ["-A", "INPUT", "-j", "REJECT"]
        ]

   c2 = Map.singleton "INPUT" $ ISC Nothing [
         ["-A", "INPUT", "-j", "KIB_INPUT"]
        ]

   c3 = Map.singleton "INPUT" $ ISC Nothing [
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


type DL a = [a] -> [a]

(%) :: DL a -> DL a -> DL a
da :: a -> DL a -> DL a
dl :: [a] -> DL a
ld :: DL a -> [a]
de :: DL a

ds % es = ds . es
da = (.) . (:)
dl xs = (xs++)
ld ds = ds []
de = id


parseIptablesSave :: Parsec String u (IptablesSave [String])
parseIptablesSave = do
    cts :: [Either String ((String, (IST [String])))]
       <- many (  (Left  <$> parseComment)
              <|> (Right <$> ((,) <$> parseTableDecl <*> (IST <$> parseChains)))
               )
    let
        fcts :: [(Either String ((String, (IST [String]))))]
             -> DL (Either ((Maybe String, Maybe String), String) ((String, (IST [String]))))
        fcts = flip2fst foldr de $ \a b -> flip da b $
                either (Left . commentContext (map fst $ rights $ b [])) Right a

        commentContext :: [a] -> t -> ((Maybe a, Maybe a), t)
        commentContext [] x      = ((Nothing, Nothing), x)
        commentContext ([a]) x   = ((Just a, Nothing), x)
        commentContext (a:b:_) x = ((Just a, Just b), x)

        istUnion (IST m) (IST o) =
            IST $ Map.unionWith iscUnion m o
        iscUnion (ISC mp rs) (ISC mp' rs') =
            assert (mp == mp') $ ISC mp $ rs ++ rs'

    return $ uncurry IptablesSave
           $ Control.Arrow.second (Map.fromListWith istUnion)
           $ partitionEithers $ ld $ fcts cts

parseComment =
  char '#' *> (many nonNewline) <* newline

parseTableDecl :: Parsec String u String
parseTableDecl = do
  char '*' *> many1 parseChainIdentChar <* newline

parseChains :: Parsec String u (Map String (ISChain ISRule))
parseChains = do
  ps <- many1 parsePolicy
  rs <- catMaybes <$> many ((Just <$> parseRule) <|> (Nothing <$ parseComment))
  parseCommit

  let
      -- CHAIN -> [RULE]
      crsm = Map.fromListWith (flip (++))
           $ fmap (Control.Arrow.second (:[]))
           $ map extractChain rs `zip` rs
      -- CHAIN -> POLICY
      cpm  = Map.fromList $ map (\p@(ISP c _ _ _) -> (c, p)) ps
      cm :: Map String (Maybe ISPolicy, Maybe [ISRule])
      cm = cpm ⟗ crsm
      cs = flip Map.mapWithKey cm $ \cn (mp, mrs) -> ISC mp $ fromMaybe [] mrs

  return cs

extractChain ("-A":chain:rest) = chain
extractChain (arg:rest) = extractChain rest
extractChain [] = error "extractChain: no chain found"

parsePolicy :: Parsec String u ISPolicy
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
  return $ ISP chain mpolicy rx tx

parseRule :: Parsec String u [String]
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
