module Ssh (sshdResource) where

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.Char
import Data.List
import Data.Maybe
import Text.Parsec hiding (parse)
import Text.Parsec.Char
import Text.Parsec.Prim hiding (parse)
import Text.Parsec.Combinator
import System.FilePath
import Debug.Trace

import Resource
import Config
import ParserUtils

sshdResource :: Resource
sshdResource = FileResource {
    rNormalize = \str -> concatMap unparse $ traceShowId $ filter isOurs $ parse str,
    rPath = etcdir </> "ssh/sshd_config",
    rParse = map markOurs . parse,
    rUnparse = concatMap unparse :: [SshCfgDir] -> String,
    rContentFunc = addOrReplace (isOurs . snd) (OwnerKib, cfg) . fromMaybe []
  }

both f a b = f a && f b
isOurs x = headMatch (SshCfgDir "Match" ["Group", "kib"] []) x

markOurs x | isOurs x = (OwnerKib, x)
           | otherwise = (OwnerSystem, x)

replace p x' x
    | p x = x'
    | otherwise = x

addOrReplace :: (a -> Bool) -> a -> [a] -> [a]
addOrReplace p x l = nubBy (both p) $ map (replace p x) l ++ [x]

cf = addOrReplace (isOurs . snd) (OwnerKib, cfg) . fromMaybe []

-- test_sshdResource :: Bool
-- test_sshdResource = case sshdResource of
--   FileResource {..} -> let
--       p0 = rContentFunc $ Just $ rParse src0
--       p2 = rContentFunc $ Just $ rParse src2
--     in
--       (p0 :: [(ResourceOwner, SshCfgDir)]) == (p2 :: [(ResourceOwner, SshCfgDir)])


src0 = "Hello World\n"
src1 = "Hello World\n\
       \Match Group kib\n"
src2 = "Hello World\n\
       \Match Group kib\n\
       \AllowAgentForwarding no\n\
       \AllowTcpForwarding no\n\
       \AcceptEnv no\n"




headMatch :: SshCfgDir -> SshCfgDir -> Bool
headMatch (SshCfgDir d as _) (SshCfgDir d' as' _) =
    map toLower d == map toLower d && as == as'
headMatch _ _ = False

cfg = SshCfgDir "Match" ["Group", "kib"] [
          SshCfgDir "AllowTcpForwarding" ["no"] [],
          SshCfgDir "AllowAgentForwarding" ["no"] [],
          SshCfgDir "AcceptEnv" ["no"] []
      ]


data SshCfgLine = SshCfgLine { sclDir :: String, sclArgs :: [String] }
                | SshCommentLine String
                  deriving (Show)

data SshCfgDir = SshCfgDir {
                    scDirective :: String,
                    scArgs      :: [String],
                    scSub       :: [SshCfgDir]
                   }
               | SshCommentDir String
                  deriving (Show)

groupCfg :: [SshCfgLine] -> [SshCfgDir]
groupCfg (SshCfgLine d as : ds)
    | isCollectDirective d = let
       (sub, rest) = collect ds
     in SshCfgDir d as (groupCfg sub) : groupCfg rest
    | otherwise =
        [SshCfgDir d as []] ++ groupCfg ds
groupCfg (SshCommentLine s : ds) = [SshCommentDir s] ++ groupCfg ds
groupCfg [] = []

isCollectDirective d = map toLower d == "host" || map toLower d == "match"

collect = span (not . isCollectDirective . sclDir)

unparse (SshCfgDir d as []) =
    intercalate " " (d:as) ++ "\n"
unparse (SshCfgDir d as subs) =
    intercalate " " (d:as) ++ "\n" ++ concatMap (("  "++) . unparse) subs
unparse (SshCommentDir s) = s ++ "\n"

parse :: String -> [SshCfgDir]
parse = groupCfg . parse'

parse' :: String -> [SshCfgLine]
parse' str =
    case runParser parseSsh () "<ssh-config>" str of
      Left err -> error (show err)
      Right a -> a

parseSsh = many $ (try parseComment <|> parseLine) <* newline

parseLine = SshCfgLine
    <$> (nnspaces *> parseDirective <* nnspaces)
    <*> (parseArgs <* nnspaces)

parseComment = do
  sp <- many nonNewlineSpace
  c <-  many1 (char '#')
  nn <- many nonNewline
  return $ SshCommentLine $ sp ++ c ++ nn

parseDirective = many1 alphaNum

parseArgs :: Stream s m Char => ParsecT s u m [String]
parseArgs = words <$> many1 nonNewline
