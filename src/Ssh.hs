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

import Resource
import Config
import ParserUtils

sshdResource :: Resource
sshdResource = FileResource {
    rNormalize = \str -> concatMap unparse $ filter isOurs $ parse str,
    rPath = etcdir </> "ssh/sshd_config",
    rParse = map markOurs . parse,
    rUnparse = concatMap unparse :: [SshCfgDir] -> String,
    rContentFunc = addOrReplace (isOurs . snd) (OwnerKib, cfg) . fromMaybe []
  }
-- [Owned SshCfgDir] -> [Owned SshCfgDir]

 where
   both f a b = f a && f b
   isOurs x = headMatch (SshCfgDir "Match" ["Group", "kib"] []) x
   markOurs x | isOurs x = (OwnerKib, x)
              | otherwise = (OwnerSystem, x)

   replace p x' x
       | p x = x'
       | otherwise = x

   addOrReplace :: (a -> Bool) -> a -> [a] -> [a]
   addOrReplace p x l = nubBy (both p) $ map (replace p x) l ++ [x]



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
    intercalate " " (d:as)
unparse (SshCfgDir d as subs) =
    intercalate " " (d:as) ++ "\n" ++ concatMap ((++"\n") . unparse) subs
unparse (SshCommentDir s) = s ++ "\n"

parse :: String -> [SshCfgDir]
parse str = groupCfg $
    case runParser parseSsh () "<ssh-config>" str of
      Left err -> error (show err)
      Right a -> a

parseSsh = many $ (try parseComment <|> parseLine) <* newline

parseLine = SshCfgLine
    <$> (nnspaces *> parseDirective <* nnspaces)
    <*> (parseArgs <* nnspaces)

parseComment = do
  sp <- many nonNewlineSpace
  c <-  many (char '#')
  nn <- many nonNewline
  return $ SshCommentLine $ sp ++ c ++ nn

parseDirective = many1 alphaNum

parseArgs :: Stream s m Char => ParsecT s u m [String]
parseArgs = words <$> many1 nonNewline
