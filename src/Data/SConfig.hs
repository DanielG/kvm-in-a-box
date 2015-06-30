-- Copyright (c) 2014 Francesco Gazzetta
module Data.SConfig
( readConfig
, writeConfig
, getValue
, Config
, Key
, Value
) where

import qualified Data.Map as M
import Data.List
import Data.Maybe (fromJust)

-- | Config key, alias for String.
type Key = String

-- | Config value, alias for String.
type Value = String

-- | Parsed configuration. Basically a Map String String
type Config = M.Map Key Value

-- | Convert configuration to parseable string
writeConfig :: Config -> String
writeConfig cfg = concat
                $ intersperse "\n"
                $ map (\(key,value) -> key ++ "=" ++ value) $ M.toList cfg

-- | Parse configuration
readConfig :: String -> Config
readConfig str = foldl readConfigLine M.empty --apply readConfigLine to every line
                      $ filter (elem '=') --make sure all lines are actual configuration lines
                      $ concatEscaped
                      $ filter (\x -> (not $ null x) && (take 1 x) /= "#") --remove comments & empty lines
                      $ lines str

--inserts a parsed key-value pair into the Config accumulator
readConfigLine :: Config -> String -> Config
readConfigLine config str = M.insert (filter (/=' ') key) value config
    where (key,_:value) = break (=='=') str

--concatenate two lines if the first ends with a backslash
concatEscaped :: [String] -> [String]
concatEscaped lines = foldr (\x (a:acc) -> if last x == '\\' then ((init x) ++ a):acc else x:a:acc) [""] lines

-- | Lookup values in a parsed configuration (alias of Map.lookup)
getValue :: Key -> M.Map Key Value -> Maybe Value
getValue = M.lookup
