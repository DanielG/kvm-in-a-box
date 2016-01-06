module JSON where

import Data.Char
import Data.List
import Data.List.Split
import Data.Aeson.TH

-- GHC staging restriction gah..

jsonOpts = defaultOptions {
             fieldLabelModifier = uncamel . drop 1,
             constructorTagModifier = uncamel,
             allNullaryToStringTag = True
           }

uncamel :: String -> String
uncamel (s:ss) = if all ((==1) . length) $ splitOn "_" underscored then map toLower (s:ss) else underscored
 where
   underscored = concatMap f $ toLower s:ss
   f c = if isUpper c then ['_', toLower c] else [c]
