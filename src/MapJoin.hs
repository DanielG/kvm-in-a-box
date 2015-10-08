module MapJoin where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

naturalJoin,    (⋈) :: Ord k => Map k a -> Map k b -> Map k (a,b)
leftSemiJoin,   (⋉) :: Ord k => Map k a -> Map k b -> Map k a
rightSemiJoin,  (⋊) :: Ord k => Map k a -> Map k b -> Map k b
leftOuterJoin,  (⟕) :: Ord k => Map k a -> Map k b -> Map k (Maybe a,b)
rightOuterJoin, (⟖) :: Ord k => Map k a -> Map k b -> Map k (a,Maybe b)
fullOuterJoin,  (⟗) :: Ord k => Map k a -> Map k b -> Map k (Maybe a,Maybe b)

naturalJoin l r = Map.intersectionWith (,) l r


leftSemiJoin = Map.intersection


rightSemiJoin = Map.intersectionWith (flip const)


leftOuterJoin l r = Map.mapWithKey (\k x -> (Map.lookup k l, x)) r


rightOuterJoin l r = Map.mapWithKey (\k x -> (x, Map.lookup k r)) l

fullOuterJoin l r = let
    l'  = Map.map (\a -> (Just a, Nothing)) l
    r' = Map.map (\a -> (Nothing, Just a)) r
  in
    Map.unionWith (\(a, _) (_, b) -> (a, b)) l' r'

-- \Join
(⋈) = naturalJoin

-- \ltimes
(⋉) = leftSemiJoin
-- \rtimes
(⋊) = rightSemiJoin

-- \leftouterjoin
(⟕) = leftOuterJoin
-- \rightouterjoin
(⟖) = rightOuterJoin
-- \fullouterjoin
(⟗) = fullOuterJoin
