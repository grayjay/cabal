{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Distribution.Client.Dependency.Modular.WeightedPSQ (
    WeightedPSQ
  , filter
  , fromList
  , keys
  , length
  , llength
  , lookup
  , mapWeightsWithKey
  , mapWithKey
  , toList
  , union
  , weights
  , zipWithIndex
  ) where

-- Lists that are always sorted by weight.

import qualified Data.Foldable as F
import Data.Function
import qualified Data.List as L
import Data.Ord
import Data.Traversable
import Prelude hiding (filter, length, lookup)

newtype WeightedPSQ w k v = WeightedPSQ [(w, k, v)]
  deriving (Eq, Show, Functor, F.Foldable, Traversable)

filter :: (v -> Bool) -> WeightedPSQ k w v -> WeightedPSQ k w v
filter p (WeightedPSQ xs) = WeightedPSQ (L.filter (p . triple_3) xs)

length :: WeightedPSQ k w v -> Int
length (WeightedPSQ xs) = L.length xs

-- | "Lazy length".
--
-- Only approximates the length, but doesn't force the list.
llength :: WeightedPSQ w k v -> Int
llength (WeightedPSQ [])       = 0
llength (WeightedPSQ (_:[]))   = 1
llength (WeightedPSQ (_:_:[])) = 2
llength (WeightedPSQ _)        = 3

toList :: WeightedPSQ w k v -> [(w, k, v)]
toList (WeightedPSQ xs) = xs

fromList :: Ord w => [(w, k, v)] -> WeightedPSQ w k v
fromList = WeightedPSQ . L.sortBy (comparing triple_1)

weights :: WeightedPSQ w k v -> [w]
weights (WeightedPSQ xs) = L.map triple_1 xs

keys :: WeightedPSQ w k v -> [k]
keys (WeightedPSQ xs) = L.map triple_2 xs

lookup :: Eq k => k -> WeightedPSQ w k v -> Maybe v
lookup k (WeightedPSQ xs) = triple_3 `fmap` L.find ((k ==) . triple_2) xs

mapWeightsWithKey :: Ord w2
                  => (k -> w1 -> w2)
                  -> WeightedPSQ w1 k v
                  -> WeightedPSQ w2 k v
mapWeightsWithKey f (WeightedPSQ xs) = fromList $
                                       L.map (\ (w, k, v) -> (f k w, k, v)) xs

mapWithKey :: (k -> v -> b) -> WeightedPSQ w k v -> WeightedPSQ w k b
mapWithKey f (WeightedPSQ xs) = WeightedPSQ $
                                L.map (\ (w, k, v) -> (w, k, f k v)) xs

zipWithIndex :: WeightedPSQ w k v -> WeightedPSQ w k (v, Int)
zipWithIndex (WeightedPSQ xs) = WeightedPSQ (zipWith f xs [0..])
  where
    f (w, k, v) i = (w, k, (v, i))

union :: Ord w => WeightedPSQ w k v -> WeightedPSQ w k v -> WeightedPSQ w k v
union (WeightedPSQ xs) (WeightedPSQ ys) = fromList (xs ++ ys)

triple_1 :: (x, y, z) -> x
triple_1 (x, _, _) = x

triple_2 :: (x, y, z) -> y
triple_2 (_, y, _) = y

triple_3 :: (x, y, z) -> z
triple_3 (_, _, z) = z
