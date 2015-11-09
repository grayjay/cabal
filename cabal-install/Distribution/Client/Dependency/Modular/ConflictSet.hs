{-# LANGUAGE CPP #-}
-- | Conflict sets
--
-- Intended for double import
--
-- > import Distribution.Client.Dependency.Modular.ConflictSet (ConflictSet)
-- > import qualified Distribution.Client.Dependency.Modular.ConflictSet as CS
module Distribution.Client.Dependency.Modular.ConflictSet (
    ConflictSet -- opaque
  , ConflictType(..)
  , showCS
    -- Set-like operations
  , toList
  , union
  , unions
  , insert
  , insertWithConflictType
  , empty
  , singleton
  , member
  , lookup
  , filter
  , fromList
  ) where

import Prelude hiding (filter, lookup)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M

import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Var

-- | The set of variables involved in a solver conflict
--
-- Since these variables should be preprocessed in some way, this type is
-- kept abstract.
newtype ConflictSet qpn = CS { fromConflictSet :: Map (Var qpn) ConflictType }
  deriving (Eq, Ord, Show)

-- TODO: The name of this type and its constructors could be improved.
data ConflictType =

  -- | Any other value in the variable's domain might resolve the conflict.
  ConflictAll

  -- | Only values that are less than the current assignment can resolve the
  -- conflict.
  | ConflictLessThan
  deriving (Eq, Ord, Show)

combineConflictType :: ConflictType -> ConflictType -> ConflictType
combineConflictType ConflictLessThan ConflictLessThan = ConflictLessThan
combineConflictType _ _ = ConflictAll

showCS :: ConflictSet QPN -> String
showCS =
    intercalate ", " . map (uncurry showConflict) . M.toList . fromConflictSet
  where
    -- TODO: How should we display the type of conflict?
    showConflict v t = "(" ++ showVar v ++ ", " ++ show t ++ ")"

{-------------------------------------------------------------------------------
  Set-like operations
-------------------------------------------------------------------------------}

toList :: ConflictSet qpn -> [Var qpn]
toList = map fst . M.toList . fromConflictSet

union :: Ord qpn => ConflictSet qpn -> ConflictSet qpn -> ConflictSet qpn
union (CS a) (CS b) = CS (M.unionWith combineConflictType a b)

unions :: Ord qpn => [ConflictSet qpn] -> ConflictSet qpn
unions = CS . M.unionsWith combineConflictType . map fromConflictSet

insert :: Ord qpn => Var qpn -> ConflictSet qpn -> ConflictSet qpn
insert v = insertWithConflictType v ConflictAll

insertWithConflictType :: Ord qpn => Var qpn -> ConflictType
                       -> ConflictSet qpn -> ConflictSet qpn
insertWithConflictType var ct (CS set) =
    CS (M.insertWith combineConflictType (simplifyVar var) ct set)

empty :: ConflictSet qpn
empty = CS M.empty

singleton :: Var qpn -> ConflictSet qpn
singleton v = CS $ M.singleton (simplifyVar v) ConflictAll

member :: Ord qpn => Var qpn -> ConflictSet qpn -> Bool
member var (CS set) = M.member (simplifyVar var) set

lookup :: Ord qpn => Var qpn -> ConflictSet qpn -> Maybe ConflictType
lookup var (CS set) = M.lookup (simplifyVar var) set

#if MIN_VERSION_containers(0,5,0)
filter :: (Var qpn -> Bool) -> ConflictSet qpn -> ConflictSet qpn
#else
filter :: Ord qpn => (Var qpn -> Bool) -> ConflictSet qpn -> ConflictSet qpn
#endif
filter p (CS set) = CS $ M.filterWithKey (\v _ -> p v) set

fromList :: Ord qpn => [Var qpn] -> ConflictSet qpn
fromList = CS . M.fromListWith combineConflictType
         . map (\v -> (simplifyVar v, ConflictAll))
