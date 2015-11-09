{-# LANGUAGE CPP #-}
#ifdef DEBUG_CONFLICT_SETS
{-# LANGUAGE ImplicitParams #-}
#endif
-- | Conflict sets
--
-- Intended for double import
--
-- > import Distribution.Solver.Modular.ConflictSet (ConflictSet)
-- > import qualified Distribution.Solver.Modular.ConflictSet as CS
module Distribution.Solver.Modular.ConflictSet (
    ConflictSet -- opaque
#ifdef DEBUG_CONFLICT_SETS
  , conflictSetOrigin
#endif
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
import Data.Function (on)
import qualified Data.Map as M

#ifdef DEBUG_CONFLICT_SETS
import Data.Tree
import GHC.Stack
#endif

import Distribution.Solver.Modular.Package
import Distribution.Solver.Modular.Var

-- | The set of variables involved in a solver conflict
--
-- Since these variables should be preprocessed in some way, this type is
-- kept abstract.
data ConflictSet qpn = CS {
    -- | The set of variables involved on the conflict
    conflictSetToMap :: Map (Var qpn) ConflictType

#ifdef DEBUG_CONFLICT_SETS
    -- | The origin of the conflict set
    --
    -- When @DEBUG_CONFLICT_SETS@ is defined @(-f debug-conflict-sets)@,
    -- we record the origin of every conflict set. For new conflict sets
    -- ('empty', 'fromVars', ..) we just record the 'CallStack'; for operations
    -- that construct new conflict sets from existing conflict sets ('union',
    -- 'filter', ..)  we record the 'CallStack' to the call to the combinator
    -- as well as the 'CallStack's of the input conflict sets.
    --
    -- Requires @GHC >= 7.10@.
  , conflictSetOrigin :: Tree CallStack
#endif
  }
  deriving (Show)

instance Eq qpn => Eq (ConflictSet qpn) where
  (==) = (==) `on` conflictSetToMap

instance Ord qpn => Ord (ConflictSet qpn) where
  compare = compare `on` conflictSetToMap

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
    intercalate ", " . map (uncurry showConflict) . M.toList . conflictSetToMap
  where
    -- TODO: How should we display the type of conflict?
    showConflict v t = "(" ++ showVar v ++ ", " ++ show t ++ ")"

{-------------------------------------------------------------------------------
  Set-like operations
-------------------------------------------------------------------------------}

toList :: ConflictSet qpn -> [Var qpn]
toList = map fst . M.toList . conflictSetToMap

union ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  Ord qpn => ConflictSet qpn -> ConflictSet qpn -> ConflictSet qpn
union cs cs' = CS {
      conflictSetToMap = M.unionWith combineConflictType
                         (conflictSetToMap cs) (conflictSetToMap cs')
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc (map conflictSetOrigin [cs, cs'])
#endif
    }

unions ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  Ord qpn => [ConflictSet qpn] -> ConflictSet qpn
unions css = CS {
      conflictSetToMap = M.unionsWith combineConflictType
                         (map conflictSetToMap css)
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc (map conflictSetOrigin css)
#endif
    }

insert :: Ord qpn => Var qpn -> ConflictSet qpn -> ConflictSet qpn
insert v = insertWithConflictType v ConflictAll

insertWithConflictType ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  Ord qpn => Var qpn -> ConflictType -> ConflictSet qpn -> ConflictSet qpn
insertWithConflictType var ct cs = CS {
      conflictSetToMap = M.insertWith combineConflictType
                         (simplifyVar var) ct (conflictSetToMap cs)
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc [conflictSetOrigin cs]
#endif
    }

empty ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  ConflictSet qpn
empty = CS {
      conflictSetToMap = M.empty
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc []
#endif
    }

singleton ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  Var qpn -> ConflictSet qpn
singleton var = CS {
      conflictSetToMap = M.singleton (simplifyVar var) ConflictAll
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc []
#endif
    }

member :: Ord qpn => Var qpn -> ConflictSet qpn -> Bool
member var = M.member (simplifyVar var) . conflictSetToMap

lookup :: Ord qpn => Var qpn -> ConflictSet qpn -> Maybe ConflictType
lookup var = M.lookup (simplifyVar var) . conflictSetToMap

filter ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
#if !MIN_VERSION_containers(0,5,0)
  Ord qpn =>
#endif
  (Var qpn -> Bool) -> ConflictSet qpn -> ConflictSet qpn
filter p cs = CS {
      conflictSetToMap = M.filterWithKey (\v _ -> p v) (conflictSetToMap cs)
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc [conflictSetOrigin cs]
#endif
    }

fromList ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  Ord qpn => [Var qpn] -> ConflictSet qpn
fromList vars = CS {
      conflictSetToMap = M.fromListWith combineConflictType
         $ map (\v -> (simplifyVar v, ConflictAll)) vars
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc []
#endif
    }
