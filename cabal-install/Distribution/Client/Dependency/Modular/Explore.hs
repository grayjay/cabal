{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Client.Dependency.Modular.Explore where

import Control.Applicative as A
import Data.Foldable
import Data.List as L
import Data.Map as M
import Data.Maybe (isNothing)

import Distribution.Client.Dependency.Modular.Assignment
import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Log
import Distribution.Client.Dependency.Modular.Message
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.PSQ as P
import Distribution.Client.Dependency.Modular.Tree
import Distribution.Client.Dependency.Modular.WeightedPSQ as W

type MaybeCS = Maybe (ConflictSet QPN)

-- | Backjumping.
--
-- A tree traversal that tries to propagate conflict sets
-- up the tree from the leaves, and thereby cut branches.
-- All the tricky things are done in the function 'combine'.
backjump :: Tree a b -> Tree a MaybeCS
backjump = snd . cata go
  where
    go :: TreeF a b (MaybeCS, Tree a MaybeCS) -> (MaybeCS, Tree a MaybeCS)
    go (FailF c fr) = (Just c, Fail c fr)
    go (DoneF rdm s) = (Nothing, Done rdm s)
    go (PChoiceF qpn _     ts) = (c, PChoice qpn c     ts')
      where
        ~(c, ts') = combine (P qpn) ts
    go (FChoiceF qfn _ b m ts) = (c, FChoice qfn c b m ts')
      where
        ~(c, ts') = combine (F qfn) ts
    go (SChoiceF qsn _ b   ts) = (c, SChoice qsn c b   ts')
      where
        ~(c, ts') = combine (S qsn) ts
    go (GoalChoiceF        ts) = (c, GoalChoice        (P.fromList ts'))
      where
        ~(cs, ts') = unzip $ L.map (\ (k, (x, v)) -> (x, (k, v))) $ P.toList ts
        c          = case cs of []    -> Nothing
                                d : _ -> d

-- | The 'combine' function is at the heart of backjumping. It takes
-- the variable we're currently considering, and a list of children
-- annotated with their respective conflict sets, and an accumulator
-- for the result conflict set. It returns a combined conflict set
-- for the parent node, and a (potentially shortened) list of children
-- with the annotations removed. We keep all children that have been
-- traversed, so that they can appear in the log.
--
-- It is *essential* that we produce the results as early as possible.
-- In particular, we have to produce the list of children prior to
-- traversing the entire list -- otherwise we lose the desired behaviour
-- of being able to traverse the tree from left to right incrementally.
--
-- We can shorten the list of children if we find an individual conflict
-- set that does not contain the current variable. In this case, we can
-- just lift the conflict set to the current level, because the current
-- level cannot possibly have contributed to this conflict, so no other
-- choice at the current level would avoid the conflict.
--
-- If any of the children might contain a successful solution
-- (indicated by Nothing), then Nothing will be the combined
-- conflict set. If all children contain conflict sets, we can
-- take the union as the combined conflict set.
combine :: forall w k v . Var QPN
        -> WeightedPSQ w k (MaybeCS, v)
        -> (MaybeCS, WeightedPSQ w k v)
combine var children = (cs, fmap snd traversed)
  where
    traversed :: WeightedPSQ w k (MaybeCS, v)
    traversed = W.takeUntil (canBackjump . fst) children

    canBackjump :: MaybeCS -> Bool
    canBackjump (Just cs') = case M.lookup (simplifyVar var) cs' of
                               Nothing               -> True
                               Just ConflictLessThan -> True
                               Just ConflictAll      -> False
    canBackjump Nothing                               = False

    cs :: MaybeCS
    cs = let css = L.map fst (W.elems children)
             unionTraversed = unionsCS `fmap` mapM fst (W.elems traversed)
         in  case L.find (liftA2 (||) canBackjump isNothing) css of
               Nothing                      -> unionTraversed
               Just Nothing                 -> Nothing
               Just (Just cs') -> case M.lookup (simplifyVar var) cs' of
                                    Nothing -> Just cs'
                                    _       -> unionTraversed

-- | Naive backtracking exploration of the search tree. This will yield correct
-- assignments only once the tree itself is validated.
explore :: Alternative m => Tree a b -> (Assignment -> m (Assignment, RevDepMap))
explore = cata go
  where
    go (FailF _ _)           _           = A.empty
    go (DoneF rdm _)         a           = pure (a, rdm)
    go (PChoiceF qpn _     ts) (A pa fa sa)   =
      asum $                                      -- try children in order,
      W.mapWithKey                                -- when descending ...
        (\ (POption k _) r -> r (A (M.insert qpn k pa) fa sa)) -- record the pkg choice
      ts
    go (FChoiceF qfn _ _ _ ts) (A pa fa sa)   =
      asum $                                      -- try children in order,
      W.mapWithKey                                -- when descending ...
        (\ k r -> r (A pa (M.insert qfn k fa) sa)) -- record the flag choice
      ts
    go (SChoiceF qsn _ _   ts) (A pa fa sa)   =
      asum $                                      -- try children in order,
      W.mapWithKey                                -- when descending ...
        (\ k r -> r (A pa fa (M.insert qsn k sa))) -- record the flag choice
      ts
    go (GoalChoiceF        ts) a              =
      casePSQ ts A.empty                      -- empty goal choice is an internal error
        (\ _k v _xs -> v a)                   -- commit to the first goal choice

-- | Version of 'explore' that returns a 'Log'.
exploreLog :: Tree a (Maybe (ConflictSet QPN)) ->
              (Assignment -> Log Message (Assignment, RevDepMap, a))
exploreLog = cata go
  where
    go (FailF c fr)          _           = failWith (Failure c fr)
    go (DoneF rdm s)         a           = succeedWith Success (a, rdm, s)
    go (PChoiceF qpn c     ts) (A pa fa sa)   =
      backjumpInfo c $
      asum $                                      -- try children in order,
      W.mapWithKey                                -- when descending ...
        (\ i@(POption k _) r -> tryWith (TryP qpn i) $     -- log and ...
                    r (A (M.insert qpn k pa) fa sa)) -- record the pkg choice
      ts
    go (FChoiceF qfn c _ _ ts) (A pa fa sa)   =
      backjumpInfo c $
      asum $                                      -- try children in order,
      W.mapWithKey                                -- when descending ...
        (\ k r -> tryWith (TryF qfn k) $          -- log and ...
                    r (A pa (M.insert qfn k fa) sa)) -- record the pkg choice
      ts
    go (SChoiceF qsn c _   ts) (A pa fa sa)   =
      backjumpInfo c $
      asum $                                      -- try children in order,
      W.mapWithKey                                -- when descending ...
        (\ k r -> tryWith (TryS qsn k) $          -- log and ...
                    r (A pa fa (M.insert qsn k sa))) -- record the pkg choice
      ts
    go (GoalChoiceF        ts) a           =
      casePSQ ts
        (failWith (Failure M.empty EmptyGoalChoice))   -- empty goal choice is an internal error
        (\ k v _xs -> continueWith (Next (close k)) (v a))     -- commit to the first goal choice

-- | Add in information about pruned trees.
--
-- TODO: This isn't quite optimal, because we do not merely report the shape of the
-- tree, but rather make assumptions about where that shape originated from. It'd be
-- better if the pruning itself would leave information that we could pick up at this
-- point.
backjumpInfo :: Maybe (ConflictSet QPN) -> Log Message a -> Log Message a
backjumpInfo c m = m <|> case c of -- important to produce 'm' before matching on 'c'!
                           Nothing -> A.empty
                           Just cs -> failWith (Failure cs Backjump)

-- | Interface.
exploreTree :: Alternative m => Tree a b -> m (Assignment, RevDepMap)
exploreTree t = explore t (A M.empty M.empty M.empty)

-- | Interface.
exploreTreeLog :: Tree a (Maybe (ConflictSet QPN)) -> Log Message (Assignment, RevDepMap, a)
exploreTreeLog t = exploreLog t (A M.empty M.empty M.empty)
