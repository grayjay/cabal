module Distribution.Client.Dependency.Modular.Explore
    ( backjump
    , backjumpAndExplore
    ) where

import Data.Foldable as F
import Data.Map as M

import Distribution.Client.Dependency.Modular.Assignment
import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Log
import Distribution.Client.Dependency.Modular.Message
import Distribution.Client.Dependency.Modular.Package
import qualified Distribution.Client.Dependency.Modular.PSQ as P
import Distribution.Client.Dependency.Modular.Tree
import qualified Distribution.Client.Dependency.Modular.WeightedPSQ as W
import qualified Distribution.Client.Dependency.Types as T

-- | This function takes the variable we're currently considering and a
-- list of children's logs. Each log yields either a solution or a
-- conflict set. The result is a combined log for the parent node that
-- has explored a prefix of the children.
--
-- We can stop traversing the children's logs if we find an individual
-- conflict set that does not contain the current variable. In this
-- case, we can just lift the conflict set to the current level,
-- because the current level cannot possibly have contributed to this
-- conflict, so no other choice at the current level would avoid the
-- conflict.
--
-- We can also stop if we find a conflict set where the current
-- variable is mapped to 'ConflictLessThan'. 'ConflictLessThan' means
-- that the conflict can only be resolved by choosing a value that is
-- to the left of the conflicting assignment. We have already traversed
-- those possible assignments, and we return the union of their
-- conflict sets.
--
-- If any of the children might contain a successful solution, we can
-- return it immediately. If all children contain conflict sets, we can
-- take the union as the combined conflict set.
backjump :: F.Foldable t => Var QPN -> t (ConflictSetLog a) -> ConflictSetLog a
backjump var xs = F.foldr combine backjumpInfo xs M.empty
  where
    combine :: ConflictSetLog a
            -> (ConflictSet QPN -> ConflictSetLog a)
            ->  ConflictSet QPN -> ConflictSetLog a
    combine (T.Done x)    _ _               = T.Done x
    combine (T.Fail cs)   f csAcc =
      case M.lookup (simplifyVar var) cs of
        Nothing                            -> backjumpInfo cs
        Just ConflictLessThan              -> backjumpInfo (csAcc `unionCS` cs)
        Just ConflictAll                   -> f (csAcc `unionCS` cs)
    combine (T.Step m ms) f cs              = T.Step m (combine ms f cs)

type ConflictSetLog = T.Progress Message (ConflictSet QPN)

-- | A tree traversal that simultaneously propagates conflict sets up
-- the tree from the leaves and creates a log.
exploreLog :: Tree a b -> (Assignment -> ConflictSetLog (Assignment, RevDepMap, a))
exploreLog = cata go
  where
    go :: TreeF a b (Assignment -> ConflictSetLog (Assignment, RevDepMap, a))
                 -> (Assignment -> ConflictSetLog (Assignment, RevDepMap, a))
    go (FailF c fr)          _           = failWith (Failure c fr) c
    go (DoneF rdm s)         a           = succeedWith Success (a, rdm, s)
    go (PChoiceF qpn _     ts) (A pa fa sa)   =
      backjump (P qpn) $                          -- try children in order,
      W.mapWithKey                                -- when descending ...
        (\ i@(POption k _) r -> tryWith (TryP qpn i) $ -- log and ...
                    r (A (M.insert qpn k pa) fa sa)) -- record the pkg choice
      ts
    go (FChoiceF qfn _ _ _ ts) (A pa fa sa)   =
      backjump (F qfn) $                          -- try children in order,
      W.mapWithKey                                -- when descending ...
        (\ k r -> tryWith (TryF qfn k) $          -- log and ...
                    r (A pa (M.insert qfn k fa) sa)) -- record the pkg choice
      ts
    go (SChoiceF qsn _ _   ts) (A pa fa sa)   =
      backjump (S qsn) $                          -- try children in order,
      W.mapWithKey                                -- when descending ...
        (\ k r -> tryWith (TryS qsn k) $          -- log and ...
                    r (A pa fa (M.insert qsn k sa))) -- record the pkg choice
      ts
    go (GoalChoiceF        ts) a           =
      P.casePSQ ts
        (failWith (Failure M.empty EmptyGoalChoice) M.empty) -- empty goal choice is an internal error
        (\ k v _xs -> continueWith (Next (close k)) (v a))   -- commit to the first goal choice

-- | Add in information about pruned trees.
--
-- TODO: This isn't quite optimal, because we do not merely report the shape of the
-- tree, but rather make assumptions about where that shape originated from. It'd be
-- better if the pruning itself would leave information that we could pick up at this
-- point.
backjumpInfo :: ConflictSet QPN -> ConflictSetLog a
backjumpInfo cs = failWith (Failure cs Backjump) cs

-- | Interface.
backjumpAndExplore :: Tree a b -> Log Message (Assignment, RevDepMap, a)
backjumpAndExplore t = toLog $ exploreLog t (A M.empty M.empty M.empty)
  where
    toLog :: T.Progress step fail done -> Log step done
    toLog = T.foldProgress T.Step (const (T.Fail ())) T.Done
