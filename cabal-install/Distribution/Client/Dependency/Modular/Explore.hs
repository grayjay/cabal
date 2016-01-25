module Distribution.Client.Dependency.Modular.Explore
    ( backjump
    , backjumpAndExplore
    ) where

import Control.Monad.State.Lazy
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
--
-- 'backjump' runs in the 'Explore' monad so that it can short-circuit
-- the calculation of state during each backjump. Additionally, it uses
-- the state to record the current best install plan each time it adds
-- a backjump to the log.
backjump :: F.Foldable t => Var QPN
         -> t (Explore (ConflictSetLog a))
         -> Explore (ConflictSetLog a)
backjump var xs = F.foldr combine backjumpInfo xs M.empty
  where
    combine :: Explore (ConflictSetLog a)
            -> (ConflictSet QPN -> Explore (ConflictSetLog a))
            ->  ConflictSet QPN -> Explore (ConflictSetLog a)
    combine lg f csAcc = lg >>= \lg' ->
        case lg' of
          T.Done x    -> return $ T.Done x
          T.Fail cs   ->
              case M.lookup (simplifyVar var) cs of
                Nothing               -> backjumpInfo cs
                Just ConflictLessThan -> backjumpInfo (csAcc `unionCS` cs)
                Just ConflictAll      -> f (csAcc `unionCS` cs)
          T.Step m ms -> T.Step m `fmap` combine (return ms) f csAcc

type ConflictSetLog = T.Progress Message (ConflictSet QPN)

-- | Record complete assignments on 'Done' nodes.
assign :: Tree a b -> Tree (Assignment, a) b
assign tree = cata go tree $ A M.empty M.empty M.empty
  where
    go :: TreeF a b (Assignment -> Tree (Assignment, a) b)
                 -> (Assignment -> Tree (Assignment, a) b)
    go (FailF c fr)            _            = Fail c fr
    go (DoneF rdm x)           a            = Done rdm (a, x)
    go (PChoiceF qpn y     ts) (A pa fa sa) = PChoice qpn y     $ W.mapWithKey f ts
        where f (POption k _) r = r (A (M.insert qpn k pa) fa sa)
    go (FChoiceF qfn y t m ts) (A pa fa sa) = FChoice qfn y t m $ W.mapWithKey f ts
        where f k             r = r (A pa (M.insert qfn k fa) sa)
    go (SChoiceF qsn y t   ts) (A pa fa sa) = SChoice qsn y t   $ W.mapWithKey f ts
        where f k             r = r (A pa fa (M.insert qsn k sa))
    go (GoalChoiceF        ts) a            = GoalChoice $ fmap ($ a) ts

type Explore = State ExploreState

data ExploreState = ExploreState {
      -- | The current best install plan.
      esBestPlan :: Maybe Plan

      -- | The current maximum score. It is equal to the minimum of the value
      -- specified with --max-score and the score of the best install plan. If
      -- neither of those two values exists, 'esMaxScore' is equal to 'Nothing'.
    , esMaxScore :: Maybe T.InstallPlanScore
    }

-- | A tree traversal that simultaneously prunes nodes based on score,
-- propagates conflict sets up the tree from the leaves, and creates a log.
--
-- The solver lowers the cutoff score as it finds better and better solutions.
-- It interleaves pruning and backjumping because the two processes are
-- interdependent. Backjumping allows the solver to calculate the current best
-- score after visiting fewer of the preceding nodes. Pruning produces the
-- conflict sets required for backjumping.
explore :: Maybe T.InstallPlanScore
        -> T.SolverExhaustiveness
        -> Tree (Assignment, ScoringState) ScoringState
        -> ConflictSetLog Plan
explore maxScore exh = (`evalState` initES) . cata go
  where
    go :: TreeF (Assignment, ScoringState)
                ScoringState
                (Explore (ConflictSetLog Plan))
       -> Explore (ConflictSetLog Plan)
    go (FailF c fr)             = fail' c fr
    go (DoneF rdm (a, ss))      =
      maybePrune ss $ do
          put ExploreState {
                  esBestPlan = Just (a, rdm, ssTotalScore ss)
                , esMaxScore = Just $ ssTotalScore ss
                }
          case exh of
            T.FindFirstSolution -> return $
                                   succeedWith Success (a, rdm, ssTotalScore ss)
            T.FindBestSolution  -> fail' (ssConflictSet ss) $
                                   SearchingForBetterScore (ssTotalScore ss)
    go (PChoiceF qpn ss     ts) =
      maybePrune ss $ backjump (P qpn) $
      W.mapWithKey (\ k r -> tryWith (TryP qpn k) `fmap` r) ts
    go (FChoiceF qfn ss _ _ ts) =
      maybePrune ss $ backjump (F qfn) $
      W.mapWithKey (\ k r -> tryWith (TryF qfn k) `fmap` r) ts
    go (SChoiceF qsn ss _   ts) =
      maybePrune ss $ backjump (S qsn) $
      W.mapWithKey (\ k r -> tryWith (TryS qsn k) `fmap` r) ts
    go (GoalChoiceF         ts) =
      P.casePSQ ts
        (fail' M.empty EmptyGoalChoice)                    -- empty goal choice is an internal error
        (\ k v _xs -> continueWith (Next (close k)) `fmap` v) -- commit to the first goal choice

    maybePrune :: ScoringState
               -> Explore (ConflictSetLog a)
               -> Explore (ConflictSetLog a)
    maybePrune ss successLog = do
      maxScore' <- gets esMaxScore
      if maybe False (ssTotalScore ss >=) maxScore'
        then let cs = ssConflictSet ss
             in fail' cs $ ExceedsMaxScore (ssTotalScore ss)
        else successLog

    initES :: ExploreState
    initES = ExploreState {
                 esBestPlan = Nothing
               , esMaxScore = maxScore
               }

-- | Add in information about pruned trees.
--
-- TODO: This isn't quite optimal, because we do not merely report the shape of the
-- tree, but rather make assumptions about where that shape originated from. It'd be
-- better if the pruning itself would leave information that we could pick up at this
-- point.
backjumpInfo :: ConflictSet QPN -> Explore (ConflictSetLog a)
backjumpInfo cs = fail' cs Backjump

-- | Fail and record the current best install plan.
fail' :: ConflictSet QPN -> FailReason -> Explore (ConflictSetLog a)
fail' c fr = (\plan -> failWith (Failure c fr plan) c) `fmap` gets esBestPlan

-- | Interface.
backjumpAndExplore :: Maybe T.InstallPlanScore
                   -> T.SolverExhaustiveness
                   -> Tree ScoringState ScoringState
                   -> Log Message Plan
backjumpAndExplore maxScore exhaustiveness =
    toLog . explore maxScore exhaustiveness . assign
  where
    toLog :: T.Progress step fail done -> Log step done
    toLog = T.foldProgress T.Step (const (T.Fail ())) T.Done
