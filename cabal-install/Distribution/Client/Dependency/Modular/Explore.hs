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
import qualified Distribution.Client.Dependency.Modular.ConflictSet as CS
import Distribution.Client.Dependency.Modular.Tree
import qualified Distribution.Client.Dependency.Modular.WeightedPSQ as W
import qualified Distribution.Client.Dependency.Types as T
import Distribution.Simple.Setup (asBool)

-- | This function takes the variable we're currently considering, an
-- initial conflict set and a
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
-- The initial conflict set corresponds to the justification that we
-- have to choose this goal at all. There is a reason why we have
-- introduced the goal in the first place, and this reason is in conflict
-- with the (virtual) option not to choose anything for the current
-- variable. See also the comments for 'avoidSet'.
--
-- 'backjump' runs in the 'Explore' monad so that it can short-circuit
-- the calculation of state during each backjump. Additionally, it uses
-- the state to record the current best install plan each time it adds
-- a backjump to the log.
--
backjump :: F.Foldable t
         => T.EnableBackjumping
         -> Var QPN
         -> ConflictSet QPN
         -> t (Explore (ConflictSetLog a))
         -> Explore (ConflictSetLog a)
backjump (T.EnableBackjumping enableBj) var initial xs =
    F.foldr combine logBackjump xs initial
  where
    combine :: Explore (ConflictSetLog a)
            -> (ConflictSet QPN -> Explore (ConflictSetLog a))
            ->  ConflictSet QPN -> Explore (ConflictSetLog a)
    combine lg f csAcc = lg >>= \lg' ->
        case lg' of
          T.Done x    -> return $ T.Done x
          T.Fail cs   ->
              case CS.lookup var cs of
                Nothing               | enableBj -> logBackjump cs
                Just ConflictLessThan | enableBj -> logBackjump (csAcc `CS.union` cs)
                _                                -> f           (csAcc `CS.union` cs)
          T.Step m ms -> T.Step m `fmap` combine (return ms) f csAcc

    logBackjump :: ConflictSet QPN -> Explore (ConflictSetLog a)
    logBackjump cs = fail' cs Backjump

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
explore :: T.EnableBackjumping
        -> Maybe T.InstallPlanScore
        -> T.FindBestSolution
        -> Tree (Assignment, ScoringState) (QGoalReason, ScoringState)
        -> ConflictSetLog Plan
explore enableBj maxScore findBest =
    (`evalState` initES) . cata go
  where
    go :: TreeF (Assignment, ScoringState)
                (QGoalReason, ScoringState)
                (Explore (ConflictSetLog Plan))
       -> Explore (ConflictSetLog Plan)
    go (FailF c fr)             = fail' c fr
    go (DoneF rdm (a, ss))      =
      maybePrune ss $ do
          put ExploreState {
                  esBestPlan = Just (a, rdm, ssTotalScore ss)
                , esMaxScore = Just $ ssTotalScore ss
                }
          if asBool findBest
          then fail' (ssConflictSet ss) $
               SearchingForBetterScore (ssTotalScore ss)
          else return $ succeedWith Success (a, rdm, ssTotalScore ss)
    go (PChoiceF qpn (gr, ss)     ts) =
      maybePrune ss $ backjump enableBj (P qpn) (avoidSet (P qpn) gr) $
      W.mapWithKey (\ k r -> tryWith (TryP qpn k) `fmap` r) ts
    go (FChoiceF qfn (gr, ss) _ _ ts) =
      maybePrune ss $ backjump enableBj (F qfn) (avoidSet (F qfn) gr) $
      W.mapWithKey (\ k r -> tryWith (TryF qfn k) `fmap` r) ts
    go (SChoiceF qsn (gr, ss) _   ts) =
      maybePrune ss $ backjump enableBj (S qsn) (avoidSet (S qsn) gr) $
      W.mapWithKey (\ k r -> tryWith (TryS qsn k) `fmap` r) ts
    go (GoalChoiceF         ts) =
      P.casePSQ ts
        (fail' CS.empty EmptyGoalChoice)                    -- empty goal choice is an internal error
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

-- | Build a conflict set corresponding to the (virtual) option not to
-- choose a solution for a goal at all.
--
-- In the solver, the set of goals is not statically determined, but depends
-- on the choices we make. Therefore, when dealing with conflict sets, we
-- always have to consider that we could perhaps make choices that would
-- avoid the existence of the goal completely.
--
-- Whenever we actual introduce a choice in the tree, we have already established
-- that the goal cannot be avoided. This is tracked in the "goal reason".
-- The choice to avoid the goal therefore is a conflict between the goal itself
-- and its goal reason. We build this set here, and pass it to the 'backjump'
-- function as the initial conflict set.
--
-- This has two effects:
--
-- - In a situation where there are no choices available at all (this happens
-- if an unknown package is requested), the initial conflict set becomes the
-- actual conflict set.
--
-- - In a situation where we backjump past the current node, the goal reason
-- of the current node will be added to the conflict set.
--
avoidSet :: Var QPN -> QGoalReason -> ConflictSet QPN
avoidSet var gr =
  CS.fromList (var : goalReasonToVars gr)

-- | Fail and record the current best install plan.
fail' :: ConflictSet QPN -> FailReason -> Explore (ConflictSetLog a)
fail' c fr = (\plan -> failWith (Failure c fr plan) c) `fmap` gets esBestPlan

-- | Interface.
backjumpAndExplore :: T.EnableBackjumping
                   -> Maybe T.InstallPlanScore
                   -> T.FindBestSolution
                   -> Tree ScoringState (QGoalReason, ScoringState)
                   -> Log Message Plan
backjumpAndExplore enableBj maxScore findBest =
    toLog . explore enableBj maxScore findBest . assign
  where
    toLog :: T.Progress step fail done -> Log step done
    toLog = T.foldProgress T.Step (const (T.Fail ())) T.Done
