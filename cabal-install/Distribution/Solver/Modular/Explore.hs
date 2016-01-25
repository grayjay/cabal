module Distribution.Solver.Modular.Explore
    ( backjump
    , backjumpAndExplore
    ) where

import Data.Foldable as F
import Data.List as L (foldl')
import Data.Map as M

import Distribution.Solver.Modular.Assignment
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Log
import Distribution.Solver.Modular.Message
import qualified Distribution.Solver.Modular.PSQ as P
import qualified Distribution.Solver.Modular.ConflictSet as CS
import qualified Distribution.Solver.Modular.WeightedPSQ as W
import Distribution.Solver.Modular.Tree
import Distribution.Solver.Types.PackagePath
import Distribution.Solver.Types.Settings
import qualified Distribution.Solver.Types.Progress as P
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
-- 'backjump' uses the 'ExploreState' to record the current best install
-- plan each time it adds a backjump to the log.
--
backjump :: EnableBackjumping
         -> Var QPN
         -> ConflictSet QPN
         -> W.WeightedPSQ w k (ExploreState -> ConflictSetLog a)
         -> ExploreState -> ConflictSetLog a
backjump (EnableBackjumping enableBj) var initial xs =
    F.foldr combine logBackjump xs initial
  where
    combine :: (ExploreState -> ConflictSetLog a)
            -> (ConflictSet QPN -> ExploreState -> ConflictSetLog a)
            ->  ConflictSet QPN -> ExploreState -> ConflictSetLog a
    combine x f csAcc cm =
      let l = x cm
      in case l of
        P.Done d  -> P.Done d
        P.Fail (cs, es) ->
          case CS.lookup var cs of
            Nothing               | enableBj -> logBackjump cs es
            Just ConflictLessThan | enableBj -> logBackjump (csAcc `CS.union` cs) es
            _                                -> f           (csAcc `CS.union` cs) es
        P.Step m ms ->
          let l' = combine (\ _ -> ms) f csAcc cm
          in P.Step m l'

    logBackjump :: ConflictSet QPN -> ExploreState -> ConflictSetLog a
    logBackjump cs = fail' cs Backjump

type ConflictSetLog = P.Progress Message (ConflictSet QPN, ExploreState)

type ConflictMap = Map (Var QPN) Int

getBestGoal :: ConflictMap -> P.PSQ (Goal QPN) a -> (Goal QPN, a)
getBestGoal cm =
  P.maximumBy
    ( flip (M.findWithDefault 0) cm
    . (\ (Goal v _) -> v)
    )

getFirstGoal :: P.PSQ (Goal QPN) a -> (Goal QPN, a)
getFirstGoal ts =
  P.casePSQ ts
    (error "getFirstGoal: empty goal choice") -- empty goal choice is an internal error
    (\ k v _xs -> (k, v))  -- commit to the first goal choice

updateCM :: ConflictSet QPN -> ConflictMap -> ConflictMap
updateCM cs cm =
  L.foldl' (\ cmc k -> M.alter inc k cmc) cm (CS.toList cs)
  where
    inc Nothing  = Just 1
    inc (Just n) = Just $! n + 1

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

data ExploreState = ExploreState {
      esConflictMap :: ConflictMap

      -- | The current best install plan.
    , esBestPlan :: Maybe Plan

      -- | The current maximum score. It is equal to the minimum of the value
      -- specified with --max-score and the score of the best install plan. If
      -- neither of those two values exists, 'esMaxScore' is equal to 'Nothing'.
    , esMaxScore :: Maybe InstallPlanScore
    }

-- | A tree traversal that simultaneously prunes nodes based on score,
-- propagates conflict sets up the tree from the leaves, and creates a log.
--
-- The solver lowers the cutoff score as it finds better and better solutions.
-- It interleaves pruning and backjumping because the two processes are
-- interdependent. Backjumping allows the solver to calculate the current best
-- score after visiting fewer of the preceding nodes. Pruning produces the
-- conflict sets required for backjumping.
explore :: EnableBackjumping
        -> CountConflicts
        -> Maybe InstallPlanScore
        -> FindBestSolution
        -> Tree (Assignment, ScoringState) (QGoalReason, ScoringState)
        -> ConflictSetLog Plan
explore enableBj (CountConflicts countConflicts) maxScore findBest t =
    cata go t initES
  where
    getBestGoal' :: P.PSQ (Goal QPN) a -> ConflictMap -> (Goal QPN, a)
    getBestGoal'
      | countConflicts = \ ts cm -> getBestGoal cm ts
      | otherwise      = \ ts _  -> getFirstGoal ts

    go :: TreeF (Assignment, ScoringState)
                (QGoalReason, ScoringState)
                (ExploreState -> ConflictSetLog Plan)
       -> (ExploreState -> ConflictSetLog Plan)
    go (FailF c fr)                   = \ es ->
      let failure = fail' c fr
          cm = updateCM c (esConflictMap es)
      in if countConflicts
         then failure es { esConflictMap = cm }
         else failure es
    go (DoneF rdm (a, ss))            =
      maybePrune ss $
          if asBool findBest
          then \ es ->
            let es' = es {
                          esBestPlan = Just (a, rdm, ssTotalScore ss)
                        , esMaxScore = Just $ ssTotalScore ss
                        }
            in fail' (ssConflictSet ss) (SearchingForBetterScore (ssTotalScore ss)) es'
          else \ _ -> succeedWith Success (a, rdm, ssTotalScore ss)

    go (PChoiceF qpn (gr, ss)     ts) =
      maybePrune ss $ backjump enableBj (P qpn) (avoidSet (P qpn) gr) $
        W.mapWithKey
          (\ k r cm ->
            let l = r cm
            in tryWith (TryP qpn k) l
          )
        ts
    go (FChoiceF qfn (gr, ss) _ _ ts) =
      maybePrune ss $ backjump enableBj (F qfn) (avoidSet (F qfn) gr) $
        W.mapWithKey
          (\ k r cm ->
            let l = r cm
            in  tryWith (TryF qfn k) l
          )
        ts
    go (SChoiceF qsn (gr, ss) _   ts) =
      maybePrune ss $ backjump enableBj (S qsn) (avoidSet (S qsn) gr) $
        W.mapWithKey
          (\ k r cm ->
            let l = r cm
            in  tryWith (TryS qsn k) l
          )
        ts
    go (GoalChoiceF         ts)       = \ es ->
      let (k, v) = getBestGoal' ts (esConflictMap es)
          l = v es
      in continueWith (Next k) l

    maybePrune :: ScoringState
               -> (ExploreState -> ConflictSetLog a)
               -> ExploreState -> ConflictSetLog a
    maybePrune ss successLog es =
      let maxScore' = esMaxScore es
      in if maybe False (ssTotalScore ss >=) maxScore'
         then let cs = ssConflictSet ss
              in (fail' cs $ ExceedsMaxScore (ssTotalScore ss)) es
         else successLog es

    initES :: ExploreState
    initES = ExploreState {
                 esConflictMap = M.empty
               , esBestPlan = Nothing
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
fail' :: ConflictSet QPN -> FailReason -> ExploreState -> ConflictSetLog a
fail' c fr es = failWith (Failure c fr (esBestPlan es)) (c, es)

-- | Interface.
backjumpAndExplore :: EnableBackjumping
                   -> CountConflicts
                   -> Maybe InstallPlanScore
                   -> FindBestSolution
                   -> Tree ScoringState (QGoalReason, ScoringState)
                   -> Log Message Plan
backjumpAndExplore enableBj countConflicts maxScore findBest =
    toLog . explore enableBj countConflicts maxScore findBest . assign
  where
    toLog :: P.Progress step fail done -> Log step done
    toLog = P.foldProgress P.Step (const (P.Fail ())) P.Done
