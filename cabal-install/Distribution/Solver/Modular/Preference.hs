{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
module Distribution.Solver.Modular.Preference
    ( avoidReinstalls
    , deferSetupChoices
    , deferWeakFlagChoices
    , enforceManualFlags
    , enforcePackageConstraints
    , enforceSingleInstanceRestriction
    , firstGoal
    , preferBaseGoalChoice
    , preferEasyGoalChoices
    , preferLinked
    , preferPackagePreferences
    , preferReallyEasyGoalChoices
    , requireInstalled
    , scoreTree
    ) where

-- Reordering or pruning the tree in order to prefer or make certain choices.

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Traversable as T
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Prelude hiding (sequence)
import Control.Monad.Reader hiding (sequence)
import Data.Map (Map)
import Data.Maybe
import Data.Ord (comparing)

import Distribution.Client.Dependency.Types
  ( PackageConstraint(..), LabeledPackageConstraint(..), ConstraintSource(..)
  , PackagePreferences(..), InstalledPreference(..) )
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.Settings

import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Package
import qualified Distribution.Solver.Modular.PSQ as P
import Distribution.Solver.Modular.Tree
import Distribution.Solver.Modular.Version
import qualified Distribution.Solver.Modular.ConflictSet as CS
import qualified Distribution.Solver.Modular.WeightedPSQ as W

-- | Update the weights of children under 'PChoice' nodes.
addWeight :: (PN -> [Ver] -> POption -> Weight) -> Tree a b -> Tree a b
addWeight f = trav go
  where
    go (PChoiceF qpn@(Q _ pn) x cs) =
      -- TODO: Inputs to 'f' shouldn't depend on the node's position in the tree.
      -- If we continue using a list of all versions as an input, it should come
      -- from the package index, not from the node's siblings.
      let sortedVersions = L.sortBy (flip compare) $ L.map version (W.keys cs)
      in  PChoiceF qpn x $
          W.mapWeightsWithKey (\k w -> f pn sortedVersions k : w) cs
    go x                            = x

version :: POption -> Ver
version (POption (I v _) _) = v

-- | Prefer to link packages whenever possible.
-- TODO: I'm not sure we should give different scores to linked and unlinked
-- nodes, since the link choices depend on goal order. The same choice could
-- have different scores in different parts of the tree.
preferLinked :: Tree a b -> Tree a b
preferLinked = addWeight (const (const linked))
  where
    linked (POption _ Nothing)  = 1
    linked (POption _ (Just _)) = 0

-- Works by setting weights on choice nodes. Also applies stanza preferences.
preferPackagePreferences :: (PN -> PackagePreferences) -> Tree a b -> Tree a b
preferPackagePreferences pcs =
    preferPackageStanzaPreferences pcs
  . addWeight (const . preferred)

  -- Note that we always rank installed before uninstalled, and later
  -- versions before earlier, but we can change the priority of the
  -- two orderings.
  . addWeight (\pn -> case preference pn of
                        PreferInstalled -> const installed
                        PreferLatest    -> latest)
  . addWeight (\pn -> case preference pn of
                        PreferInstalled -> latest
                        PreferLatest    -> const installed)
  where
    -- Prefer packages with higher version numbers over packages with
    -- lower version numbers.
    latest :: [Ver] -> POption -> Weight
    latest sortedVersions pOpt =
      -- TODO: We should probably score versions based on their release
      -- dates.
      let index = fromJust $ L.elemIndex (version pOpt) sortedVersions
      in  fromIntegral index / L.genericLength sortedVersions

    preference :: PN -> InstalledPreference
    preference pn =
      let PackagePreferences _ ipref _ = pcs pn
      in  ipref

    -- | Prefer versions satisfying more preferred version ranges.
    preferred :: PN -> POption -> Weight
    preferred pn pOpt =
      let PackagePreferences vrs _ _ = pcs pn
      in fromIntegral . negate . L.length $
         L.filter (flip checkVR (version pOpt)) vrs

    -- Prefer installed packages over non-installed packages.
    installed :: POption -> Weight
    installed (POption (I _ (Inst _)) _) = 0
    installed _                          = 1

type Score = Reader ScoringState

-- | Traversal that scores all choice and 'Done' nodes.
scoreTree :: Tree a b -> Tree ScoringState (b, ScoringState)
scoreTree = (`runReader` initSS) . cata go
  where
    go :: TreeF a b (Score (Tree ScoringState (b, ScoringState)))
                  -> Score (Tree ScoringState (b, ScoringState))
    go (PChoiceF qpn gr     cs)        =
      PChoice qpn . (gr,) <$> ask <*> processChildren (P qpn) cs
    go (FChoiceF qfn gr t m cs)        =
      FChoice qfn . (gr,) <$> ask <*> pure t <*> pure m <*> processChildren (F qfn) cs
    go (SChoiceF qsn gr t   cs)        =
      SChoice qsn . (gr,) <$> ask <*> pure t            <*> processChildren (S qsn) cs
    go (GoalChoiceF        cs)        = GoalChoice     <$> T.sequence cs
    go (DoneF revDepMap _)            = Done revDepMap <$> ask
    go (FailF conflictSet failReason) = return $ Fail conflictSet failReason

    -- TODO: This function currently scores a node by dividing its index in the
    -- list of siblings by the total number of siblings. This is an easy way to
    -- calculate scores of type InstallPlanScore (isomorphic to Double) from
    -- nodes that have weight type [Double], without giving too much weight to
    -- the first Double in the list.
    --
    -- This function should use the node's weight as its score once weights have
    -- type 'Double'. Score should not depend on the node's position in the
    -- tree.
    processChildren :: Var QPN
                    -> W.WeightedPSQ w k (Score (Tree a (b, ScoringState)))
                    -> Score (W.WeightedPSQ w k (Tree a (b, ScoringState)))
    processChildren var cs =
      let processChild c i = scoreNode var (i == 0) (fromIntegral i / l) c
          l = fromIntegral (W.length cs)
      in  l `seq` T.traverse (uncurry processChild) (W.zipWithIndex cs)

    scoreNode :: Var QPN
              -> Bool
              -> InstallPlanScore
              -> Score (Tree a (b, ScoringState))
              -> Score (Tree a (b, ScoringState))
    scoreNode var isZero score r = ask >>= \ss ->
      let total = score + ssTotalScore ss
          conflictSet =
            if isZero

              -- If the current node does not affect the score, then there is no
              -- need to add to the conflict set.
              then ssConflictSet ss

              -- Use 'ConflictLessThan' for the current variable.  If we
              -- backtrack to this level after a descendent exceeds the max
              -- score, and this variable has not been added to the conflict set
              -- for any other reason, then we don't need to try any siblings to
              -- the right. Those siblings would only raise the score.
              else CS.insertWithConflictType var ConflictLessThan (ssConflictSet ss)
          ss' = ScoringState total conflictSet
      in local (const ss') r

    initSS :: ScoringState
    initSS = ScoringState {
        ssTotalScore  = 0
      , ssConflictSet = CS.empty
      }

-- | Traversal that tries to establish package stanza enable\/disable
-- preferences. Works by reordering the branches of stanza choices.
preferPackageStanzaPreferences :: (PN -> PackagePreferences) -> Tree a b -> Tree a b
preferPackageStanzaPreferences pcs = trav go
  where
    go (SChoiceF qsn@(SN (PI (Q pp pn) _) s) gr _tr ts) | primaryPP pp =
        let PackagePreferences _ _ spref = pcs pn
            enableStanzaPref = s `elem` spref
            -- move True case first to try enabling the stanza
            ts' = W.mapWeightsWithKey (\k w -> score k : w) ts
            score k
              | enableStanzaPref && not k = 1
              | otherwise                 = 0
         in SChoiceF qsn gr True ts'   -- True: now weak choice
    go x = x

-- | Helper function that tries to enforce a single package constraint on a
-- given instance for a P-node. Translates the constraint into a
-- tree-transformer that either leaves the subtree untouched, or replaces it
-- with an appropriate failure node.
processPackageConstraintP :: PP
                          -> ConflictSet QPN
                          -> I
                          -> LabeledPackageConstraint
                          -> Tree a b
                          -> Tree a b
processPackageConstraintP pp _ _ (LabeledPackageConstraint _ src) r
  | src == ConstraintSourceUserTarget && not (primaryPP pp)         = r
    -- the constraints arising from targets, like "foo-1.0" only apply to
    -- the main packages in the solution, they don't constrain setup deps

processPackageConstraintP _ c i (LabeledPackageConstraint pc src) r = go i pc
  where
    go (I v _) (PackageConstraintVersion _ vr)
        | checkVR vr v  = r
        | otherwise     = Fail c (GlobalConstraintVersion vr src)
    go _       (PackageConstraintInstalled _)
        | instI i       = r
        | otherwise     = Fail c (GlobalConstraintInstalled src)
    go _       (PackageConstraintSource    _)
        | not (instI i) = r
        | otherwise     = Fail c (GlobalConstraintSource src)
    go _       _ = r

-- | Helper function that tries to enforce a single package constraint on a
-- given flag setting for an F-node. Translates the constraint into a
-- tree-transformer that either leaves the subtree untouched, or replaces it
-- with an appropriate failure node.
processPackageConstraintF :: Flag
                          -> ConflictSet QPN
                          -> Bool
                          -> LabeledPackageConstraint
                          -> Tree a b
                          -> Tree a b
processPackageConstraintF f c b' (LabeledPackageConstraint pc src) r = go pc
  where
    go (PackageConstraintFlags _ fa) =
        case L.lookup f fa of
          Nothing            -> r
          Just b | b == b'   -> r
                 | otherwise -> Fail c (GlobalConstraintFlag src)
    go _                             = r

-- | Helper function that tries to enforce a single package constraint on a
-- given flag setting for an F-node. Translates the constraint into a
-- tree-transformer that either leaves the subtree untouched, or replaces it
-- with an appropriate failure node.
processPackageConstraintS :: OptionalStanza
                          -> ConflictSet QPN
                          -> Bool
                          -> LabeledPackageConstraint
                          -> Tree a b
                          -> Tree a b
processPackageConstraintS s c b' (LabeledPackageConstraint pc src) r = go pc
  where
    go (PackageConstraintStanzas _ ss) =
        if not b' && s `elem` ss then Fail c (GlobalConstraintFlag src)
                                 else r
    go _                               = r

-- | Traversal that tries to establish various kinds of user constraints. Works
-- by selectively disabling choices that have been ruled out by global user
-- constraints.
enforcePackageConstraints :: M.Map PN [LabeledPackageConstraint]
                          -> Tree a QGoalReason
                          -> Tree a QGoalReason
enforcePackageConstraints pcs = trav go
  where
    go (PChoiceF qpn@(Q pp pn)              gr      ts) =
      let c = varToConflictSet (P qpn)
          -- compose the transformation functions for each of the relevant constraint
          g = \ (POption i _) -> foldl (\ h pc -> h . processPackageConstraintP pp c i pc) id
                           (M.findWithDefault [] pn pcs)
      in PChoiceF qpn gr      (W.mapWithKey g ts)
    go (FChoiceF qfn@(FN (PI (Q _ pn) _) f) gr tr m ts) =
      let c = varToConflictSet (F qfn)
          -- compose the transformation functions for each of the relevant constraint
          g = \ b -> foldl (\ h pc -> h . processPackageConstraintF f c b pc) id
                           (M.findWithDefault [] pn pcs)
      in FChoiceF qfn gr tr m (W.mapWithKey g ts)
    go (SChoiceF qsn@(SN (PI (Q _ pn) _) f) gr tr   ts) =
      let c = varToConflictSet (S qsn)
          -- compose the transformation functions for each of the relevant constraint
          g = \ b -> foldl (\ h pc -> h . processPackageConstraintS f c b pc) id
                           (M.findWithDefault [] pn pcs)
      in SChoiceF qsn gr tr   (W.mapWithKey g ts)
    go x = x

-- | Transformation that tries to enforce manual flags. Manual flags
-- can only be re-set explicitly by the user. This transformation should
-- be run after user preferences have been enforced. For manual flags,
-- it checks if a user choice has been made. If not, it disables all but
-- the first choice.
enforceManualFlags :: Tree a QGoalReason -> Tree a QGoalReason
enforceManualFlags = trav go
  where
    go (FChoiceF qfn gr tr True ts) = FChoiceF qfn gr tr True $
      let c = varToConflictSet (F qfn)
      in  case span isDisabled (W.toList ts) of
            ([], y : ys) -> W.fromList (y : L.map (\ (w, b, _) -> (w, b, Fail c ManualFlag)) ys)
            _            -> ts -- something has been manually selected, leave things alone
      where
        isDisabled (_, _, Fail _ (GlobalConstraintFlag _)) = True
        isDisabled _                                       = False
    go x                                                   = x

-- | Require installed packages.
requireInstalled :: (PN -> Bool) -> Tree a QGoalReason -> Tree a QGoalReason
requireInstalled p = trav go
  where
    go (PChoiceF v@(Q _ pn) gr cs)
      | p pn      = PChoiceF v gr (W.mapWithKey installed cs)
      | otherwise = PChoiceF v gr                         cs
      where
        installed (POption (I _ (Inst _)) _) x = x
        installed _ _ = Fail (varToConflictSet (P v)) CannotInstall
    go x          = x

-- | Avoid reinstalls.
--
-- This is a tricky strategy. If a package version is installed already and the
-- same version is available from a repo, the repo version will never be chosen.
-- This would result in a reinstall (either destructively, or potentially,
-- shadowing). The old instance won't be visible or even present anymore, but
-- other packages might have depended on it.
--
-- TODO: It would be better to actually check the reverse dependencies of installed
-- packages. If they're not depended on, then reinstalling should be fine. Even if
-- they are, perhaps this should just result in trying to reinstall those other
-- packages as well. However, doing this all neatly in one pass would require to
-- change the builder, or at least to change the goal set after building.
avoidReinstalls :: (PN -> Bool) -> Tree a QGoalReason -> Tree a QGoalReason
avoidReinstalls p = trav go
  where
    go (PChoiceF qpn@(Q _ pn) gr cs)
      | p pn      = PChoiceF qpn gr disableReinstalls
      | otherwise = PChoiceF qpn gr cs
      where
        disableReinstalls =
          let installed = [ v | (_, POption (I v (Inst _)) _, _) <- W.toList cs ]
          in  W.mapWithKey (notReinstall installed) cs

        notReinstall vs (POption (I v InRepo) _) _ | v `elem` vs =
          Fail (varToConflictSet (P qpn)) CannotReinstall
        notReinstall _ _ x =
          x
    go x          = x

-- | Always choose the first goal in the list next, abandoning all
-- other choices.
--
-- This is unnecessary for the default search strategy, because
-- it descends only into the first goal choice anyway,
-- but may still make sense to just reduce the tree size a bit.
firstGoal :: Tree a b -> Tree a b
firstGoal = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.firstOnly xs)
    go x                = x
    -- Note that we keep empty choice nodes, because they mean success.

-- | Transformation that tries to make a decision on base as early as
-- possible. In nearly all cases, there's a single choice for the base
-- package. Also, fixing base early should lead to better error messages.
preferBaseGoalChoice :: Tree a b -> Tree a b
preferBaseGoalChoice = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.preferByKeys isBase xs)
    go x                = x

    isBase :: OpenGoal comp -> Bool
    isBase (OpenGoal (Simple (Dep (Q _pp pn) _) _) _) | unPN pn == "base" = True
    isBase _                                                              = False

-- | Deal with setup dependencies after regular dependencies, so that we can
-- will link setup depencencies against package dependencies when possible
deferSetupChoices :: Tree a b -> Tree a b
deferSetupChoices = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.preferByKeys noSetup xs)
    go x                = x

    noSetup :: OpenGoal comp -> Bool
    noSetup (OpenGoal (Simple (Dep (Q (PP _ns (Setup _)) _) _) _) _) = False
    noSetup _                                                        = True

-- | Transformation that tries to avoid making weak flag choices early.
-- Weak flags are trivial flags (not influencing dependencies) or such
-- flags that are explicitly declared to be weak in the index.
deferWeakFlagChoices :: Tree a b -> Tree a b
deferWeakFlagChoices = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.prefer noWeakStanza (P.prefer noWeakFlag xs))
    go x                = x

    noWeakStanza :: Tree a b -> Bool
    noWeakStanza (SChoice _ _ True _) = False
    noWeakStanza _                    = True

    noWeakFlag :: Tree a b -> Bool
    noWeakFlag (FChoice _ _ True _ _) = False
    noWeakFlag _                      = True

-- | Transformation that sorts choice nodes so that
-- child nodes with a small branching degree are preferred.
--
-- Only approximates the number of choices in the branches.
-- In particular, we try to take any goal immediately if it has
-- a branching degree of 0 (guaranteed failure) or 1 (no other
-- choice possible).
--
-- Returns at most one choice.
--
preferEasyGoalChoices :: Tree a b -> Tree a b
preferEasyGoalChoices = trav go
  where
    -- TODO: --dynamic-goal-reordering requires this function to leave all goal
    -- choices. If we decide to keep the feature, this function should be
    -- refactored to calculate the first element more efficiently, as before:
    -- go (GoalChoiceF xs) = GoalChoiceF (P.dminimumBy dchoices xs)

    go (GoalChoiceF xs) = GoalChoiceF (P.sortBy (comparing dchoices) xs)
      -- (a different implementation that seems slower):
      -- GoalChoiceF (P.firstOnly (P.preferOrElse zeroOrOneChoices (P.minimumBy choices) xs))
    go x                = x

-- | A variant of 'preferEasyGoalChoices' that just keeps the
-- ones with a branching degree of 0 or 1. Note that unlike
-- 'preferEasyGoalChoices', this may return more than one
-- choice.
--
preferReallyEasyGoalChoices :: Tree a b -> Tree a b
preferReallyEasyGoalChoices = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.prefer zeroOrOneChoices xs)
    go x                = x

-- | Monad used internally in enforceSingleInstanceRestriction
--
-- For each package instance we record the goal for which we picked a concrete
-- instance. The SIR means that for any package instance there can only be one.
type EnforceSIR = Reader (Map (PI PN) QPN)

-- | Enforce ghc's single instance restriction
--
-- From the solver's perspective, this means that for any package instance
-- (that is, package name + package version) there can be at most one qualified
-- goal resolving to that instance (there may be other goals _linking_ to that
-- instance however).
enforceSingleInstanceRestriction :: Tree a QGoalReason -> Tree a QGoalReason
enforceSingleInstanceRestriction = (`runReader` M.empty) . cata go
  where
    go :: TreeF a QGoalReason (EnforceSIR (Tree a QGoalReason)) -> EnforceSIR (Tree a QGoalReason)

    -- We just verify package choices.
    go (PChoiceF qpn gr cs) =
      PChoice qpn gr <$> T.sequence (W.mapWithKey (goP qpn) cs)
    go _otherwise =
      innM _otherwise

    -- The check proper
    goP :: QPN -> POption -> EnforceSIR (Tree a QGoalReason) -> EnforceSIR (Tree a QGoalReason)
    goP qpn@(Q _ pn) (POption i linkedTo) r = do
      let inst = PI pn i
      env <- ask
      case (linkedTo, M.lookup inst env) of
        (Just _, _) ->
          -- For linked nodes we don't check anything
          r
        (Nothing, Nothing) ->
          -- Not linked, not already used
          local (M.insert inst qpn) r
        (Nothing, Just qpn') -> do
          -- Not linked, already used. This is an error
          return $ Fail (CS.union (varToConflictSet (P qpn)) (varToConflictSet (P qpn'))) MultipleInstances
