{-# LANGUAGE CPP #-}
module Distribution.Client.Dependency.Modular.Preference where

-- Reordering or pruning the tree in order to prefer or make certain choices.

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Traversable as T
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import qualified Data.Set as S
import Prelude hiding (sequence)
import Control.Monad.Reader hiding (sequence)
import Data.Ord
import Data.Map (Map)
import Data.Maybe
import Data.Traversable (sequence)

import Distribution.Version

import Distribution.Client.Dependency.Types
  ( InstallPlanScore, PackageConstraint(..), LabeledPackageConstraint(..)
  , PackagePreferences(..), InstalledPreference(..) )
import Distribution.Client.Types
  ( OptionalStanza(..) )

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.PSQ as P
import Distribution.Client.Dependency.Modular.WeightedPSQ as W
import Distribution.Client.Dependency.Modular.Tree
import Distribution.Client.Dependency.Modular.Version

addWeight :: (PN -> [Version] -> POption -> Weight) -> Tree a b -> Tree a b
addWeight f = trav go
  where
    go (PChoiceF v@(Q _ pn) r cs) =
      let sortedVersions = L.sortBy (flip compare) $ L.map version (W.keys cs)
          forceWeights psq = sum (concat (W.weights psq)) `seq` psq
      in  PChoiceF v r $ forceWeights $
          W.mapWeightsWithKey ((:) . f pn sortedVersions) cs
    go x                          = x

version :: POption -> Version
version (POption (I v _) _) = v

-- | Prefer to link packages whenever possible.
preferLinked :: Tree a b -> Tree a b
preferLinked = addWeight (const (const linked))
  where
    linked (POption _ Nothing)  = 1
    linked (POption _ (Just _)) = 0

-- | Traversal that tries to establish package preferences (not constraints).
-- Works by setting weights on choice nodes.
preferPackagePreferences :: (PN -> PackagePreferences) -> Tree a b -> Tree a b
preferPackagePreferences pcs =
    addWeight (const . preferred)

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
    latest :: [Version] -> POption -> Weight
    latest sortedVersions pOpt =
      let index = fromJust $ L.elemIndex (version pOpt) sortedVersions
      in  fromIntegral index / L.genericLength sortedVersions

    preference :: PN -> InstalledPreference
    preference pn =
      let PackagePreferences _ ipref = pcs pn
      in  ipref

    -- Prefer preferred versions.
    preferred :: PN -> POption -> Weight
    preferred pn pOpt =
      let PackagePreferences vr _ = pcs pn
      in  if checkVR vr (version pOpt) then 0 else 1

    -- Prefer installed packages over non-installed packages.
    installed :: POption -> Weight
    installed (POption (I _ (Inst _)) _) = 0
    installed _              = 1

data ScoringState = ScoringState {
      -- | The sum of the scores of all nodes from the root to the current node.
      ssTotalScore  :: InstallPlanScore

      -- | The conflict set that should be used if a node exceeds the max score.
    , ssConflictSet :: ConflictSet QPN
    }
    deriving Show

type PruneWithScore = Reader ScoringState

-- | Traversal that prunes all nodes that exceed the max score, even if they are
-- not 'Done'. It also records the score on 'Done' nodes.
pruneWithMaxScore :: Maybe InstallPlanScore
                  -> Tree a QGoalReasonChain
                  -> Tree InstallPlanScore QGoalReasonChain
pruneWithMaxScore maxScore = (`runReader` initSS) . cata go
  where
    go :: TreeF a QGoalReasonChain
              (PruneWithScore (Tree InstallPlanScore QGoalReasonChain))
       -> PruneWithScore (Tree InstallPlanScore QGoalReasonChain)
    go (PChoiceF qpn gr     cs) =
      PChoice qpn gr     <$> processChildren (P qpn) gr cs
    go (FChoiceF qfn gr t m cs) =
      FChoice qfn gr t m <$> processChildren (F qfn) gr cs
    go (SChoiceF qsn gr t   cs) =
      SChoice qsn gr t   <$> processChildren (S qsn) gr cs
    go (GoalChoiceF         cs)       = GoalChoice     <$> T.sequence cs
    go (DoneF revDepMap _)            = Done revDepMap <$> asks ssTotalScore
    go (FailF conflictSet failReason) = return $ Fail conflictSet failReason

    processChildren :: Var QPN
                    -> QGoalReasonChain
                    -> WeightedPSQ w k (PruneWithScore (Tree a QGoalReasonChain))
                    -> PruneWithScore (WeightedPSQ w k (Tree a QGoalReasonChain))
    processChildren var gr cs =
      let processChild c i = scoreOrPrune var gr (i == 0) (fromIntegral i / l) c
          l = fromIntegral (W.length cs)
      in  l `seq` T.traverse (uncurry processChild) (W.zipWithIndex cs)

    scoreOrPrune :: Var QPN
                 -> QGoalReasonChain
                 -> Bool
                 -> InstallPlanScore
                 -> PruneWithScore (Tree a QGoalReasonChain)
                 -> PruneWithScore (Tree a QGoalReasonChain)
    scoreOrPrune var gr isBest score r = ask >>= \ss ->
      let total = score + ssTotalScore ss
          conflictSet =
            if isBest
              then ssConflictSet ss
              else insertCS var ConflictLessThan $
                   unionCS (goalReasonChainToVars gr) (ssConflictSet ss)
          ss' = ScoringState total conflictSet
      in if total `seq` maybe False (total >) maxScore
           then return $ Fail conflictSet (ExceedsMaxScore total)
           else local (const ss') r

    initSS :: ScoringState
    initSS = ScoringState {
        ssTotalScore  = 0
      , ssConflictSet = M.empty
      }

-- | Helper function that tries to enforce a single package constraint on a
-- given instance for a P-node. Translates the constraint into a
-- tree-transformer that either leaves the subtree untouched, or replaces it
-- with an appropriate failure node.
processPackageConstraintP :: ConflictSet QPN
                          -> I
                          -> LabeledPackageConstraint
                          -> Tree a b
                          -> Tree a b
processPackageConstraintP c i (LabeledPackageConstraint pc src) r = go i pc
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
                          -> Tree a QGoalReasonChain
                          -> Tree a QGoalReasonChain
enforcePackageConstraints pcs = trav go
  where
    go (PChoiceF qpn@(Q _ pn)               gr      ts) =
      let c = toConflictSet (Goal (P qpn) gr)
          -- compose the transformation functions for each of the relevant constraint
          g = \ (POption i _) -> foldl (\ h pc -> h . processPackageConstraintP   c i pc) id
                           (M.findWithDefault [] pn pcs)
      in PChoiceF qpn gr      (W.mapWithKey g ts)
    go (FChoiceF qfn@(FN (PI (Q _ pn) _) f) gr tr m ts) =
      let c = toConflictSet (Goal (F qfn) gr)
          -- compose the transformation functions for each of the relevant constraint
          g = \ b -> foldl (\ h pc -> h . processPackageConstraintF f c b pc) id
                           (M.findWithDefault [] pn pcs)
      in FChoiceF qfn gr tr m (W.mapWithKey g ts)
    go (SChoiceF qsn@(SN (PI (Q _ pn) _) f) gr tr   ts) =
      let c = toConflictSet (Goal (S qsn) gr)
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
enforceManualFlags :: Tree a QGoalReasonChain -> Tree a QGoalReasonChain
enforceManualFlags = trav go
  where
    go (FChoiceF qfn gr tr True ts) = FChoiceF qfn gr tr True $
      let c = toConflictSet (Goal (F qfn) gr)
      in  case span isDisabled (W.toList ts) of
            ([], y : ys) -> W.fromList (y : L.map (\ (w, b, _) -> (w, b, Fail c ManualFlag)) ys)
            _            -> ts -- something has been manually selected, leave things alone
      where
        isDisabled (_, _, Fail _ (GlobalConstraintFlag _)) = True
        isDisabled _                                       = False
    go x                                                   = x

-- | Require installed packages.
requireInstalled :: (PN -> Bool) -> Tree a QGoalReasonChain -> Tree a QGoalReasonChain
requireInstalled p = trav go
  where
    go (PChoiceF v@(Q _ pn) gr cs)
      | p pn      = PChoiceF v gr (W.mapWithKey installed cs)
      | otherwise = PChoiceF v gr                         cs
      where
        installed (POption (I _ (Inst _)) _) x = x
        installed _ _ = Fail (toConflictSet (Goal (P v) gr)) CannotInstall
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
avoidReinstalls :: (PN -> Bool) -> Tree a QGoalReasonChain -> Tree a QGoalReasonChain
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
          Fail (toConflictSet (Goal (P qpn) gr)) CannotReinstall
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
    go (GoalChoiceF xs) = -- casePSQ xs (GoalChoiceF xs) (\ _ t _ -> out t) -- more space efficient, but removes valuable debug info
                          casePSQ xs (GoalChoiceF (P.fromList [])) (\ g t _ -> GoalChoiceF (P.fromList [(g, t)]))
    go x                = x
    -- Note that we keep empty choice nodes, because they mean success.

-- | Transformation that tries to make a decision on base as early as
-- possible. In nearly all cases, there's a single choice for the base
-- package. Also, fixing base early should lead to better error messages.
preferBaseGoalChoice :: Tree a b -> Tree a b
preferBaseGoalChoice = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.sortByKeys preferBase xs)
    go x                = x

    preferBase :: OpenGoal comp -> OpenGoal comp -> Ordering
    preferBase (OpenGoal (Simple (Dep (Q _pp pn) _) _) _) _ | unPN pn == "base" = LT
    preferBase _ (OpenGoal (Simple (Dep (Q _pp pn) _) _) _) | unPN pn == "base" = GT
    preferBase _ _                                                              = EQ

-- | Deal with setup dependencies after regular dependencies, so that we can
-- will link setup depencencies against package dependencies when possible
deferSetupChoices :: Tree a b -> Tree a b
deferSetupChoices = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.sortByKeys deferSetup xs)
    go x                = x

    deferSetup :: OpenGoal comp -> OpenGoal comp -> Ordering
    deferSetup (OpenGoal (Simple (Dep (Q (Setup _ _) _) _) _) _) _ = GT
    deferSetup _ (OpenGoal (Simple (Dep (Q (Setup _ _) _) _) _) _) = LT
    deferSetup _ _                                                 = EQ

-- | Transformation that sorts choice nodes so that
-- child nodes with a small branching degree are preferred. As a
-- special case, choices with 0 branches will be preferred (as they
-- are immediately considered inconsistent), and choices with 1
-- branch will also be preferred (as they don't involve choice).
preferEasyGoalChoices :: Tree a b -> Tree a b
preferEasyGoalChoices = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.sortBy (comparing choices) xs)
    go x                = x

-- | Transformation that tries to avoid making weak flag choices early.
-- Weak flags are trivial flags (not influencing dependencies) or such
-- flags that are explicitly declared to be weak in the index.
deferWeakFlagChoices :: Tree a b -> Tree a b
deferWeakFlagChoices = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.sortBy defer xs)
    go x                = x

    defer :: Tree a b -> Tree a b -> Ordering
    defer (FChoice _ _ True _ _) _ = GT
    defer _ (FChoice _ _ True _ _) = LT
    defer _ _                      = EQ

-- | Variant of 'preferEasyGoalChoices'.
--
-- Only approximates the number of choices in the branches. Less accurate,
-- more efficient.
lpreferEasyGoalChoices :: Tree a b -> Tree a b
lpreferEasyGoalChoices = trav go
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.sortBy (comparing lchoices) xs)
    go x                = x

-- | Variant of 'preferEasyGoalChoices'.
--
-- I first thought that using a paramorphism might be faster here,
-- but it doesn't seem to make any difference.
preferEasyGoalChoices' :: Tree a b -> Tree a b
preferEasyGoalChoices' = para (inn . go)
  where
    go (GoalChoiceF xs) = GoalChoiceF (P.map fst (P.sortBy (comparing (choices . snd)) xs))
    go x                = fmap fst x

-- | Monad used internally in enforceSingleInstanceRestriction
type EnforceSIR = Reader (Map (PI PN) QPN)

-- | Enforce ghc's single instance restriction
--
-- From the solver's perspective, this means that for any package instance
-- (that is, package name + package version) there can be at most one qualified
-- goal resolving to that instance (there may be other goals _linking_ to that
-- instance however).
enforceSingleInstanceRestriction :: Tree a QGoalReasonChain -> Tree a QGoalReasonChain
enforceSingleInstanceRestriction = (`runReader` M.empty) . cata go
  where
    go :: TreeF a QGoalReasonChain (EnforceSIR (Tree a QGoalReasonChain)) -> EnforceSIR (Tree a QGoalReasonChain)

    -- We just verify package choices.
    go (PChoiceF qpn gr cs) =
      PChoice qpn gr <$> sequence (W.mapWithKey (goP qpn) cs)
    go _otherwise =
      innM _otherwise

    -- The check proper
    goP :: QPN -> POption -> EnforceSIR (Tree a QGoalReasonChain) -> EnforceSIR (Tree a QGoalReasonChain)
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
          let cs = M.fromSet (const ConflictAll) (S.fromList [P qpn, P qpn'])
          return $ Fail cs MultipleInstances
