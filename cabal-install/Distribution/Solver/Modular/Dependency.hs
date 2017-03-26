{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
#ifdef DEBUG_CONFLICT_SETS
{-# LANGUAGE ImplicitParams #-}
#endif
module Distribution.Solver.Modular.Dependency (
    -- * Variables
    Var(..)
  , simplifyVar
  , varPI
  , showVar
    -- * Conflict sets
  , ConflictSet
  , ConflictMap
  , CS.showConflictSet
    -- * Constrained instances
  , CI(..)
  , merge
    -- * Flagged dependencies
  , FlaggedDeps
  , FlaggedDep(..)
  , FlagDeps(..)
  , FlagInfo(..)
  , Dep(..)
  , showDep
  , Constraint(..)
  , PackageDep(..)
  , flattenFlaggedDeps
  , QualifyOptions(..)
  , qualifyDeps
  , unqualifyDeps
    -- * Reverse dependency map
  , RevDepMap
    -- * Goals
  , Goal(..)
  , GoalReason(..)
  , QGoalReason
  , ResetVar(..)
  , goalToVar
  , goalVarToConflictSet
  , varToConflictSet
  , goalReasonToVars
  ) where

import Prelude hiding (pi)

import Data.Map (Map)
import qualified Data.List as L

import Language.Haskell.Extension (Extension(..), Language(..))

import Distribution.Text

import Distribution.Solver.Modular.ConflictSet (ConflictSet, ConflictMap)
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Package
import Distribution.Solver.Modular.Var
import Distribution.Solver.Modular.Version
import qualified Distribution.Solver.Modular.ConflictSet as CS

import Distribution.Solver.Types.ComponentDeps (Component(..))
import Distribution.Solver.Types.PackagePath

#ifdef DEBUG_CONFLICT_SETS
import GHC.Stack (CallStack)
#endif

{-------------------------------------------------------------------------------
  Constrained instances
-------------------------------------------------------------------------------}

-- | Constrained instance. If the choice has already been made, this is
-- a fixed instance, and we record the package name for which the choice
-- is for convenience. Otherwise, it is a list of version ranges paired with
-- the goals / variables that introduced them.
data CI qpn = Fixed I (Var qpn) | Constrained [VROrigin qpn]
  deriving (Eq, Show, Functor)

showCI :: CI QPN -> String
showCI (Fixed i _)      = "==" ++ showI i
showCI (Constrained vr) = showVR (collapse vr)

-- | Merge constrained instances. We currently adopt a lazy strategy for
-- merging, i.e., we only perform actual checking if one of the two choices
-- is fixed. If the merge fails, we return a conflict set indicating the
-- variables responsible for the failure, as well as the two conflicting
-- fragments.
--
-- Note that while there may be more than one conflicting pair of version
-- ranges, we only return the first we find.
--
-- TODO: Different pairs might have different conflict sets. We're
-- obviously interested to return a conflict that has a "better" conflict
-- set in the sense the it contains variables that allow us to backjump
-- further. We might apply some heuristics here, such as to change the
-- order in which we check the constraints.
merge ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  CI QPN -> CI QPN -> Either (ConflictSet, (CI QPN, CI QPN)) (CI QPN)
merge c@(Fixed i g1)       d@(Fixed j g2)
  | i == j                                    = Right c
  | otherwise                                 = Left (CS.union (varToConflictSet g1) (varToConflictSet g2), (c, d))
merge c@(Fixed (I v _) g1)   (Constrained rs) = go rs -- I tried "reverse rs" here, but it seems to slow things down ...
  where
    go []              = Right c
    go (d@(vr, g2) : vrs)
      | checkVR vr v   = go vrs
      | otherwise      = Left (CS.union (varToConflictSet g1) (varToConflictSet g2), (c, Constrained [d]))
merge c@(Constrained _)    d@(Fixed _ _)      = merge d c
merge   (Constrained rs)     (Constrained ss) = Right (Constrained (rs ++ ss))

{-------------------------------------------------------------------------------
  Flagged dependencies
-------------------------------------------------------------------------------}

-- | Flagged dependencies
--
-- 'FlaggedDeps' is the modular solver's view of a packages dependencies:
-- rather than having the dependencies indexed by component, each dependency
-- defines what component it is in.
--
-- Note that each dependency is associated with a Component. We must know what
-- component the dependencies belong to, or else we don't be able to construct
-- fine-grained reverse dependencies.
type FlaggedDeps qpn = [FlaggedDep qpn]

-- | Flagged dependencies can be plain dependencies or constraints, or
-- flag-dependent dependency trees.
data FlaggedDep qpn =
    -- | Dependencies which are conditional on a flag choice.
    Flagged (FlagDeps qpn)
    -- | Dependencies which are always enabled, for the component.
  | SimpleDep (PackageDep qpn)  Component
    -- | Constraints which are always enabled.
  | SimpleConstraint Constraint Component
  deriving (Eq, Show)

-- | A flag or stanza paired with two alternate dependency trees.
data FlagDeps qpn =
    FlagDeps (FlagInfo qpn) (FlaggedDeps qpn) (FlaggedDeps qpn)
  deriving (Eq, Show)

-- | Information about a solver flag, which can be either a package flag
-- or stanza.
data FlagInfo qpn =
    -- | Information about a package flag.
    PkgFlagInfo   (FN qpn) FInfo
    -- | Information about a test/benchmark stanza.
  | StanzaInfo (SN qpn)
  deriving (Eq, Show)

-- | Conversatively flatten out flagged dependencies
--
-- NOTE: We do not include constraints or filter out duplicate dependencies.
flattenFlaggedDeps :: FlaggedDeps qpn -> [(PackageDep qpn, Component)]
flattenFlaggedDeps = concatMap aux
  where
    aux :: FlaggedDep qpn -> [(PackageDep qpn, Component)]
    aux (Flagged (FlagDeps _ t f)) = flattenFlaggedDeps t ++ flattenFlaggedDeps f
    aux (SimpleDep d c)            = [(d, c)]
    aux (SimpleConstraint _ _)     = []

-- | Is this dependency on an executable
type IsExe = Bool

-- | A single dependency that the solver must satisfy.
data Dep qpn =
    GoalDep (PackageDep qpn)  -- ^ A dependency that introduces a package goal.
  | ConstraintDep Constraint  -- ^ A dependency that does not introduce a goal.
  deriving (Eq, Show)

-- | A 'PackageDep' associates a package name with a constrained instance.
--
-- 'PackageDep' intentionally has no 'Functor' instance because the type variable
-- is used both to record the dependencies as well as who's doing the
-- depending; having a 'Functor' instance makes bugs where we don't distinguish
-- these two far too likely. (By rights 'PackageDep' ought to have two type variables.)
data PackageDep qpn = PackageDep IsExe qpn (CI qpn)  -- ^ dependency on a package (possibly for executable
  deriving (Eq, Show)

-- | A dependency that does not introduce a goal.
data Constraint = 
    Ext  Extension         -- ^ dependency on a language extension
  | Lang Language          -- ^ dependency on a language version
  | Pkg  PkgconfigName VR  -- ^ dependency on a pkg-config package
  deriving (Eq, Show)

showDep :: Dep QPN -> String
showDep (GoalDep (PackageDep is_exe qpn (Fixed i v)            )) =
  (if P qpn /= v then showVar v ++ " => " else "") ++
  showQPN qpn ++
  (if is_exe then " (exe) " else "") ++ "==" ++ showI i
showDep (GoalDep (PackageDep is_exe qpn (Constrained [(vr, v)]))) =
  showVar v ++ " => " ++ showQPN qpn ++
  (if is_exe then " (exe) " else "") ++ showVR vr
showDep (GoalDep (PackageDep is_exe qpn ci                     )) =
  showQPN qpn ++ (if is_exe then " (exe) " else "") ++ showCI ci
showDep (ConstraintDep (Ext ext))   = "requires " ++ display ext
showDep (ConstraintDep (Lang lang)) = "requires " ++ display lang
showDep (ConstraintDep (Pkg pn vr)) = "requires pkg-config package "
                      ++ display pn ++ display vr
                      ++ ", not found in the pkg-config database"

-- | Options for goal qualification (used in 'qualifyDeps')
--
-- See also 'defaultQualifyOptions'
data QualifyOptions = QO {
    -- | Do we have a version of base relying on another version of base?
    qoBaseShim :: Bool

    -- Should dependencies of the setup script be treated as independent?
  , qoSetupIndependent :: Bool
  }
  deriving Show

-- | Apply built-in rules for package qualifiers
--
-- Although the behaviour of 'qualifyDeps' depends on the 'QualifyOptions',
-- it is important that these 'QualifyOptions' are _static_. Qualification
-- does NOT depend on flag assignment; in other words, it behaves the same no
-- matter which choices the solver makes (modulo the global 'QualifyOptions');
-- we rely on this in 'linkDeps' (see comment there).
--
-- NOTE: It's the _dependencies_ of a package that may or may not be independent
-- from the package itself. Package flag choices must of course be consistent.
qualifyDeps :: QualifyOptions -> QPN -> FlaggedDeps PN -> FlaggedDeps QPN
qualifyDeps QO{..} (Q pp@(PackagePath ns q) pn) = go
  where
    go :: FlaggedDeps PN -> FlaggedDeps QPN
    go = map go1

    go1 :: FlaggedDep PN -> FlaggedDep QPN
    go1 (Flagged (FlagDeps (PkgFlagInfo fn nfo) t f)) =
         Flagged (FlagDeps (PkgFlagInfo (fmap (Q pp) fn) nfo) (go t) (go f))
    go1 (Flagged (FlagDeps (StanzaInfo sn) t f)) =
         Flagged (FlagDeps (StanzaInfo (fmap (Q pp) sn)) (go t) (go f))
    go1 (SimpleDep dep comp)    = SimpleDep (goD dep comp) comp
    go1 (SimpleConstraint constr comp)    = SimpleConstraint constr comp

    -- Suppose package B has a setup dependency on package A.
    -- This will be recorded as something like
    --
    -- > PackageDep "A" (Constrained [(AnyVersion, Goal (P "B") reason])
    --
    -- Observe that when we qualify this dependency, we need to turn that
    -- @"A"@ into @"B-setup.A"@, but we should not apply that same qualifier
    -- to the goal or the goal reason chain.
    goD :: PackageDep PN -> Component -> PackageDep QPN
    goD (PackageDep is_exe dep ci) comp
      | is_exe      = PackageDep is_exe (Q (PackagePath ns (QualExe pn dep)) dep) (fmap (Q pp) ci)
      | qBase  dep  = PackageDep is_exe (Q (PackagePath ns (QualBase  pn)) dep) (fmap (Q pp) ci)
      | qSetup comp = PackageDep is_exe (Q (PackagePath ns (QualSetup pn)) dep) (fmap (Q pp) ci)
      | otherwise   = PackageDep is_exe (Q (PackagePath ns inheritedQ) dep) (fmap (Q pp) ci)

    -- If P has a setup dependency on Q, and Q has a regular dependency on R, then
    -- we say that the 'Setup' qualifier is inherited: P has an (indirect) setup
    -- dependency on R. We do not do this for the base qualifier however.
    --
    -- The inherited qualifier is only used for regular dependencies; for setup
    -- and base deppendencies we override the existing qualifier. See #3160 for
    -- a detailed discussion.
    inheritedQ :: Qualifier
    inheritedQ = case q of
                   QualSetup _  -> q
                   QualExe _ _  -> q
                   QualToplevel -> q
                   QualBase _   -> QualToplevel

    -- Should we qualify this goal with the 'Base' package path?
    qBase :: PN -> Bool
    qBase dep = qoBaseShim && unPackageName dep == "base"

    -- Should we qualify this goal with the 'Setup' package path?
    qSetup :: Component -> Bool
    qSetup comp = qoSetupIndependent && comp == ComponentSetup

-- | Remove qualifiers from set of dependencies
--
-- This is used during link validation: when we link package @Q.A@ to @Q'.A@,
-- then all dependencies @Q.B@ need to be linked to @Q'.B@. In order to compute
-- what to link these dependencies to, we need to requalify @Q.B@ to become
-- @Q'.B@; we do this by first removing all qualifiers and then calling
-- 'qualifyDeps' again.
unqualifyDeps :: FlaggedDeps QPN -> FlaggedDeps PN
unqualifyDeps = go
  where
    go :: FlaggedDeps QPN -> FlaggedDeps PN
    go = map go1

    go1 :: FlaggedDep QPN -> FlaggedDep PN
    go1 (Flagged (FlagDeps (PkgFlagInfo fn nfo) t f)) = Flagged (FlagDeps (PkgFlagInfo (fmap unq fn) nfo) (go t) (go f))
    go1 (Flagged (FlagDeps (StanzaInfo sn) t f)) = Flagged (FlagDeps (StanzaInfo (fmap unq sn)) (go t) (go f))
    go1 (SimpleDep dep comp)    = SimpleDep (goD dep) comp
    go1 (SimpleConstraint constr comp)    = SimpleConstraint constr comp

    goD :: PackageDep QPN -> PackageDep PN
    goD (PackageDep is_exe qpn ci) = PackageDep is_exe (unq qpn) (fmap unq ci)

    unq :: QPN -> PN
    unq (Q _ pn) = pn

{-------------------------------------------------------------------------------
  Reverse dependency map
-------------------------------------------------------------------------------}

-- | A map containing reverse dependencies between qualified
-- package names.
type RevDepMap = Map QPN [(Component, QPN)]

{-------------------------------------------------------------------------------
  Goals
-------------------------------------------------------------------------------}

-- | A goal is just a solver variable paired with a reason.
-- The reason is only used for tracing.
data Goal qpn = Goal (Var qpn) (GoalReason qpn)
  deriving (Eq, Show, Functor)

-- | Reason why a goal is being added to a goal set.
data GoalReason qpn =
    UserGoal
  | PDependency (PI qpn)
  | FDependency (FN qpn) Bool
  | SDependency (SN qpn)
  deriving (Eq, Show, Functor)

type QGoalReason = GoalReason QPN

class ResetVar f where
  resetVar :: Var qpn -> f qpn -> f qpn

instance ResetVar CI where
  resetVar v (Fixed i _)       = Fixed i v
  resetVar v (Constrained vrs) =
      Constrained (L.map (\ (x, y) -> (x, resetVar v y)) vrs)

instance ResetVar Dep where
  resetVar v (GoalDep (PackageDep is_exe qpn ci)) =
      GoalDep $ PackageDep is_exe qpn (resetVar v ci)
  resetVar _ cd@(ConstraintDep _) = cd

instance ResetVar PackageDep where
  resetVar v (PackageDep is_exe qpn ci) = PackageDep is_exe qpn (resetVar v ci)

instance ResetVar Var where
  resetVar = const

goalToVar :: Goal a -> Var a
goalToVar (Goal v _) = v

-- | Compute a singleton conflict set from a goal, containing just
-- the goal variable.
--
-- NOTE: This is just a call to 'varToConflictSet' under the hood;
-- the 'GoalReason' is ignored.
goalVarToConflictSet :: Goal QPN -> ConflictSet
goalVarToConflictSet (Goal g _gr) = varToConflictSet g

-- | Compute a singleton conflict set from a 'Var'
varToConflictSet :: Var QPN -> ConflictSet
varToConflictSet = CS.singleton

-- | A goal reason is mostly just a variable paired with the
-- decision we made for that variable (except for user goals,
-- where we cannot really point to a solver variable). This
-- function drops the decision and recovers the list of
-- variables (which will be empty or contain one element).
--
goalReasonToVars :: GoalReason qpn -> [Var qpn]
goalReasonToVars UserGoal                 = []
goalReasonToVars (PDependency (PI qpn _)) = [P qpn]
goalReasonToVars (FDependency qfn _)      = [F qfn]
goalReasonToVars (SDependency qsn)        = [S qsn]

{-------------------------------------------------------------------------------
  Version ranges paired with origins
-------------------------------------------------------------------------------}

type VROrigin qpn = (VR, Var qpn)

-- | Helper function to collapse a list of version ranges with origins into
-- a single, simplified, version range.
collapse :: [VROrigin qpn] -> VR
collapse = simplifyVR . L.foldr ((.&&.) . fst) anyVR
