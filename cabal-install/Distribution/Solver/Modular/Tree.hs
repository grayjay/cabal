{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Distribution.Solver.Modular.Tree
    ( FailReason(..)
    , POption(..)
    , Tree(..)
    , TreeF(..)
    , Weight
    , ana
    , cata
    , choices
    , dchoices
    , inn
    , innM
    , para
    , trav
    , zeroOrOneChoices
    ) where

import Control.Monad hiding (mapM, sequence)
import Data.Foldable
import Data.Traversable
import Prelude hiding (foldr, mapM, sequence)

import Distribution.Solver.Modular.Degree
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Package
import Distribution.Solver.Modular.PSQ (PSQ)
import Distribution.Solver.Modular.Version
import Distribution.Solver.Modular.WeightedPSQ (WeightedPSQ)
import qualified Distribution.Solver.Modular.WeightedPSQ as W
import Distribution.Solver.Types.Settings
import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.PackagePath

type Weight = Double

-- | Type of the search tree. Inlining the choice nodes for now.
data Tree a b =
    -- | Choose a version for a package (or choose to link)
    PChoice QPN b (WeightedPSQ [Weight] POption (Tree a b))

    -- | Choose a value for a flag
    --
    -- The Bool indicates whether it's manual.
  | FChoice QFN b WeakOrTrivial Bool (WeightedPSQ [Weight] Bool (Tree a b))

    -- | Choose whether or not to enable a stanza
  | SChoice QSN b WeakOrTrivial (WeightedPSQ [Weight] Bool (Tree a b))

    -- | Choose which choice to make next
    --
    -- Invariants:
    --
    -- * PSQ should never be empty
    -- * For each choice we additionally record the 'QGoalReason' why we are
    --   introducing that goal into tree. Note that most of the time we are
    --   working with @Tree QGoalReason@; in that case, we must have the
    --   invariant that the 'QGoalReason' cached in the 'PChoice', 'FChoice'
    --   or 'SChoice' directly below a 'GoalChoice' node must equal the reason
    --   recorded on that 'GoalChoice' node.
  | GoalChoice (PSQ (Goal QPN) (Tree a b))

    -- | We're done -- we found a solution!
  | Done RevDepMap a

    -- | We failed to find a solution in this path through the tree
  | Fail (ConflictSet QPN) FailReason
  deriving (Eq, Show, Functor)

-- | A package option is a package instance with an optional linking annotation
--
-- The modular solver has a number of package goals to solve for, and can only
-- pick a single package version for a single goal. In order to allow to
-- install multiple versions of the same package as part of a single solution
-- the solver uses qualified goals. For example, @0.P@ and @1.P@ might both
-- be qualified goals for @P@, allowing to pick a difference version of package
-- @P@ for @0.P@ and @1.P@.
--
-- Linking is an essential part of this story. In addition to picking a specific
-- version for @1.P@, the solver can also decide to link @1.P@ to @0.P@ (or
-- vice versa). It means that @1.P@ and @0.P@ really must be the very same package
-- (and hence must have the same build time configuration, and their
-- dependencies must also be the exact same).
--
-- See <http://www.well-typed.com/blog/2015/03/qualified-goals/> for details.
data POption = POption I (Maybe PackagePath)
  deriving (Eq, Show)

data FailReason = InconsistentInitialConstraints
                | Conflicting [Dep QPN]
                | CannotInstall
                | CannotReinstall
                | Shadowed
                | Broken
                | GlobalConstraintVersion VR ConstraintSource
                | GlobalConstraintInstalled ConstraintSource
                | GlobalConstraintSource ConstraintSource
                | GlobalConstraintFlag ConstraintSource
                | ManualFlag
                | MalformedFlagChoice QFN
                | MalformedStanzaChoice QSN
                | EmptyGoalChoice
                | Backjump
                | MultipleInstances
                | DependenciesNotLinked String
                | CyclicDependencies
                | ExceedsMaxScore InstallPlanScore
                | SearchingForBetterScore InstallPlanScore
  deriving (Eq, Show)

-- | Functor for the tree type.
data TreeF a b c =
    PChoiceF    QPN b                    (WeightedPSQ [Weight] POption c)
  | FChoiceF    QFN b WeakOrTrivial Bool (WeightedPSQ [Weight] Bool    c)
  | SChoiceF    QSN b WeakOrTrivial      (WeightedPSQ [Weight] Bool    c)
  | GoalChoiceF                          (PSQ (Goal QPN) c)
  | DoneF       RevDepMap a
  | FailF       (ConflictSet QPN) FailReason
  deriving (Functor, Foldable, Traversable)

out :: Tree a b -> TreeF a b (Tree a b)
out (PChoice    p i     ts) = PChoiceF    p i     ts
out (FChoice    p i b m ts) = FChoiceF    p i b m ts
out (SChoice    p i b   ts) = SChoiceF    p i b   ts
out (GoalChoice         ts) = GoalChoiceF         ts
out (Done       x s       ) = DoneF       x s
out (Fail       c x       ) = FailF       c x

inn :: TreeF a b (Tree a b) -> Tree a b
inn (PChoiceF    p i     ts) = PChoice    p i     ts
inn (FChoiceF    p i b m ts) = FChoice    p i b m ts
inn (SChoiceF    p i b   ts) = SChoice    p i b   ts
inn (GoalChoiceF         ts) = GoalChoice         ts
inn (DoneF       x s       ) = Done       x s
inn (FailF       c x       ) = Fail       c x

innM :: Monad m => TreeF a b (m (Tree a b)) -> m (Tree a b)
innM (PChoiceF    p i     ts) = liftM (PChoice    p i    ) (sequence ts)
innM (FChoiceF    p i b m ts) = liftM (FChoice    p i b m) (sequence ts)
innM (SChoiceF    p i b   ts) = liftM (SChoice    p i b  ) (sequence ts)
innM (GoalChoiceF         ts) = liftM (GoalChoice        ) (sequence ts)
innM (DoneF       x s       ) = return $ Done     x s
innM (FailF       c x       ) = return $ Fail     c x

-- | Determines whether a tree is active, i.e., isn't a failure node.
active :: Tree a b -> Bool
active (Fail _ _) = False
active _          = True

-- | Determines how many active choices are available in a node. Note that we
-- count goal choices as having one choice, always.
choices :: Tree a b -> Int
choices (PChoice    _ _     ts) = W.length (W.filter active ts)
choices (FChoice    _ _ _ _ ts) = W.length (W.filter active ts)
choices (SChoice    _ _ _   ts) = W.length (W.filter active ts)
choices (GoalChoice         _ ) = 1
choices (Done       _ _       ) = 1
choices (Fail       _ _       ) = 0

-- | Variant of 'choices' that only approximates the number of choices.
dchoices :: Tree a b -> Degree
dchoices (PChoice    _ _     ts) = W.degree (W.filter active ts)
dchoices (FChoice    _ _ _ _ ts) = W.degree (W.filter active ts)
dchoices (SChoice    _ _ _   ts) = W.degree (W.filter active ts)
dchoices (GoalChoice         _ ) = ZeroOrOne
dchoices (Done       _ _       ) = ZeroOrOne
dchoices (Fail       _ _       ) = ZeroOrOne

-- | Variant of 'choices' that only approximates the number of choices.
zeroOrOneChoices :: Tree a b -> Bool
zeroOrOneChoices (PChoice    _ _     ts) = W.isZeroOrOne (W.filter active ts)
zeroOrOneChoices (FChoice    _ _ _ _ ts) = W.isZeroOrOne (W.filter active ts)
zeroOrOneChoices (SChoice    _ _ _   ts) = W.isZeroOrOne (W.filter active ts)
zeroOrOneChoices (GoalChoice         _ ) = True
zeroOrOneChoices (Done       _ _       ) = True
zeroOrOneChoices (Fail       _ _       ) = True

-- | Catamorphism on trees.
cata :: (TreeF a b c -> c) -> Tree a b -> c
cata phi x = (phi . fmap (cata phi) . out) x

trav :: (TreeF a b (Tree a c) -> TreeF a c (Tree a c)) -> Tree a b -> Tree a c
trav psi x = cata (inn . psi) x

-- | Paramorphism on trees.
para :: (TreeF a b (c, Tree a b) -> c) -> Tree a b -> c
para phi = phi . fmap (\ x -> (para phi x, x)) . out

-- | Anamorphism on trees.
ana :: (c -> TreeF a b c) -> c -> Tree a b
ana psi = inn . fmap (ana psi) . psi
