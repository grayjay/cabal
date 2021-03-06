{-# OPTIONS_GHC -fno-warn-orphans #-}
module UnitTests.Distribution.System
    ( tests
    ) where

import Control.Monad (liftM2)
import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.System
import Test.Tasty
import Test.Tasty.QuickCheck

textRoundtrip :: (Show a, Eq a, Pretty a, Parsec a) => a -> Property
textRoundtrip x = simpleParsec (prettyShow x) === Just x

tests :: [TestTree]
tests =
    [ testProperty "Text OS round trip"       (textRoundtrip :: OS -> Property)
    , testProperty "Text Arch round trip"     (textRoundtrip :: Arch -> Property)
    , testProperty "Text Platform round trip" (textRoundtrip :: Platform -> Property)
    ]

instance Arbitrary OS where
    arbitrary = elements knownOSs

instance Arbitrary Arch where
    arbitrary = elements knownArches

instance Arbitrary Platform where
    arbitrary = liftM2 Platform arbitrary arbitrary
