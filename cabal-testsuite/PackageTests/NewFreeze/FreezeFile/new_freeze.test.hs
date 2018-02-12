import Test.Cabal.Prelude
import Control.Monad.IO.Class
import Data.Char
import System.Directory

-- Test for 'cabal new-freeze' with only a single library dependency.
-- my-local-package depends on my-library-dep, which has versions 1.0 and 2.0.
main = cabalTest $ withSourceCopy $
  withRepo "repo" $ do
    cwd <- fmap testCurrentDir getTestEnv
    let freezeFile = cwd </> "cabal.project.freeze"

    shouldNotExist freezeFile

    -- new-build should choose the latest version for the dependency.
    cabal' "new-build" ["--dry-run"] >>= assertUsesLatestDependency

    -- Freeze a dependency on the older version.
    cabal "new-freeze" ["--constraint=my-library-dep==1.0"]

    -- The file should constrain the dependency, but not the local package.
    shouldExist freezeFile
    assertFileDoesContain freezeFile "any.my-library-dep ==1.0"
    assertFileDoesNotContain freezeFile "my-local-package"
  where
    assertUsesLatestDependency out = do
      assertOutputContains "my-library-dep-2.0 (lib)" out
      assertOutputDoesNotContain "my-library-dep-1.0" out
