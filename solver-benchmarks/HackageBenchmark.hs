{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module HackageBenchmark (
    hackageBenchmarkMain

-- Exposed for testing:
  , CabalResult(..)
  , combineTrialResults
  , isSignificant
  , isInterestingResultPair
  , shouldSkipAfterTrial1
  ) where

import Control.Monad (forM_, replicateM, unless, when)
import qualified Data.ByteString as B
import Data.List (nub, unzip4)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import qualified Data.Vector.Unboxed as V
import qualified Options.Applicative as O
import Statistics.Sample (mean, stdDev)
import Statistics.Test.MannWhitneyU (PositionTest(..), TestResult(..), mannWhitneyUtest)
import Statistics.Types (PValue, mkPValue)
import System.Exit (ExitCode(..), die)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
import System.Process ( StdStream(CreatePipe), CreateProcess(..), callProcess
                      , createProcess, readProcess, shell, waitForProcess )
import Text.Printf (PrintfArg(..), printf)

import Distribution.Package (PackageName, mkPackageName, unPackageName)

data Args = Args {
    argCabal1                      :: FilePath
  , argCabal2                      :: FilePath
  , argCabal1Flags                 :: [String]
  , argCabal2Flags                 :: [String]
  , argPackages                    :: [PackageName]
  , argMinRunTimeDifferenceToRerun :: Double
  , argPValue                      :: Double
  , argTrials                      :: Int
  , argPrintTrials                 :: Bool
  , argPrintSkippedPackages        :: Bool
  , argTimeoutSeconds              :: Int
  }

data CabalTrial = CabalTrial NominalDiffTime CabalResult

data CabalResult
  = Success
  | NoInstallPlan
  | BackjumpLimit
  | PkgNotFound
  | Timeout
  | Unknown
  deriving (Eq, Show)

hackageBenchmarkMain :: IO ()
hackageBenchmarkMain = do
  hSetBuffering stdout LineBuffering
  args@Args {..} <- O.execParser parserInfo
  checkArgs args
  printConfig args
  pkgs <- getPackages args
  putStrLn ""

  let -- The maximum length of the heading and package names.
      nameColumnWidth :: Int
      nameColumnWidth =
          maximum $ map length $ "package" : map unPackageName pkgs
      runCabal1 = runCabal argTimeoutSeconds argCabal1 argCabal1Flags
      runCabal2 = runCabal argTimeoutSeconds argCabal2 argCabal2Flags

  -- When the output contains both trails and summaries, label each row as
  -- "trial" or "summary".
  when argPrintTrials $ putStr $ printf "%-16s " "trial/summary"
  putStrLn $
      printf "%-*s %-13s %-13s %11s %11s %11s %11s %11s"
             nameColumnWidth "package" "result1" "result2"
             "mean1" "mean2" "stddev1" "stddev2" "speedup"

  forM_ pkgs $ \pkg -> do
    let printTrial msgType result1 result2 time1 time2 =
            putStrLn $
            printf "%-16s %-*s %-13s %-13s %10.3fs %10.3fs"
                   msgType nameColumnWidth (unPackageName pkg)
                   result1 result2 time1 time2

    CabalTrial t1 r1 <- runCabal1 pkg
    CabalTrial t2 r2 <- runCabal2 pkg
    if shouldSkipAfterTrial1 argMinRunTimeDifferenceToRerun t1 t2 r1 r2
    then when argPrintSkippedPackages $
         if argPrintTrials
         then printTrial "trial (skipping)" r1 r2 t1 t2
         else putStrLn $ printf "%-*s (first run times were too similar)"
                                nameColumnWidth (unPackageName pkg)
    else do
      when argPrintTrials $ printTrial "trial" r1 r2 t1 t2
      (ts1, ts2, rs1, rs2) <- (unzip4 . ((t1, t2, r1, r2) :) <$>)
                            . replicateM (argTrials - 1) $ do
        CabalTrial t1' r1' <- runCabal1 pkg
        CabalTrial t2' r2' <- runCabal2 pkg
        when argPrintTrials $ printTrial "trial" r1' r2' t1' t2'
        return (t1', t2', r1', r2')

      let result1 = combineTrialResults rs1
          result2 = combineTrialResults rs2
          times1 = V.fromList (map diffTimeToDouble ts1)
          times2 = V.fromList (map diffTimeToDouble ts2)
          mean1 = mean times1
          mean2 = mean times2
          stddev1 = stdDev times1
          stddev2 = stdDev times2
          speedup = mean1 / mean2

      when argPrintTrials $ putStr $ printf "%-16s " "summary"
      if isInterestingResultPair result1 result2
          || isSignificant (mkPValue argPValue) ts1 ts2
      then putStrLn $
           printf "%-*s %-13s %-13s %10.3fs %10.3fs %10.3fs %10.3fs %10.3f"
                  nameColumnWidth (unPackageName pkg)
                  result1 result2 mean1 mean2 stddev1 stddev2 speedup
      else when (argPrintTrials || argPrintSkippedPackages) $
           putStrLn $
           printf "%-*s (not significant)" nameColumnWidth (unPackageName pkg)
  where
    checkArgs :: Args -> IO ()
    checkArgs Args {..} = do
      unless (argTrials > 0) $ die "--trials must be greater than 0."
      unless (argMinRunTimeDifferenceToRerun >= 0) $
          die "--min-run-time-percentage-difference-to-rerun must be non-negative."

    printConfig :: Args -> IO ()
    printConfig Args {..} = do
      putStrLn "Comparing:"
      putStrLn $ "1: " ++ argCabal1 ++ " " ++ unwords argCabal1Flags
      callProcess argCabal1 ["--version"]
      putStrLn $ "2: " ++ argCabal2 ++ " " ++ unwords argCabal2Flags
      callProcess argCabal2 ["--version"]
      -- TODO: Print index state.
      putStrLn "Base package database:"
      callProcess "ghc-pkg" ["list"]

    getPackages :: Args -> IO [PackageName]
    getPackages Args {..} = do
      pkgs <-
          if null argPackages
          then do
            putStrLn $ "Obtaining the package list (using " ++ argCabal1 ++ ") ..."
            list <- readProcess argCabal1 ["list", "--simple-output"] ""
            return $ nub [mkPackageName $ head (words line) | line <- lines list]
          else do
            putStrLn "Using given package list ..."
            return argPackages
      putStrLn $ "Done, got " ++ show (length pkgs) ++ " packages."
      return pkgs

runCabal :: Int -> FilePath -> [String] -> PackageName -> IO CabalTrial
runCabal timeoutSeconds cabal flags pkg = do
  ((exitCode, err), time) <- timeEvent $ do
    let timeout = "timeout --foreground -sINT " ++ show timeoutSeconds
        cabalCmd =
            unwords $
            [cabal, "install", unPackageName pkg, "--dry-run", "-v0"] ++ flags
        cmd = (shell (timeout ++ " " ++ cabalCmd)) { std_err = CreatePipe }

    (_, _, Just errh, ph) <- createProcess cmd
    err <- B.hGetContents errh
    (, err) <$> waitForProcess ph
  let exhaustiveMsg =
          "After searching the rest of the dependency tree exhaustively"
      result
        | exitCode == ExitSuccess                                  = Success
        | exitCode == ExitFailure 124                              = Timeout
        | fromString exhaustiveMsg `B.isInfixOf` err               = NoInstallPlan
        | fromString "Backjump limit reached" `B.isInfixOf` err    = BackjumpLimit
        | fromString "There is no package named" `B.isInfixOf` err = PkgNotFound
        | otherwise                                                = Unknown
  return (CabalTrial time result)

isSignificant :: PValue Double -> [NominalDiffTime] -> [NominalDiffTime] -> Bool
isSignificant pvalue xs ys =
  let toVector = V.fromList . map diffTimeToDouble
  in case mannWhitneyUtest SamplesDiffer pvalue (toVector xs) (toVector ys) of
       Nothing             -> error "not enough data for mannWhitneyUtest"
       Just Significant    -> True
       Just NotSignificant -> False

-- Should we stop after the first trial of this package to save time? This
-- function skips the package if the results are uninteresting and the times are
-- within --min-run-time-percentage-difference-to-rerun.
shouldSkipAfterTrial1 :: Double
                      -> NominalDiffTime
                      -> NominalDiffTime
                      -> CabalResult
                      -> CabalResult
                      -> Bool
shouldSkipAfterTrial1 0                            _  _  _       _       = False
shouldSkipAfterTrial1 _                            _  _  Timeout Timeout = True
shouldSkipAfterTrial1 maxRunTimeDifferenceToIgnore t1 t2 r1      r2      =
    not (isInterestingResultPair r1 r2)
 && abs (t1 - t2) / min t1 t2 < realToFrac (maxRunTimeDifferenceToIgnore / 100)

isInterestingResultPair :: CabalResult -> CabalResult -> Bool
isInterestingResultPair r1 r2 = r1 /= r2 || not (isExpectedResult r1)

-- Is this result expected in a benchmark run on all of Hackage?
isExpectedResult :: CabalResult -> Bool
isExpectedResult Success       = True
isExpectedResult NoInstallPlan = True
isExpectedResult BackjumpLimit = True
isExpectedResult Timeout       = True
isExpectedResult PkgNotFound   = False
isExpectedResult Unknown       = False

-- Combine CabalResults from multiple trials. Ignoring timeouts, all results
-- should be the same. If they aren't the same, we returns Unknown.
combineTrialResults :: [CabalResult] -> CabalResult
combineTrialResults rs
  | allEqual rs                          = head rs
  | allEqual [r | r <- rs, r /= Timeout] = Timeout
  | otherwise                            = Unknown
  where
    allEqual :: Eq a => [a] -> Bool
    allEqual xs = length (nub xs) == 1

timeEvent :: IO a -> IO (a, NominalDiffTime)
timeEvent task = do
  start <- getCurrentTime
  r <- task
  end <- getCurrentTime
  return (r, diffUTCTime end start)

diffTimeToDouble :: NominalDiffTime -> Double
diffTimeToDouble = fromRational . toRational

instance PrintfArg NominalDiffTime where
  formatArg = formatArg . diffTimeToDouble

instance PrintfArg CabalResult where
  formatArg = formatArg . show

parserInfo :: O.ParserInfo Args
parserInfo = O.info (argParser O.<**> O.helper)
     ( O.fullDesc
    <> O.progDesc ("Find differences between two cabal commands when solving"
                   ++ " for all packages on Hackage.")
    <> O.header "hackage-benchmark" )

argParser :: O.Parser Args
argParser = Args
    <$> O.strOption
         ( O.long "cabal1"
        <> O.metavar "PATH"
        <> O.help "First cabal executable")
    <*> O.strOption
         ( O.long "cabal2"
        <> O.metavar "PATH"
        <> O.help "Second cabal executable")
    <*> O.option (words <$> O.str)
         ( O.long "cabal1-flags"
        <> O.value []
        <> O.metavar "FLAGS"
        <> O.help "Extra flags for the first cabal executable")
    <*> O.option (words <$> O.str)
         ( O.long "cabal2-flags"
        <> O.value []
        <> O.metavar "FLAGS"
        <> O.help "Extra flags for the second cabal executable")
    <*> O.option (map mkPackageName . words <$> O.str)
         ( O.long "packages"
        <> O.value []
        <> O.metavar "PACKAGES"
        <> O.help ("Space separated list of packages to test, or all of Hackage"
                   ++ " if unspecified"))
    <*> O.option O.auto
         ( O.long "min-run-time-percentage-difference-to-rerun"
        <> O.showDefault
        <> O.value 0.0
        <> O.metavar "PERCENTAGE"
        <> O.help ("Stop testing a package when the difference in run times in"
                   ++ " the first trial are within this percentage, in order to"
                   ++ " save time"))
    <*> O.option O.auto
         ( O.long "pvalue"
        <> O.showDefault
        <> O.value 0.05
        <> O.metavar "DOUBLE"
        <> O.help ("p-value used to determine whether to print the results for"
                   ++ " each package"))
    <*> O.option O.auto
         ( O.long "trials"
        <> O.showDefault
        <> O.value 10
        <> O.metavar "N"
        <> O.help "Number of trials for each package")
    <*> O.switch
         ( O.long "print-trials"
        <> O.help "Whether to include the results from individual trials in the output")
    <*> O.switch
         ( O.long "print-skipped-packages"
        <> O.help "Whether to include skipped packages in the output")
    <*> O.option O.auto
         ( O.long "timeout"
        <> O.showDefault
        <> O.value 90
        <> O.metavar "SECONDS"
        <> O.help "Maximum time to run a cabal command, in seconds")
