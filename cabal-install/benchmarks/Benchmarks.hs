module Main (main) where

import Distribution.Simple.Configure (findDistPrefOrDefault)
import Distribution.Simple.Program.Types
        (ConfiguredProgram, Program(..), simpleProgram, programPath)
import Distribution.Simple.Program.Find
        (ProgramSearchPathEntry(ProgramSearchPathDir), defaultProgramSearchPath)
import Distribution.Simple.Setup ( Flag(..) )
import Distribution.Simple.Utils (die, withTempDirectory)
import Distribution.Verbosity (normal)
import Distribution.Simple.Program.Db
        (defaultProgramDb, requireProgram, setProgramSearchPath)
import Distribution.Simple.Utils ( findProgramVersion )

import Control.Concurrent (forkIO)
import Control.Exception (evaluate)
import Control.Monad (unless, void, when)
import Data.List (isInfixOf)
import System.Directory (canonicalizePath, getTemporaryDirectory)
import System.Exit ( ExitCode(..) )
import System.FilePath ((</>))
import System.Process
        ( CreateProcess(..), StdStream(CreatePipe)
        , createProcess, proc, waitForProcess )
import System.IO (BufferMode(..), hGetContents, hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  testSolverMemoryUsage

testSolverMemoryUsage :: IO ()
testSolverMemoryUsage = do
  cabal <- findCabal
  let mbj1 = 1000
      mbj2 = 10000
  bytes1 <- run cabal mbj1

-- Testing absolute memory usage doens't work very well, since it can change
-- for many reasons.
  -- when (bytes1 > 10^(7 :: Int)) $
  --   die "Used more than 10MB."

  bytes2 <- run cabal mbj2
  when (fromIntegral bytes2 / fromIntegral bytes1 > (1.05 :: Double)) $
      die $
      "Average bytes used increased by more than 5% between "
      ++ show mbj1 ++ " and " ++ show mbj2 ++ " backjumps."

run :: ConfiguredProgram -> Int -> IO Int
run cabal backjumps = do
  putStrLn $ show backjumps ++ " backjumps"
  tempDir <- getTemporaryDirectory
  withTempDirectory normal tempDir "" $ \dir -> do
    let tempFile = dir </> "stats.txt"
        command = proc (programPath cabal)
                  [ "install", "--dry-run", "--max-backjumps", show backjumps
                  , "+RTS", "-t" ++ tempFile, "--machine-readable"]
        config = command {
                        std_out = CreatePipe
                      , std_err = CreatePipe
                      , cwd = Just "benchmarks"
                      }
    (_, Just outh, Just errh, ph) <- createProcess config
    out <- hGetContents outh
    err <- hGetContents errh
    void $ forkIO $ void $ evaluate (length out)
    void $ evaluate (length err)
    code <- waitForProcess ph
    when (code == ExitSuccess) $
      die "Unexpected success."
    unless ("Backjump limit reached" `isInfixOf` err) $
      die $ "cabal should have reached the backjump limit: " ++ err
    stats <- readFile tempFile
    void $ evaluate (length stats)
    case parseAvgBytes stats of
      Nothing -> die $ "Cannot parse output: " ++ stats
      Just n -> do
        putStrLn $ "Average bytes used: " ++ show n
        return n

parseAvgBytes :: String -> Maybe Int
parseAvgBytes output = do
  stats <- parse $ unlines $ tail (lines output)
  maxBytes <- lookup "average_bytes_used" stats
  parse maxBytes

parse :: Read a => String -> Maybe a
parse str = case reads str of
              [] -> Nothing
              (x, _) : _ -> Just x


---------- Copied from IntegrationTests.hs ----------

findCabal :: IO ConfiguredProgram
findCabal = do
  -- Find executables and build directories, etc.
  distPref <- findDistPrefOrDefault NoFlag
  buildDir <- canonicalizePath (distPref </> "build/cabal")
  let programSearchPath = ProgramSearchPathDir buildDir : defaultProgramSearchPath
  (cabal, _) <- requireProgram normal cabalProgram (setProgramSearchPath programSearchPath defaultProgramDb)
  return cabal

-- | Cabal executable
cabalProgram :: Program
cabalProgram = (simpleProgram "cabal") {
    programFindVersion = findProgramVersion "--numeric-version" id
  }
