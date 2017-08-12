import HackageBenchmark
import Statistics.Types (mkPValue)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assert, testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "unit tests" [

    testGroup "isSignificant" [

        testCase "detect shift in distribution" $ assert $
            isSignificant (mkPValue 0.05) [1,2..7] [4,5..10]

      , testCase "ignore shift in distribution with high p-value" $ assert $
            isSignificant (mkPValue 0.99) [1,2..7] [4,5..10]

      , testCase "ignore same data" $ assert $
            not $ isSignificant (mkPValue 0.05) [1,2..10] [1,2..10]

      , testCase "ignore outlier" $ assert $
            not $ isSignificant (mkPValue 0.05) [1, 2, 1, 1, 1] [2, 1, 50, 1, 1]
      ]

  , testGroup "combineTrialResults" [

        testCase "convert unexpected difference to Unknown" $
            combineTrialResults [NoInstallPlan, BackjumpLimit] @?= Unknown

      , testCase "take one of repeated errors" $
            combineTrialResults [NoInstallPlan, NoInstallPlan] @?= NoInstallPlan

      , testCase "take one of repeated successes" $
            combineTrialResults [Solution, Solution] @?= Solution

      , testCase "timeout overrides other results" $
            combineTrialResults [Solution, Timeout, Solution] @?= Timeout

      , testCase "convert unexpected difference to Unknown, even with timeout" $
            combineTrialResults [Solution, Timeout, NoInstallPlan] @?= Unknown
    ]

  , testGroup "isInterestingResultPair" [

        testCase "rerun different results" $ assert $
            isInterestingResultPair NoInstallPlan BackjumpLimit

      , testCase "rerun unknown result" $ assert $
            isInterestingResultPair Unknown Unknown

      , testCase "rerun PkgNotFound" $ assert $
            isInterestingResultPair PkgNotFound PkgNotFound

      , testCase "ignore same expected error" $ assert $
            not $ isInterestingResultPair NoInstallPlan NoInstallPlan

      , testCase "ignore success" $ assert $
            not $ isInterestingResultPair Solution Solution
    ]

  , testGroup "shouldSkipAfterTrial1" [

        testCase "rerun when min difference is zero" $ assert $
            not $ shouldSkipAfterTrial1 0 1.0 1.0 Solution Solution

      , testCase "rerun when min difference is zero, even with timeout" $ assert $
            not $ shouldSkipAfterTrial1 0 1.0 1.0 Timeout Timeout

      , testCase "treat timeouts as the same time" $ assert $
            shouldSkipAfterTrial1 0.000001 89.9 92.0 Timeout Timeout

      , testCase "skip when times are too close - 1" $ assert $
                  shouldSkipAfterTrial1 10 1.0 0.91  Solution Solution

      , testCase "skip when times are too close - 2" $ assert $
                  shouldSkipAfterTrial1 10 1.0 1.09  Solution Solution

      , testCase "rerun when times aren't too close - 1" $ assert $
            not $ shouldSkipAfterTrial1 10 1.0 0.905 Solution Solution

      , testCase "rerun when times aren't too close - 2" $ assert $
            not $ shouldSkipAfterTrial1 10 1.0 1.1   Solution Solution
    ]
  ]
