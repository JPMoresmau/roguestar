
module Tests 
    (TestResult(..), 
     TestCase, 
     test, 
     runAllTests, 
     sampleTestCase)
    where

data TestResult = Passed String | Failed String deriving Show

type TestCase = IO TestResult

-- |
-- Sample test case that always passes.
--
sampleTestCase :: IO TestResult
sampleTestCase = do return (Passed "sampleTestCase")

-- |
-- True if the TestResult is Passed, False otherwise
--
testResultToBool :: TestResult -> Bool
testResultToBool (Passed _) = True
testResultToBool (Failed _) = False

-- |
-- Simple way to generate a TestResult based on a boolean test result.
--
test :: String -> Bool -> TestCase
test str True = return $ Passed str
test str False = return $ Failed str

-- |
-- Runs every specified test case, returning True iff all tests pass.
-- Results from the tests are printed.
--
runAllTests :: [TestCase] -> IO Bool
runAllTests [] = do return True
runAllTests (testCase:testCases) = do testResult <- testCase
				      putStrLn (show testResult)
				      testResults <- runAllTests testCases
				      return (testResults && testResultToBool testResult)
