module Tests 
    (TestResult( Passed, Failed ), TestCase, runAllTests, sampleTestCase)
    where

data TestResult = Passed String | Failed String deriving Show

type TestCase = IO TestResult

--
-- Sample test case that always passes.
--
sampleTestCase :: IO TestResult
sampleTestCase = do return (Passed "sampleTestCase")

--
-- True if the TestResult is Passed, False otherwise
--
testResultToBool :: TestResult -> Bool
testResultToBool (Passed _) = True
testResultToBool (Failed _) = False

--
-- Runs every specified test case, returning True iff all tests pass.
-- Results from the tests are printed.
--
runAllTests :: [TestCase] -> IO Bool
runAllTests [] = do return True
runAllTests (testCase:testCases) = do testResult <- testCase
				      putStrLn (show testResult)
				      testResults <- runAllTests testCases
				      return (testResults && testResultToBool testResult)