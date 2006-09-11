--------------------------------------------------------------------------
--  roguestar-engine: the space-adventure roleplaying game backend.       
--  Copyright (C) 2006 Christopher Lane Hinson <lane@downstairspeople.org>  
--                                                                        
--  This program is free software; you can redistribute it and/or modify  
--  it under the terms of the GNU General Public License as published by  
--  the Free Software Foundation; either version 2 of the License, or     
--  (at your option) any later version.                                   
--                                                                        
--  This program is distributed in the hope that it will be useful,       
--  but WITHOUT ANY WARRANTY; without even the implied warranty of        
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         
--  GNU General Public License for more details.                          
--                                                                        
--  You should have received a copy of the GNU General Public License along  
--  with this program; if not, write to the Free Software Foundation, Inc.,  
--  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.           
--                                                                        
--------------------------------------------------------------------------

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