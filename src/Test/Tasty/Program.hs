{-# LANGUAGE DeriveDataTypeable #-}

-- | This module provides a function that tests whether a program can
-- be run successfully. For example if you have 'foo.hs' source file:
--
-- > module Foo where
-- >
-- > foo :: Int
-- > foo = 5
--
-- you can test whether GHC can compile it:
--
-- > module Main (
-- >   main
-- >  ) where
-- >
-- > import Test.Tasty
-- > import Test.Tasty.Program
-- >
-- > main :: IO ()
-- > main = defaultMain $ testGroup "Compilation with GHC" $ [
-- >     testProgram "Foo" "ghc" ["-fforce-recomp", "foo.hs"] Nothing
-- >   ]
--
-- Program's output and error streams are ignored.

module Test.Tasty.Program (
   testProgram
 ) where

import Data.Typeable        ( Typeable                                 )
import System.Directory     ( findExecutable                           )
import System.Exit          ( ExitCode(..)                             )
import System.Process       ( runInteractiveProcess, waitForProcess    )
import Test.Tasty.Providers ( IsTest (..), Result, TestName, TestTree,
                              singleTest, testPassed, testFailed       )

data TestProgram = TestProgram String [String] (Maybe FilePath)
     deriving (Typeable)

-- | Create test that runs a program with given options. Test succeeds
-- if program terminates successfully.
testProgram :: TestName        -- ^ Test name
            -> String          -- ^ Program name
            -> [String]        -- ^ Program options
            -> Maybe FilePath  -- ^ Optional working directory
            -> TestTree
testProgram testName program opts workingDir =
    singleTest testName (TestProgram program opts workingDir)

instance IsTest TestProgram where
  run _ (TestProgram program opts workingDir) _ = do
    execFound <- findExecutable program
    case execFound of
      Nothing       -> return $ execNotFoundFailure program
      Just progPath -> runProgram progPath opts workingDir

  testOptions = return []

-- | Run a program with given options and optional working directory.
-- Return success if program exits with success code.
runProgram :: String          -- ^ Program name
           -> [String]        -- ^ Program options
           -> Maybe FilePath  -- ^ Optional working directory
           -> IO Result
runProgram program opts workingDir = do
  (_, _, _, pid) <- runInteractiveProcess program opts workingDir Nothing
  ecode <- waitForProcess pid
  case ecode of
    ExitSuccess      -> return success
    ExitFailure code -> return $ exitFailure program code

-- | Indicates successful test
success :: Result
success = testPassed ""

-- | Indicates that program does not exist in the path
execNotFoundFailure :: String -> Result
execNotFoundFailure file =
  testFailed $ "Cannot locate program " ++ file ++ " in the PATH"

-- | Indicates that program failed with an error code
exitFailure :: String -> Int -> Result
exitFailure file code =
  testFailed $ "Program " ++ file ++ " failed with code " ++ show code
