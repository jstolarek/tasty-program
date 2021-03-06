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
 , CatchStderr
 ) where

import Control.DeepSeq      ( deepseq                                  )
import Data.Typeable        ( Typeable                                 )
import Data.Proxy           ( Proxy (..)                               )
import System.Directory     ( findExecutable                           )
import System.Exit          ( ExitCode(..)                             )
import System.Process       ( runInteractiveProcess, waitForProcess    )
import System.IO            ( hGetContents                             )
import Test.Tasty.Providers ( IsTest (..), Result, TestName, TestTree,
                              singleTest, testPassed, testFailed       )
import Test.Tasty.Options   ( IsOption (..), OptionDescription(..),
                              safeRead, lookupOption, flagCLParser     )

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
  run opts (TestProgram program args workingDir) _ = do
    execFound <- findExecutable program

    let CatchStderr catchStderr = lookupOption opts

    case execFound of
      Nothing       -> return $ execNotFoundFailure program
      Just progPath -> runProgram progPath args workingDir catchStderr

  testOptions = return [Option (Proxy :: Proxy CatchStderr)]

newtype CatchStderr = CatchStderr Bool deriving (Show, Typeable)

instance IsOption CatchStderr where
  defaultValue = CatchStderr False
  parseValue   = fmap CatchStderr . safeRead
  optionName   = return "catch-stderr"
  optionHelp   = return "Catch standart error of programs"
  optionCLParser = flagCLParser (Just 'e') (CatchStderr True)

-- | Run a program with given options and optional working directory.
-- Return success if program exits with success code.
runProgram :: String          -- ^ Program name
           -> [String]        -- ^ Program options
           -> Maybe FilePath  -- ^ Optional working directory
           -> Bool            -- ^ Whether to print stderr on error
           -> IO Result
runProgram program args workingDir catchStderr = do
  (_, _, stderrH, pid) <- runInteractiveProcess program args workingDir Nothing

  stderr <- if catchStderr then fmap Just (hGetContents stderrH) else return Nothing
  ecode  <- stderr `deepseq` waitForProcess pid

  case ecode of
    ExitSuccess      -> return success
    ExitFailure code -> return $ exitFailure program code stderr

-- | Indicates successful test
success :: Result
success = testPassed ""

-- | Indicates that program does not exist in the path
execNotFoundFailure :: String -> Result
execNotFoundFailure file =
  testFailed $ "Cannot locate program " ++ file ++ " in the PATH"

-- | Indicates that program failed with an error code
exitFailure :: String -> Int -> Maybe String -> Result
exitFailure file code stderr =
  testFailed $ "Program " ++ file ++ " failed with code " ++ show code
               ++ case stderr of
                    Nothing -> ""
                    Just s  -> "\n Stderr was: \n" ++ s

