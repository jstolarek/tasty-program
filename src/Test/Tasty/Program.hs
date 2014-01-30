{-# LANGUAGE DeriveDataTypeable #-}

module Test.Tasty.Program where

import Control.Monad (unless)
import Data.Typeable (Typeable)
import System.Directory (doesFileExist, getPermissions, executable)
import System.FilePath (pathSeparator)
import System.Exit (ExitCode(..))
import System.IO (hGetContents)
import System.Process (runInteractiveProcess, waitForProcess)
import Test.Tasty.Providers (TestName, TestTree, Result(..), IsTest (..), singleTest)

data TestProgram = TestProgram {
      testProgramName :: String
    , testProgramOpts :: [String]
    , testProgramPath :: Maybe FilePath
--    , testProgramOut  :: Maybe FilePath
    } deriving (Typeable)

testProgram :: TestName
            -> String          -- program to run
            -> [String]        -- options
            -> Maybe FilePath  -- working dir
--            -> Maybe FilePath  -- output
            -> TestTree
testProgram testName program opts workingDir = -- out =
    singleTest testName (TestProgram program opts workingDir ) --out)

instance IsTest TestProgram where
  run _ (TestProgram program opts workingDir) _ = do {
  ; fileExistsFlag <- doesFileExist  program
  ; if not fileExistsFlag
  ; then return $ fileDoesNotExistFailure program
  ; else do {
  ; execAllowedFlag <- execPermission program
  ; if not execAllowedFlag
  ; then return $ noExecPermissionFailure program
  ; else do {
  ; (_, _, _, pid) <- runInteractiveProcess program opts workingDir Nothing
--  ; sout  <- hGetContents out
--  ; serr  <- hGetContents err
  ; ecode <- waitForProcess pid
  ; case ecode of
      ExitSuccess      -> return success
      ExitFailure code -> return $ exitFailure program code
  }}}

  testOptions = return []

execPermission :: FilePath -> IO Bool
execPermission file = do
  getPermissions file >>= (return . executable)

fileDoesNotExistFailure :: String -> Result
fileDoesNotExistFailure file = Result
  { resultSuccessful  = False
  , resultDescription = "Cannot locate program " ++ file
  }

noExecPermissionFailure :: String -> Result
noExecPermissionFailure file = Result
  { resultSuccessful  = False
  , resultDescription = "No permissions to execute " ++ file
  }

success :: Result
success = Result True ""

exitFailure :: String -> Int -> Result
exitFailure file code = Result
  { resultSuccessful  = False
  , resultDescription = "Program " ++ file ++ " failed with code " ++ show code
  }
