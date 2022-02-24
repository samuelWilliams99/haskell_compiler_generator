module Main (main) where

import CompilerGenerator (runCompilerGenerator)
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    runCompilerGenerator $ head args