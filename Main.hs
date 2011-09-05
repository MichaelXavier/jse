{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import JSE.CLI (annotations, convertConfig) 
import JSE (pipeline)
import Data.Enumerator (run_)
import System.Console.CmdArgs.Implicit (cmdArgs_)

main :: IO ()
main = do
  cliConfig <- cmdArgs_ annotations
  run_ $ pipeline $ convertConfig cliConfig
