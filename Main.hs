{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import JSE.CLI (annotations, convertConfig) 
import JSE (pipeline, Config(..))
import Data.Enumerator (run_, printChunks, ($$), (=$))
import Data.Enumerator.Binary (enumHandle)
import System.Console.CmdArgs.Implicit (cmdArgs_)
import System.IO (stdin)

main :: IO ()
main = do
  cliConfig <- cmdArgs_ annotations
  run_ $ pipeline $ convertConfig cliConfig
