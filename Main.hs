{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

-- TODO: turn off cliconfig
import JSE.CLI (annotations, convertConfig, CLIConfig(..)) 
import JSE (pipeline, Config(..))
import Data.Enumerator (run_, printChunks, ($$), (=$))
import Data.Enumerator.Binary (enumHandle)
import System.Console.CmdArgs.Implicit (cmdArgs_)
import System.IO (stdin)

main :: IO ()
main = do
  cliConfig <- cmdArgs_ annotations :: IO CLIConfig
  run_ $ pipeline $ convertConfig cliConfig
