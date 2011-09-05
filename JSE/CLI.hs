{-# LANGUAGE DeriveDataTypeable #-}
--module JSE.CLI (annotations, convertConfig, CLIConfig) where
module JSE.CLI where

import JSE (Config(..), readFilterSpec)

import Data.Data (Data)
import Data.Enumerator.Binary (enumFile, enumHandle)
import Data.Text (pack)
import Data.Typeable (Typeable)
import System.Console.CmdArgs.Implicit ((&=),
                                        Annotate((:=)),
                                        Ann,
                                        (+=),
                                        def,
                                        help,
                                        summary,
                                        record,
                                        program,
                                        explicit,
                                        name,
                                        args,
                                        typ,
                                        typFile)
import System.IO (stdin)

data CLIConfig = CLIConfig { rFields     :: [String],
                             inFile      :: Maybe FilePath,
                             ignoreC     :: Bool,
                             filterSpecs :: [String]} deriving (Show, Data, Typeable)

annotations :: Annotate Ann
annotations = record CLIConfig {rFields = def, ignoreC = False, inFile = Nothing, filterSpecs = []} 
                     [ rFields    := def     += help "Fields to return. Returns all if not specified." += explicit += typ "FIELD" += name "f",
                      ignoreC     := False   += help "Case sensitive (defaults insensitive)" += explicit += name "c",
                      inFile      := Nothing += typFile += explicit += name "i" += help "Filename (stdin otherwise)",
                      filterSpecs := def     += args]
                      += program "jse"
                      += summary "JSON Stream Editor"

convertConfig :: CLIConfig -> Config
convertConfig clc = Config { returnFields = map pack $ rFields clc,
                             source       = genSource $ inFile clc,
                             filters      = map (readFilterSpec ignore . pack) $ filterSpecs clc }
  where ignore              = ignoreC clc
        genSource Nothing   = enumHandle bufferSize stdin
        genSource (Just fp) = enumFile fp

bufferSize :: Integer
bufferSize = 8192
