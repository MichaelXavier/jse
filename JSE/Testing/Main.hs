module JSE.Testing.Main (main) where

import Test.Hspec (hspecX)

import qualified JSE.Testing.JSE as J (specs)

main :: IO ()
main = hspecX J.specs
