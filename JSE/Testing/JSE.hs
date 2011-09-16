{-# LANGUAGE OverloadedStrings #-}
module JSE.Testing.JSE (specs) where

import Data.Aeson.Types (Value(Null))
import Test.Hspec (Specs, describe, descriptions, it)
import Text.Regex.PCRE.Light (compile, caseless)
import Text.Regex.PCRE.Light.Base (Regex(..))

import JSE

specs :: Specs
specs = descriptions [describe_readFilterSpec,
                      describe_readFilterSpec_StringFilter,
                      describe_readFilterSpec_ValueFilter,
                      describe_readFilterSpec_PatternFilter]

describe_readFilterSpec :: Specs
describe_readFilterSpec = describe "readFilterSpec" [
  it "allows for multi-word keys" (readFilterSpec False spec == StringFilter "foo bar" "baz" False)]
  where spec = "foo bar:baz"

describe_readFilterSpec_StringFilter :: Specs
describe_readFilterSpec_StringFilter = describe "readFilterSpec StringFilter" [
  it "parses" (readFilterSpec False spec == StringFilter "foo" "bar baz" False),
  it "preserves the case sensitivity" (readFilterSpec True spec == StringFilter "foo" "bar baz" True)]
  where spec = "foo:\"bar baz\""

describe_readFilterSpec_ValueFilter :: Specs
describe_readFilterSpec_ValueFilter = describe "readFilterSpec ValueFilter" [
  it "parses, ignoring sensitivity" (readFilterSpec False spec == ValueFilter "foo" Null)]
  where spec = "foo:null"

describe_readFilterSpec_PatternFilter :: Specs
describe_readFilterSpec_PatternFilter = describe "readFilterSpec PatternFilter" [
  it "parses" $ matcher $ readFilterSpec False spec]
  where matcher (PatternFilter "foo" (Regex _ "bar baz")) = True
        matcher _                                         = False
        spec                                              = "foo:/bar baz/"

-- Pipeline stage testing

--describe_splitLines :: Specs
--describe_splitLines = describe "splitLines" [
--  it "chunks when it sees a newline"
--  ]


-- Helpers
--enumBS :: Monad m => ByteString -> Enumerator ByteString m b
--enumBS bs =  undefined
--  where builder = fromByteString bs
