{-# LANGUAGE OverloadedStrings #-}
module JSE.Testing.JSE (specs) where

import Data.Aeson.Types (Value(Null))
import Test.Hspec (Specs, describe, descriptions, it)
import Text.Regex.PCRE.Light (compile, caseless)

import JSE

specs :: Specs
specs = descriptions [describe_readFilterSpec,
                      describe_readFilterSpec_StringFilter,
                      describe_readFilterSpec_ValueFilter,
                      describe_readFilterSpec_PatternFilter]

describe_readFilterSpec :: Specs
describe_readFilterSpec = describe "readFilterSpec" [
  it "allows for multi-word keys" (readFilterSpec False spec == StringFilter "foo bar" "baz" False)]
  where spec = "\"foo bar\":baz"

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
describe_readFilterSpec_PatternFilter = describe "readFilterSpec ValueFilter" [
  it "parses" (readFilterSpec False spec == PatternFilter "foo" regInsensitive),
  it "compiles with the case sensitivity" (readFilterSpec False spec == PatternFilter "foo" regSensitive)]
  where spec           = "foo:/bar baz/"
        regSensitive   = compile "bar baz" []
        regInsensitive = compile "bar baz" [caseless]
