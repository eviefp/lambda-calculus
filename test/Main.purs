module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Unit.Output.Fancy (runTest)
import Test.Parser as Parser

main :: Effect Unit
main = launchAff_ $ runTest do
  Parser.suite
