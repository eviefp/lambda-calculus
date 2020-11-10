module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Parser as Parser
import Test.Simplifier as Simplifier
import Test.Sequent as Sequent
import Test.Unit.Output.Fancy (runTest)

main :: Effect Unit
main = launchAff_ $ runTest do
  Parser.suite
  Simplifier.suite
  Sequent.suite
