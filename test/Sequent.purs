module Test.Sequent
    ( suite
    ) where

import Prelude
import SequentCalculus.Sequent

import Test.Unit (Test, TestSuite, test)
import Test.Unit as Assert
import Test.Unit as T
import Test.Unit.Assert as AssertT
import Text.Parsing.Parser (runParser)

var :: String -> Formula String
var = Variable

-- TODO: Add negative tests too!
suite :: TestSuite
suite = T.suite "sequent" do
    tautologyTests

assertTautology :: Formula String -> Test
assertTautology input =
    AssertT.assert (print $ printFormula input) (tautology input)

-- TODO: Add more positive tests
tautologyTests :: TestSuite
tautologyTests = T.suite "trivial" do
   test "variables" do
      assertTautology $ var "P" :->: var "P"
      assertTautology $ var "P" :->: var "P" :->: var "P"
      assertTautology $ var "A" :->: var "B" :->: var "A"
      assertTautology $ var "A" :\/: var "B" :->: var "B" :\/: var "A"
      assertTautology $ var "A" :->: var "A" :\/: var "B"
      assertTautology $ var "A" :/\: var "B" :->: var "B" :/\: var "A"
      assertTautology $ (var "A" :\/: var "B" :->: var "C") :->: ((var "A" :->: var "C") :/\: (var "B" :->: var "C"))
      assertTautology $ (var "A" :->: var "A" :->: var "B") :->: (var "A" :->: var "B")
      assertTautology $ (var "C" :->: var "A") :->: (var "C" :->: var "B") :->: var "C" :->: (var "A" :/\: var "B")
      assertTautology $ (var "A" :->: var "C") :->: (var "B" :->: var "C") :->: (var "A" :\/: var "B") :->: var "C"

