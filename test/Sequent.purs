module Test.Sequent
    ( suite
    ) where

import SequentCalculus.Sequent

import Data.Traversable (traverse_)
import Prelude (discard, ($))
import Test.Unit (Test, TestSuite, test)
import Test.Unit as T
import Test.Unit.Assert as AssertT

suite :: TestSuite
suite = T.suite "sequent" do
    tautologyTests
    notTautologies

tautologyTests :: TestSuite
tautologyTests = T.suite "tautologies" do
   traverse_ go
        [ a :->: a
        , a :->: a :->: a
        , a :->: b :->: a
        , a :\/: b :->: b :\/: a
        , a :->: a :\/: b
        , a :/\: b :->: b :/\: a
        , (a :\/: b :->: c) :->: ((a :->: c) :/\: (b :->: c))
        , (a :->: a :->: b) :->: (a :->: b)
        , (c :->: a) :->: (c :->: b) :->: c :->: (a :/\: b)
        , (a :->: c) :->: (b :->: c) :->: (a :\/: b) :->: c
        , (a :->: b) :->: (a :/\: c) :->: (b :/\: c)
        , (a :->: b) :->: (a :\/: c) :->: (b :\/: c)
        , (b :->: c) :->: (a :->: b) :->: (a :->: c)
        , (a :->: a :->: b) :->: (c :->: a) :->: (c :->: c :->: b)
        ]
  where
    go :: Formula String -> TestSuite
    go f =
        test (print $ printFormula f)
            $ assertTautology f

notTautologies :: TestSuite
notTautologies = T.suite "negative tests" do
   traverse_ go
        [ a
        , a :->: b
        , a :->: a :->: b
        , (a :->: b) :->: (b :/\: c) :->: (a :/\: c)
        ]
  where
    go :: Formula String -> TestSuite
    go f =
        test (print $ printFormula f)
            $ assertFails f

var :: String -> Formula String
var = Variable

a :: Formula String
a = var "A"

b :: Formula String
b = var "B"

c :: Formula String
c = var "C"

assertTautology :: Formula String -> Test
assertTautology input =
    AssertT.assert (print $ printFormula input) (tautology input)

assertFails :: Formula String -> Test
assertFails input =
    AssertT.assertFalse (print $ printFormula input) (tautology input)

