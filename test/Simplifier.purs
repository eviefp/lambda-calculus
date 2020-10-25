module Test.Simplifier
    ( suite
    ) where

import Prelude

import Data.Either (Either(..))
import LambdaCalculus.Parser (term)
import LambdaCalculus.Simplifier (fromLocallyNamed, simplify, toLocallyNamed)
import LambdaCalculus.Syntax as S
import Test.Unit (Test, TestSuite, test)
import Test.Unit as Assert
import Test.Unit as T
import Test.Unit.Assert as AssertT
import Text.Parsing.Parser (runParser)

sym :: String -> S.Symbol
sym = S.Symbol

idx :: Int -> S.Term
idx = S.Variable <<< S.Index

var :: String -> S.Term
var = S.Variable <<< S.Symbol

abs :: String -> S.Term -> S.Term
abs s t = S.Abstraction (sym s) t

app :: S.Term -> S.Term -> S.Term
app = S.Application

suite :: TestSuite
suite = T.suite "simplifier" do
    simplifierTests
    toLocallyNamedTests

simplifyNothing :: String -> Test
simplifyNothing input =
    case runParser input term of
       Left err -> Assert.failure $ "Parsing error: " <> show err
       Right t -> AssertT.shouldEqual t (simplify t)

simplifiesTo :: String -> String -> Test
simplifiesTo input result =
    case runParser input term, runParser result term of
       Right t, Right e ->
           AssertT.shouldEqual (simplify t) e
       _, _ -> Assert.failure "baaaaaaad"

locallyRenamesTo :: String -> S.Term -> Test
locallyRenamesTo input exp =
    case runParser input term of
       Right t -> AssertT.shouldEqual (toLocallyNamed t) exp
       _       -> Assert.failure "baaaaaaad"

fromLocallyTo :: S.Term -> String -> Test
fromLocallyTo exp input =
    case runParser input term of
       Right t -> AssertT.shouldEqual t (fromLocallyNamed exp)
       _       -> Assert.failure "baaaaaaad"

simplifierTests :: TestSuite
simplifierTests = T.suite "1" do
   test "variables" do
      simplifyNothing "x"
   test "abstraction" do
      simplifyNothing "\\x. x"
      simplifyNothing "\\y. \\x. x"
   test "application" do
      simplifiesTo "(\\x. x) y" "y"
      simplifiesTo "(\\x. \\y. x) a" "\\y. a"
      simplifiesTo "x ((\\x. x) z)" "x z"
      simplifiesTo "(\\x. x) (\\x. x)" "\\x. x"
      simplifiesTo "((\\x. \\y. x) y) x" "y"
   test "correctly rename bindings" do
      simplifiesTo "(\\x. \\x. x) y" "\\x. x"
   test "does not bind free variables" do
      simplifiesTo "(\\x. \\y. x) y" "\\y0. y"
   test "tuples" do
      simplifiesTo "First (Tuple a b)" "a"
      simplifiesTo "(\\x. First (Tuple a x)) b" "a"
      simplifiesTo "Second (Tuple a b)" "b"
      simplifiesTo "(\\x. Second (Tuple a x)) b" "b"
      simplifiesTo "Second (Tuple a b c)" "Second (Tuple a b c)"
   test "eithers" do
      simplifiesTo "Either (\\x. y) g (Left a)" "y"
      simplifiesTo "Either (\\x. y) g (Right b)" "g b"
   test "ordering" do
      simplifiesTo "First ((\\x. Tuple a x) b)" "a"
      simplifiesTo "Either ((\\x. \\y. x) a) g (Left y)" "a"
   test "failingTest" do
      simplifiesTo "((\\x. Either) b) f g (Right a)" "g a"
      simplifiesTo "((\\x. Either) b) ((\\y. f) x) g (Right a)" "g a"


toLocallyNamedTests :: TestSuite
toLocallyNamedTests = T.suite "toLocallyNamed" do
    test "toLocallyNamed" do
        locallyRenamesTo "\\x. x" (abs "x" (idx 0))
        locallyRenamesTo "\\x. y" (abs "x" (var "y"))
    test "fromLocallyNamed" do
        fromLocallyTo (abs "x" (idx 0)) "\\x. x"
