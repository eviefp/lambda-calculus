module Test.CurryHoward
    ( suite
    ) where

import CurryHoward

import Data.Either (Either(..))
import Data.Maybe (Maybe(Just), maybe)
import Data.Traversable (traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import LambdaCalculus.Parser as LParser
import LambdaCalculus.Pretty as LP
import LambdaCalculus.Syntax ((:->))
import LambdaCalculus.Syntax as LC
import Prelude (show, ($), (<<<), (<>), (==))
import Test.Unit (Test, TestSuite, test)
import Test.Unit as T
import Test.Unit.Assert as AssertT
import Text.Parsing.Parser (runParser)

suite :: TestSuite
suite = T.suite "simple" do
    simpleTests

simpleTests :: TestSuite
simpleTests = T.suite "simpleTests" do
   traverse_ go
        [ a :-> a /\ "\\a0. a0"
        , a :-> a :-> a /\ "\\a0. \\a0. a0"
        , a :-> b :-> a /\ "\\a0. \\b0. a0"
        , b :-> a :-> a /\ "\\b0. \\a0. a0"
        ]
  where
    go :: (LC.Type /\ String) -> TestSuite
    go (ty /\ input) =
        test
            (input <> ": " <> (LP.print $ LP.printType ty))
            $ case runParser input LParser.term of
                Left err ->
                    T.failure $ "Parse error: " <> show err
                Right term ->
                    assertTypeFindsTerm ty term (findTerm ty)

a :: LC.Type
a = LC.Type (LC.Symbol "A")

b :: LC.Type
b = LC.Type (LC.Symbol "B")

assertTypeFindsTerm :: LC.Type -> LC.Term -> Maybe LC.Term -> Test
assertTypeFindsTerm ty term result =
    AssertT.assert
        (maybe "No result found" (LP.print <<< LP.printTerm) result)
        (result == Just term)

