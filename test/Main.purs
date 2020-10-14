module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Class.Console (log)
import LambdaCalculus.Parser (Parser, abstraction, application, symbol, term, type', variable)
import Text.Parsing.Parser (runParser)

oofIfFails :: forall a. Parser a -> String -> Effect Unit
oofIfFails parser input =
  case runParser input parser of
    Left err -> do
      log $ "oof on " <> input
      log (show err)
    Right _ -> pure unit

testParser :: Effect Unit
testParser = do
   traverse_ (oofIfFails type') [ "type A", "type ABC" ]
   traverse_ (oofIfFails variable) [ "aab", "asdfb" ]
   traverse_ (oofIfFails symbol) [ "aa", "asdf" ]
   traverse_ (oofIfFails abstraction) [ "\\x. asdf", "\\x y z. z" ]
   traverse_ (oofIfFails application) [ "f a", "x \\y. y", "f x y" ]
   traverse_ (oofIfFails term) [ "(f a)", "(\\x y. z) y", "((x y))" ]
   traverse_ (oofIfFails term) [ "Left a", "Left (f a)", "Left (\\x. y z)" ]
   traverse_ (oofIfFails term) [ "Right a", "First (f a)", "Second (\\x. y z)" ]
   traverse_ (oofIfFails term) [ "Tuple f x", "Tuple (f y) (g x)"]
   traverse_ (oofIfFails term) [ "Either a b c", "Either (\\x. y) (\\y z. h) a" ]

main :: Effect Unit
main = do
  testParser

