module Test.Parser
    ( suite
    ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Tuple (Tuple, uncurry)
import Data.Tuple.Nested ((/\))
import LambdaCalculus.Parser (Parser, abstraction', anyKeyword, application', ast, file, function', product', sum', term, termDef, type', typeDecl, typeName, variable)
import LambdaCalculus.Syntax as S
import Test.Unit (Test, TestSuite, test)
import Test.Unit as Assert
import Test.Unit as T
import Test.Unit.Assert as AssertT
import Text.Parsing.Parser (runParser)

positiveParserTest :: forall a. Show a => Parser a -> String -> Test
positiveParserTest p input =
    case runParser input p of
         Left err ->
             Assert.failure
                 $ "Failed for '"
                 <> input
                 <> "' because of '"
                 <> show err
                 <> "'"
         Right _  -> Assert.success

negativeParserTest :: forall a. Show a => Parser a -> String -> Test
negativeParserTest p input =
    case runParser input p of
         Left err -> Assert.success
         Right res  ->
             Assert.failure
                 $ "Parser should fail for '"
                 <> input
                 <> "' but got '"
                 <> show res
                 <> "'"

specificParserTest :: forall a. Show a => Eq a => Parser a -> String -> a -> Test
specificParserTest p input a =
    case runParser input p of
         Left err ->
             Assert.failure
                 $ "Failed for '"
                 <> input
                 <> "' because of '"
                 <> show err
                 <> "'"
         Right r -> AssertT.equal a r

sym :: String -> S.Symbol
sym = S.Symbol

var :: String -> S.Term
var = S.Variable <<< S.Symbol

typ :: String -> S.Type
typ = S.Type <<< S.Symbol

testParser
    :: forall a
     . Show a
    => Eq a
    => { name           :: String
       , parser         :: Parser a
       , positiveInputs :: Array String
       , negativeInputs :: Array String
       , specificData   :: Array (Tuple String a)
       }
    -> TestSuite
testParser { name, parser, positiveInputs, negativeInputs, specificData } =
    test name do
      traverse_ (positiveParserTest parser) positiveInputs
      traverse_ (negativeParserTest parser) negativeInputs
      traverse_ (uncurry $ specificParserTest parser) specificData

suite :: TestSuite
suite = T.suite "parser" do
    termParser
    typeParser
    astParser

termParser :: TestSuite
termParser = T.suite "term parser" do
    testParser
        { name: "variable"
        , parser: variable
        , positiveInputs: [ "a", "b", "abc" ]
        , negativeInputs: [ "A", " ", "", "1", "1abc", "type" ]
        , specificData:
            [ "a1" /\ var "a1"
            , "a'" /\ var "a'"
            , "a'b" /\ var "a'b"
            , "a_" /\ var "a_"
            , "a_c" /\ var "a_c"
            , "α" /\ var "α"
            , "αβγδ" /\ var "αβγδ"
            ]
        }
    testParser
        { name: "anyKeyword"
        , parser: anyKeyword
        , positiveInputs:
            [ "Left", "Right", "Either", "Tuple", "First", "Second" ]
        , negativeInputs:
            [ "Blah", "Whatever", "RIGHT" ]
        , specificData: []
        }
    testParser
        { name: "abstraction"
        , parser: abstraction' term
        , positiveInputs:
            [ "\\x. y", "\\xy. xy" ]
        , negativeInputs:
            [ "\\\\x. y", "\\x", "\\x \\y. y" ]
        , specificData:
            [ "\\a. \\b. \\c. a"
                /\ (S.Abstraction
                       (sym "a")
                       (S.Abstraction
                           (sym "b")
                           (S.Abstraction
                             (sym "c")
                             (var "a")
                           )
                       )

                    )
            ]
        }
    testParser
        { name: "application"
        , parser: application' term
        , positiveInputs:
            [ "x y", "x y z", "(x y) (x z)" ]
        , negativeInputs: []
        , specificData:
            [ "x y z"
                /\
                    S.Application
                        (S.Application (var "x") (var "y"))
                        (var "z")
            ]
        }
    testParser
        { name: "term"
        , parser: term
        , positiveInputs:
            [ "\\x. \\y. Left x"
            , "(\\x. x) (Tuple a (\\x. y))"
            , "Left (f a)"
            ]
        , negativeInputs: []
        , specificData:
            [ "(x y) z"
                /\
                    S.Application
                        (S.Application (var "x") (var "y"))
                        (var "z")
            , "(\\x. y) (w z)"
               /\
                   S.Application
                        (S.Abstraction
                            (sym "x")
                            (var "y")
                        )
                        (S.Application
                            (var "w")
                            (var "z")
                        )
            ]
        }

typeParser :: TestSuite
typeParser = T.suite "type parser" do
    testParser
        { name: "typeName"
        , parser: typeName
        , positiveInputs: [ "A", "B", "ABC", "Abc" ]
        , negativeInputs: [ "a", "aBC", "abc" ]
        , specificData:
            [ "Γ" /\ S.Type (S.Symbol "Γ")
            , "ΔΦΠ" /\ S.Type (S.Symbol "ΔΦΠ")
            , "Ωα" /\ S.Type (S.Symbol "Ωα")
            ]
        }
    testParser
        { name: "function"
        , parser: function' type'
        , positiveInputs: [ "A -> B", "A -> B -> C", "(A -> B) -> C" ]
        , negativeInputs: [ "A ->", "-> B", "->", ". -> Y" ]
        , specificData:
            [ "A -> B -> C"
                /\ S.Function
                    (typ "A")
                    (S.Function (typ "B") (typ "C"))
            ]
        }
    testParser
        { name: "sum"
        , parser: sum' type'
        , positiveInputs: [ "A + B", "A + B + C", "(A + B) + C" ]
        , negativeInputs: [ "A +", "+ B", "+", ". + Y" ]
        , specificData:
            [ "A + B + C"
                /\ S.Sum
                    (typ "A")
                    (S.Sum (typ "B") (typ "C"))
            ]
        }
    testParser
        { name: "product"
        , parser: product' type'
        , positiveInputs: [ "A * B", "A * B * C", "(A * B) * C" ]
        , negativeInputs: [ "A *", "* B", "*", ". * Y" ]
        , specificData:
            [ "A * B * C"
                /\ S.Product
                    (typ "A")
                    (S.Product (typ "B") (typ "C"))
            ]
        }
    testParser
        { name: "type"
        , parser: type'
        , positiveInputs: [ "A + B -> C", "(A -> B) + (B -> C)" ]
        , negativeInputs: [ "-> * +" ]
        , specificData:
            [ "A * B + C"
                /\ S.Sum
                    (S.Product (typ "A") (typ "B"))
                    (typ "C")
            , "A * B -> C"
                /\ S.Function
                    (S.Product (typ "A") (typ "B"))
                    (typ "C")
            , "(A -> B) -> C + D"
                /\ S.Function
                        (S.Function (typ "A") (typ "B"))
                        (S.Sum (typ "C") (typ "D"))
            ]
        }

astParser :: TestSuite
astParser = T.suite "ast parser" do
    testParser
        { name: "typeDecl"
        , parser: typeDecl
        , positiveInputs: [ "type A", "type B", "type Abc" ]
        , negativeInputs: [ "type_ A", "A type B", "type (A + B)" ]
        , specificData: []
        }
    testParser
        { name: "termDef"
        , parser: termDef
        , positiveInputs:
            [ "f : A\nf = \\x. y"
            , "id : A -> A\nid = \\a. a"
            , "either : (A -> C) -> (B -> C) -> (A + B) -> C\neither = \\left. \\right. \\e. Either left right e"
            , "mapRight : (A -> C) -> (A + B) -> (A + C)\n mapRight = \\f. \\e. Either (\\a . a) f e"
            ]
        , negativeInputs:
            [ "f : A"
            , "f = \\a . a"
            , "f : A\ng : B"
            , "f : A\ng = \\x . x"
            ]
        , specificData: []
        }
    testParser
        { name: "ast"
        , parser: ast
        , positiveInputs:
            [ "type A"
            , "type B"
            , "f : A -> B\nf = \\x. y"
            ]
        , negativeInputs:
            [ "Type A"
            , "f : A -> B"
            , "f = \\x. x"
            , "f : A -> B\ng = y"
            ]
        , specificData: []
        }
    testParser
        { name: "file"
        , parser: file
        , positiveInputs:
            [ "type A"
            , "f : A -> B\nf = \\x. y"
            , """

-- comment here

{- hello -}


type A
type B
f : A -> A
f = \x. x
"""
            ]
        , negativeInputs: []
        , specificData:
            [ "type A" /\ [ S.TypeDecl (sym "A") ]
            , "type A\ntype B"
                /\ [ S.TypeDecl (sym "A")
                   , S.TypeDecl (sym "B")
                   ]
            ,
"""type C

type D
"""
                /\ [ S.TypeDecl (sym "C")
                   , S.TypeDecl (sym "D")
                   ]
            ,
"""
-- comment here
{- hello -}
type A
type B
f : A -> A
f = \x. x
"""
            /\
                [ S.TypeDecl (sym "A")
                , S.TypeDecl (sym "B")
                , S.TermDef
                    (sym "f")
                    (S.Function
                        (typ "A")
                        (typ "A"))
                    (S.Abstraction
                        (sym "x")
                        (var "x"))
                ]
            ]
        }
