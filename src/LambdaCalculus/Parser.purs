module LambdaCalculus.Parser where

import Prelude

import Control.Alternative ((<|>))
import Control.Lazy (fix)
import Control.MonadPlus (guard)
import Data.Array (many)
import Data.Char.Unicode as CU
import Data.Foldable (foldl)
import Data.Identity (Identity)
import Data.Maybe (maybe)
import Data.String.CodeUnits as S
import LambdaCalculus.Syntax (AST(..), Symbol(..), Term(..), Type(..))
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.Language as Lang
import Text.Parsing.Parser.String (oneOf)
import Text.Parsing.Parser.Token as Token

type Parser = ParserT String Identity

-------------------------------------------------------------------------------
-- Symbol

symbol :: Parser Symbol
symbol = do
   name <- parser.identifier
   guard $ maybe false CU.isAsciiLower $ S.charAt 0 name
   pure $ Symbol name

typeSymbol :: Parser Symbol
typeSymbol = do
   name <- parser.identifier
   guard $ maybe false CU.isAsciiUpper $ S.charAt 0 name
   pure $ Symbol name

-------------------------------------------------------------------------------
-- Term

variable :: Parser Term
variable = do
   name <- parser.identifier
   guard $ maybe false CU.isAsciiLower $ S.charAt 0 name
   pure $ Variable (Symbol name)

keyword :: String -> Term -> Parser Term
keyword name ctor =
   parser.reserved name $> ctor

anyKeyword :: Parser Term
anyKeyword =
    keyword "Left" Left
        <|> keyword "Right" Right
        <|> keyword "Either" Either
        <|> keyword "Tuple" Tuple
        <|> keyword "First" First
        <|> keyword "Second" Second

abstraction' :: Parser Term -> Parser Term
abstraction' termParser = do
  parser.reservedOp "\\"
  identifier <- symbol
  parser.reservedOp "."
  t <- termParser
  pure $ Abstraction identifier t

application' :: Parser Term -> Parser Term
application' termParser = do
    first <- atom termParser
    atoms <- many (atom termParser)
    pure $ foldl Application first atoms
  where
    atom tp =
           abstraction' tp
               <|> parser.parens termParser
               <|> variable
               <|> anyKeyword

term :: Parser Term
term = fix
    \t ->
        try (application' t)
            <|> try (parser.parens t)
            <|> try (abstraction' t)
            <|> try anyKeyword
            <|> variable

-------------------------------------------------------------------------------
-- Type

typeName :: Parser Type
typeName = Type <$> typeSymbol

function':: Parser Type -> Parser Type
function' typeParser = do
    input <- atom typeParser
    parser.reservedOp "->"
    rest <- typeParser
    pure $ Function input rest
  where
    atom tp = typeName <|> parser.parens tp

sum' :: Parser Type -> Parser Type
sum' typeParser = do
    input <- atom typeParser
    parser.reservedOp "+"
    rest <- typeParser
    pure $ Sum input rest
  where
    atom tp = typeName <|> parser.parens tp

product' :: Parser Type -> Parser Type
product' typeParser = do
    input <- atom typeParser
    parser.reservedOp "*"
    rest <- typeParser
    pure $ Product input rest
  where
    atom tp = typeName <|> parser.parens tp

parseTypeExpr :: Parser Type
parseTypeExpr = fix
    \t ->
        buildExprParser
            [ [ Infix (parser.reservedOp "*" $> Product) AssocRight ]
            , [ Infix (parser.reservedOp "+" $> Sum) AssocRight ]
            , [ Infix (parser.reservedOp "->" $> Function) AssocRight ]
            ] (parser.parens t <|> typeName)

type' :: Parser Type
type' = parseTypeExpr

-------------------------------------------------------------------------------
-- AST

typeDecl :: Parser AST
typeDecl = do
    parser.reserved "type"
    TypeDecl <$> typeSymbol

termDef :: Parser AST
termDef = do
    name <- symbol
    parser.reservedOp ":"
    t <- type'
    name' <- symbol
    guard $ name == name'
    parser.reservedOp "="
    impl <- term
    pure $ TermDef name t impl

ast :: Parser AST
ast = fix
    \t ->
       typeDecl
           <|> termDef
           <|> parser.parens t

file :: Parser (Array AST)
file = do
    parser.whiteSpace
    many ast

-------------------------------------------------------------------------------
-- Internal

parser :: Token.GenTokenParser String Identity
parser = Token.makeTokenParser lcDef

lcDef :: Token.LanguageDef
lcDef = Token.LanguageDef (Token.unGenLanguageDef Lang.emptyDef)
  { commentStart    = "{-"
  , commentEnd      = "-}"
  , commentLine     = "--"
  , nestedComments  = true
  , identStart      = Token.letter
  , identLetter     = Token.alphaNum <|> oneOf ['_', '\'']
  , opStart         = op'
  , opLetter        = op'
  , reservedOpNames = ["->", "\\", ".", "+", "*", ":", "="]
  , reservedNames   = ["type", "Left", "Right", "Either", "First", "Second", "Tuple"]
  , caseSensitive   = true
  }
  where
    op' :: forall m . (Monad m) => ParserT String m Char
    op' = oneOf [':', '\\', '.', '+', '*', '-', '>', '=']
