module LambdaCalculus.Parser where

import Prelude

import Control.Alternative ((<|>))
import Control.Lazy (fix)
import Control.MonadPlus (guard)
import Data.Array (many)
import Data.Array.NonEmpty as NE
import Data.Char.Unicode as CU
import Data.Foldable (foldl)
import Data.Identity (Identity)
import Data.Maybe (maybe)
import Data.String.CodeUnits as S
import LambdaCalculus.Syntax (Symbol(..), Term(..), Type(..))
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.Language as Lang
import Text.Parsing.Parser.String (oneOf)
import Text.Parsing.Parser.Token as Token

type Parser = ParserT String Identity

symbol :: Parser Symbol
symbol = do
   name <- parser.identifier
   guard $ maybe false CU.isAsciiLower $ S.charAt 0 name
   pure $ Symbol name

type' :: Parser Type
type' = do
   parser.reserved "type"
   name <- parser.identifier
   guard $ maybe false CU.isAsciiUpper $ S.charAt 0 name
   pure $ Type (Symbol name)

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
  identifiers <- NE.some symbol
  parser.reservedOp "."
  t <- termParser
  pure $ Abstraction identifiers t

abstraction :: Parser Term
abstraction = abstraction' term

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

application :: Parser Term
application = application' term

term :: Parser Term
term = fix
    \t ->
        application' t
            <|> parser.parens t
            <|> abstraction' t
            <|> variable
            <|> anyKeyword

-- TODO:
-- type parser
-- ast parser

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
  , reservedOpNames = ["->", "\\", ".", "+", "*", ":"]
  , reservedNames   = ["type", "Left", "Right", "Either", "First", "Second", "Tuple"]
  , caseSensitive   = true
  }
  where
    op' :: forall m . (Monad m) => ParserT String m Char
    op' = oneOf [':', '\\', '.', '+', '*', '-', '>']
