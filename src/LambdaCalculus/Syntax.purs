module LambdaCalculus.Syntax where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Symbol
     = Symbol String
     | Index Int

derive instance genericSymbol :: Generic Symbol _
derive instance eqSymbol :: Eq Symbol
instance showSymbol :: Show Symbol where
    show t = genericShow t


data Type
  -- | A, B, ..., Abc, ABC
  = Type Symbol
  -- | A -> B, A -> B -> C ~ A -> (B -> C)
  | Function Type Type
  -- | A + B + C ~ A + (B + C)
  | Sum Type Type
  -- | A * B * C ~ A * (B * C)
  | Product Type Type

derive instance eqType :: Eq Type
derive instance genericType :: Generic Type _

instance showType :: Show Type where
  show t = genericShow t

data Term
   -- | x, y, z
   = Variable Symbol
   -- | \x. term, \x. \y. x
   -- | Later, we might add syntactic sugar e.g., \x y. x
   | Abstraction Symbol Term
   -- | f x, x x, (\x. x) y
   | Application Term Term
   -- -- | Left
   | Left
   -- -- | Right
   | Right
   -- -- | Either
   | Either
   -- -- | Tuple
   | Tuple
   -- -- | First
   | First
   -- -- | Second
   | Second

derive instance genericTerm :: Generic Term _
derive instance eqTerm :: Eq Term

instance showTerm :: Show Term where
  show t = genericShow t

data AST
  -- | type A
  -- | type B
  = TypeDecl Symbol
  -- | f : A + B * C
  -- | f = <term>
  | TermDef Symbol Type Term

derive instance eqAST :: Eq AST
derive instance genericAST :: Generic AST _

instance showAST :: Show AST where
  show t = genericShow t
