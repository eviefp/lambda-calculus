module LambdaCalculus.Syntax where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

newtype Symbol = Symbol String

derive instance genericSymbol :: Generic Symbol _
derive newtype instance showSymbol :: Show Symbol

data Type
  = Type Symbol
  | Function Type Type
  | Sum Type Type
  | Product Type Type

derive instance genericType :: Generic Type _

instance showType :: Show Type where
  show t = genericShow t

data Term
   -- | x
   = Variable Symbol
   -- | \x y z. term
   | Abstraction (NonEmptyArray Symbol) Term
   -- | f x
   | Application Term Term
   -- -- | Left x
   | Left
   -- -- | Right x
   | Right
   -- -- | Either :: (a -> c) -> (b -> c) -> Sum a b -> c
   | Either
   -- -- | Tuple x y
   | Tuple
   -- -- | First t
   | First
   -- -- | Second t
   | Second

derive instance genericTerm :: Generic Term _

instance showTerm :: Show Term where
  show t = genericShow t

data AST
  -- | type A
  = TypeDecl Symbol Type
  -- | f : A + B * C
  -- | f = <term>
  | TermDef Symbol Type Term

derive instance genericAST :: Generic AST _

instance showAST :: Show AST where
  show t = genericShow t
