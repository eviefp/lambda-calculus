module LambdaCalculus.Pretty where

import Prelude

import Dodo as Dodo
import LambdaCalculus.Syntax as LC

print :: forall ann. Dodo.Doc ann -> String
print = Dodo.print Dodo.plainText Dodo.twoSpaces

par :: forall ann. Dodo.Doc ann -> Dodo.Doc ann
par = Dodo.enclose (Dodo.text "(") (Dodo.text ")")

printSymbol :: forall ann. LC.Symbol -> Dodo.Doc ann
printSymbol = case _ of
    LC.Symbol str -> Dodo.text str
    LC.Index  n   -> Dodo.text $ show n

printType :: forall ann. LC.Type -> Dodo.Doc ann
printType = case _ of
    LC.Type sym ->
        printSymbol sym
    LC.Function t1 t2     ->
        par $ printType t1 <> Dodo.text " -> " <> printType t2
    LC.Sum t1 t2     ->
        par $ printType t1 <> Dodo.text " + " <> printType t2
    LC.Product t1 t2     ->
        par $ printType t1 <> Dodo.text " * " <> printType t2

printTerm :: forall ann. LC.Term -> Dodo.Doc ann
printTerm = case _ of
    LC.Variable sym ->
        printSymbol sym
    LC.Abstraction sym t ->
        par
            $ Dodo.text "\\"
            <> printSymbol sym
            <> Dodo.text ". "
            <> printTerm t
    LC.Application t1 t2 ->
        par $ printTerm t1 <> Dodo.space <> printTerm t2
    LC.Left ->
        Dodo.text "Left"
    LC.Right ->
        Dodo.text "Right"
    LC.Either ->
        Dodo.text "Either"
    LC.Tuple ->
        Dodo.text "Tuple"
    LC.First ->
        Dodo.text "First"
    LC.Second ->
        Dodo.text "Second"

