module SequentCalculus.Sequent where

import Prelude

import Data.Array as Array
import Data.Foldable (any, elem, findMap, foldMap, foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Dodo as Dodo

data Formula a
    = Variable a -- "A" or "B"
    | Implication (Formula a) (Formula a) -- Implication (Variable "A") (Variable "B") == A -> B
    | And (Formula a) (Formula a) -- A /\ B
    | Or (Formula a) (Formula a) -- A \/ B

derive instance eqFormula :: Eq a => Eq (Formula a)
derive instance ordFormula :: Ord a => Ord (Formula a)
derive instance genericFormula :: Generic (Formula a) _

instance showFormula :: Show a => Show (Formula a) where
    show f = genericShow f

printFormula :: forall ann. Formula String -> Dodo.Doc ann
printFormula = case _ of
    Variable a -> Dodo.text a
    a :/\: b   -> par $ printFormula a <> Dodo.text " /\\ " <> printFormula b
    a :\/: b   -> par $ printFormula a <> Dodo.text " \\/ " <> printFormula b
    a :->: b   -> par $ printFormula a <> Dodo.text " -> " <> printFormula b

par :: forall ann. Dodo.Doc ann -> Dodo.Doc ann
par = Dodo.enclose (Dodo.text "(") (Dodo.text ")")

printSequent :: forall ann. Sequent String -> Dodo.Doc ann
printSequent (hyps :=>: con) =
    Dodo.foldWithSeparator
        (Dodo.text ",")
        (printFormula <$> hyps)
    <> Dodo.text " => "
    <> printFormula con

printDerivation :: forall ann. Derivation (Sequent String) -> Dodo.Doc ann
printDerivation = go ""
  where
    go :: String -> Derivation (Sequent String) -> Dodo.Doc ann
    go branch = case _ of
        Leaf a -> printSequent a
        Next rule a next ->
            printDerivation next
                <> Dodo.break
                <> Dodo.text "-------------------------------- " <> par (Dodo.text (show rule))
                <> Dodo.break
                <> printSequent a
        Branch rule a left right ->
            Dodo.text (branch <> "L")
                <> Dodo.break
                <> go (branch <> "L") left
                <> Dodo.break
                <> Dodo.break
                <> Dodo.break
                <> Dodo.text (branch <> "R")
                <> Dodo.break
                <> go (branch <> "R") right
                <> Dodo.break
                <> Dodo.break
                <> Dodo.break
                <> Dodo.text (branch <> "L      " <> branch <> "R")
                <> Dodo.break
                <> Dodo.text "-------------------------------- "
                <> par (Dodo.text (show rule))
                <> Dodo.break
                <> printSequent a

print :: forall ann. Dodo.Doc ann -> String
print = Dodo.print Dodo.plainText Dodo.twoSpaces

-- TODO: fold does not quite do the right thing; it's hard to tell list items apart.
log :: Formula String -> String
log =
    foldMap (\s -> "\n\n ================================ \n\n" <> s)
        <<< map (print <<< printDerivation)
        <<< buildDerivation
        <<< formulaToSequent

infixr 6 Implication as :->:
infixr 7 And as :/\:
infixr 7 Or as :\/:

data Sequent a = Sequent (Array (Formula a)) (Formula a) -- hyps :=>: con

derive instance genericSequent :: Generic (Sequent a) _

instance showSequent :: Show a => Show (Sequent a) where
    show f = genericShow f

infixr 5 Sequent as :=>:

formulaToSequent :: forall a. Formula a -> Sequent a
formulaToSequent f = [] :=>: f

sequentToFormula :: forall a. Sequent a -> Formula a
sequentToFormula (xs :=>: con) = case Array.uncons xs of
    Just { head, tail } -> foldl (:/\:) head tail :->: con
    Nothing -> con

data Rule
    = RAnd
    | LAnd
    | ROr1
    | ROr2
    | LOr
    | RImp
    | L0Imp
    | LOrImp
    | LAndImp
    | LImpImp

derive instance genericRule :: Generic Rule _

instance showRule :: Show Rule where
    show f = genericShow f

data Derivation a
    = Leaf a
    | Next Rule a (Derivation a)
    | Branch Rule a (Derivation a) (Derivation a)

derive instance genericDerivation :: Generic (Derivation a) _

instance showDerivation :: Show a => Show (Derivation a) where
    show f = genericShow f

buildDerivation :: forall a. Eq a => Sequent a -> List (Derivation (Sequent a))
buildDerivation = case _ of
    seq@(hyps :=>: a :->: b) ->
        Next RImp seq <$> buildDerivation ((Array.cons a hyps) :=>: b)
    seq@(hyps :=>: a :/\: b) ->
        Branch RAnd seq
            <$> buildDerivation (hyps :=>: a)
            <*> buildDerivation (hyps :=>: b)
    seq@(hyps :=>: con)
      | Just res@(a :/\: b) <- findMap matchAnd hyps ->
          Next LAnd seq
              <$> buildDerivation (((Array.delete res hyps) <> [a, b]) :=>: con)
      | Just res@(a :\/: b) <- findMap matchOr hyps ->
        Branch LOr seq
            <$> buildDerivation (((Array.delete res hyps) <> [a]) :=>: con)
            <*> buildDerivation (((Array.delete res hyps) <> [b]) :=>: con)
      | Just res@((a :\/: b) :->: c) <- findMap matchOrImp hyps ->
        Next LOrImp seq
            <$> buildDerivation (((Array.delete res hyps) <> [a :->: c, b :->: c]) :=>: con)
      | Just res@((a :/\: b) :->: c) <- findMap matchAndImp hyps ->
        Next LAndImp seq
            <$> buildDerivation (((Array.delete res hyps) <> [a :->: b :->: c]) :=>: con)
      | Just res@((a :->: b) :->: c) <- findMap matchImpImp hyps ->
        Branch LImpImp seq
            <$> buildDerivation (((Array.delete res hyps) <> [b :->: c, a]) :=>: b)
            <*> buildDerivation (((Array.delete res hyps) <> [c]) :=>: con)
      | Just res@(Variable a :->: b) <- findMap (matchVarImp hyps) hyps ->
        Next L0Imp seq
            <$> buildDerivation (((Array.delete res hyps) <> [b]) :=>: con)
    seq@(hyps :=>: a :\/: b) ->
            (Next ROr1 seq <$> buildDerivation (hyps :=>: a))
            <>
            (Next ROr2 seq <$> buildDerivation (hyps :=>: b))
    seq@(hyps :=>: Variable a) ->
        pure $ Leaf seq

isProof :: forall a. Eq a => Derivation (Sequent a) -> Boolean
isProof = case _ of
    Leaf (hyps :=>: a) -> a `elem` hyps
    Next _ _ d         -> isProof d
    Branch _ _ d1 d2   -> isProof d1 && isProof d2

tautology :: forall a. Eq a => Formula a -> Boolean
tautology = any isProof <<< buildDerivation <<< formulaToSequent

matchAnd :: forall a. Formula a -> Maybe (Formula a)
matchAnd = case _ of
    f@(a :/\: b) -> Just f
    _            -> Nothing

matchOr :: forall a. Formula a -> Maybe (Formula a)
matchOr = case _ of
    f@(a :\/: b) -> Just f
    _            -> Nothing

matchOrImp :: forall a. Formula a -> Maybe (Formula a)
matchOrImp = case _ of
    f@((a :\/: b) :->: c) -> Just f
    _                     -> Nothing

matchAndImp :: forall a. Formula a -> Maybe (Formula a)
matchAndImp = case _ of
    f@((a :/\: b) :->: c) -> Just f
    _                     -> Nothing

matchImpImp :: forall a. Formula a -> Maybe (Formula a)
matchImpImp = case _ of
    f@((a :->: b) :->: c) -> Just f
    _                     -> Nothing

matchVarImp :: forall a. Eq a => Array (Formula a) -> Formula a -> Maybe (Formula a)
matchVarImp hyps = case _ of
    f@(a :->: b)
      | a `elem` hyps -> Just f
    _                 -> Nothing
