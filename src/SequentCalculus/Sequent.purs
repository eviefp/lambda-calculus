module SequentCalculus.Sequent where

import Prelude

import Data.Array as Array
import Data.Foldable (elem, findMap, foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Maybe (Maybe(..))

data Formula a
    = Variable a
    | Implication (Formula a) (Formula a)
    | And (Formula a) (Formula a)
    | Or (Formula a) (Formula a)

derive instance eqFormula :: Eq a => Eq (Formula a)
derive instance genericFormula :: Generic (Formula a) _

instance showFormula :: Show a => Show (Formula a) where
    show f = genericShow f

infixr 7 Implication as :->:
infixr 6 And as :/\:
infixr 6 Or as :\/:

data Sequent a = Sequent (Array (Formula a)) (Formula a)

derive instance genericSequent :: Generic (Sequent a) _

instance showSequent :: Show a => Show (Sequent a) where
    show f = genericShow f

infixr 5 Sequent as :=>:

sequentToFormula :: forall a. Sequent a -> Formula a
sequentToFormula (xs :=>: con) = case Array.uncons xs of
    Just { head, tail } -> foldl (:/\:) head tail :->: con
    Nothing -> con

formulaToSequent :: forall a. Formula a -> Sequent a
formulaToSequent f = [] :=>: f

-- We have a formula
-- we can create a sequent
-- we search for a proof?

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

matchAnd :: forall a. Formula a -> Maybe (Formula a)
matchAnd = case _ of
    f@(a :/\: b) -> Just f
    _            -> Nothing

matchOr :: forall a. Formula a -> Maybe (Formula a)
matchOr = case _ of
    f@(a :\/: b) -> Just f
    _            -> Nothing

matchImp :: forall a. Formula a -> Maybe (Formula a)
matchImp = case _ of
    f@(a :->: b) -> Just f
    _            -> Nothing

-- Sequent $ [] => A \/ B -> B \/ A
-- [Next RImp $ A \/ B => B \/ A]
buildDerivation :: forall a. Eq a => Sequent a -> List (Derivation (Sequent a))
buildDerivation = case _ of
    seq@(hyps :=>: con)
      | Just res@(a :/\: b) <- findMap matchAnd hyps ->
          Next LAnd seq
              <$> buildDerivation (((Array.delete res hyps) <> [a, b]) :=>: con)
      | Just res@(a :\/: b) <- findMap matchOr hyps ->
        Branch LOr seq
            <$> buildDerivation (((Array.delete res hyps) <> [a]) :=>: con)
            <*> buildDerivation (((Array.delete res hyps) <> [b]) :=>: con)
      | Just res@((a :\/: b) :->: c) <- findMap matchImp hyps ->
        Next LOrImp seq
            <$> buildDerivation (((Array.delete res hyps) <> [a :->: c, b :->: c]) :=>: con)
      | Just res@((a :/\: b) :->: c) <- findMap matchImp hyps ->
        Next LAndImp seq
            <$> buildDerivation (((Array.delete res hyps) <> [a :->: b :->: c]) :=>: con)
      | Just res@((a :->: b) :->: c) <- findMap matchImp hyps ->
        Branch LImpImp seq
            <$> buildDerivation (((Array.delete res hyps) <> [b :->: c, a]) :=>: b)
            <*> buildDerivation (((Array.delete res hyps) <> [c]) :=>: con)
      -- TODO: find all here!
      | Just res@(Variable a :->: b) <- findMap matchImp hyps ->
          case Variable a `elem` hyps of
              false -> pure $ Leaf seq
              true  ->
                  Next L0Imp seq
                      <$> buildDerivation (((Array.delete res hyps) <> [b]) :=>: con)
    seq@(hyps :=>: Variable a) ->
        pure $ Leaf seq
    seq@(hyps :=>: a :->: b) ->
        Next RImp seq <$> buildDerivation ((Array.cons a hyps) :=>: b)
    seq@(hyps :=>: a :/\: b) ->
        Branch RAnd seq
            <$> buildDerivation (hyps :=>: a)
            <*> buildDerivation (hyps :=>: b)
    seq@(hyps :=>: a :\/: b) ->
            (Next ROr1 seq <$> buildDerivation (hyps :=>: a))
            <>
            (Next ROr2 seq <$> buildDerivation (hyps :=>: b))

-- A => B -> A
e1 :: Sequent String
e1 = [Variable "A"] :=>: Variable "B" :->: Variable "A"

-- _ => A /\ B -> A
e2 :: Sequent String
e2 = [] :=>: Variable "A" :/\: Variable "B" :->: Variable "A"
