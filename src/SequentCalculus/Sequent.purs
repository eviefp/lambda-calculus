module SequentCalculus.Sequent where

data Formula a
    = Variable a
    | Implication (Formula a) (Formula a)
    | And (Formula a) (Formula a)
    | Or (Formula a) (Formula a)

infixr 7 Implication as :->:
infixr 6 And as :/\:
infixr 6 Or as :\/:

data Sequent a = Sequent (Array (Formula a)) (Array (Formula a))

infixr 5 Sequent as :=>:

-- A => B -> A
e1 :: Sequent String
e1 = [Variable "A"] :=>: [Variable "B" :->: Variable "A"]

-- _ => A /\ B -> A
e2 :: Sequent String
e2 = [] :=>: [Variable "A" :/\: Variable "B" :->: Variable "A"]
