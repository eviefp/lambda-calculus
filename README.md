# Deriving Lambda Calculus Terms from Sequent Calculus Proofs

This project is about deriving lambda calculus terms from proofs using an
efficient sequent calculus proof search algorithm.

The plan is to have this working for a trivial lambda calculus with variables,
abstraction, application, sum, and product types.

Once this is done, we will try to add more features to the lambda calculus, and
have the sequent calculus proof search and the term translation code match these
features.

What to add:
- the `Void` type
- product constructors
- sum type constructors
- recursive types
- polymorphism
- higher kinded types
- ...?
- dependent types?




## TODO

- pretty printer for LC

Here's a list of things that would be nice to have but aren't trivial:

- Sequent/PrettyPrinting, I would like to be able to put documents side-by-side
    This is hard to do because Wadler-Leijen printers do not natively support this sort of operation.

