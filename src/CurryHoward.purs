module CurryHoward where

import Prelude

import Control.Monad.RWS (get, put)
import Control.Monad.State (State, evalState, gets, runState)
import Data.Foldable (find)
import Data.Map (Map)
import Data.Map as M
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String as C
import Data.Traversable (traverse)
import LambdaCalculus.Syntax as LC
import Partial.Unsafe (unsafeCrashWith)
import SequentCalculus.Sequent ((:->:), (:/\:), (:\/:), (:=>:))
import SequentCalculus.Sequent as SC

typeToFormula :: LC.Type -> SC.Formula String
typeToFormula = case _ of
    LC.Type (LC.Symbol s) ->
        SC.Variable s
    LC.Type (LC.Index  s) ->
        unsafeCrashWith "unexpected Index in typeToFormula"
    LC.Function t1 t2 ->
        typeToFormula t1 :->: typeToFormula t2
    LC.Sum t1 t2 ->
        typeToFormula t1 :\/: typeToFormula t2
    LC.Product t1 t2 ->
        typeToFormula t1 :/\: typeToFormula t2

findTerm :: LC.Type -> Maybe LC.Term
findTerm =
    (_ `evalState` Map.empty)
        <<< map derivationToTerm
        <<< find SC.isProof
        <<< SC.buildDerivation
        <<< SC.formulaToSequent
        <<< typeToFormula


type DerivationState =
    { freshName :: Map String Int
    , scope     :: Map (SC.Formula String) String
    }

derivationToTerm
    :: SC.Derivation (SC.Sequent String)
    -> State DerivationState LC.Term
derivationToTerm = case _ of
    SC.Leaf (hyps :=>: var@(SC.Variable c)) ->
        maybe
            (unsafeCrashWith "did not find variable in hyps")
            (\_ -> do
                _ <- traverse findNameAndAddToScope hyps
                LC.Variable <<< LC.Symbol <$> findNameAndAddToScope var
            )
            $ find (_ == var) hyps
    SC.Leaf _ ->
        unsafeCrashWith "did not find variable in hyps"
    SC.Next SC.LAnd seq deriv -> do
        c <- derivationToTerm deriv

findNameAndAddToScope
    :: SC.Formula String
    -> State DerivationState String
findNameAndAddToScope = case _ of
    f@(SC.Variable v) -> go v f
    f@(a :/\: b) -> go "Tuple" f
    f@(a :\/: b) -> go "Either" f
    f@(a :->: b) -> go "Function" f
  where
    go :: String -> SC.Formula String -> State DerivationState String
    go name formula = do
        dState <- get
        case M.lookup name dState.freshName of
            Nothing -> do
                let
                    name' = C.toLower name <> "0"
                    dState' =
                        { freshName: M.insert name' 1 dState.freshName
                        , scope: M.insert formula name' dState.scope
                        }
                put dState'
                pure name'
            Just i -> do
                let
                    name' = C.toLower name <> show i
                    dState' =
                        { freshName: M.insert name' (i + 1) dState.freshName
                        , scope: M.insert formula name' dState.scope
                        }
                put dState'
                pure name'
