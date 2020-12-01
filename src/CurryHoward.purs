module CurryHoward where

import Prelude

import Control.Monad.RWS (get, put)
import Control.Monad.State (State, evalState, gets)
import Data.Foldable (find)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', maybe, maybe')
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
    map (_ `evalState` initialState)
        <<< map derivationToTerm
        <<< find SC.isProof
        <<< SC.buildDerivation
        <<< SC.formulaToSequent
        <<< typeToFormula

-- TODO: This type is wrong.
type DerivationState =
    { freshName :: Map String Int
    , scope     :: Map (SC.Formula String) String
    }

initialState :: DerivationState
initialState = mempty

derivationToTerm
    :: SC.Derivation (SC.Sequent String)
    -> State DerivationState LC.Term
derivationToTerm = case _ of
    SC.Leaf (hyps :=>: var@(SC.Variable _)) ->
        maybe'
            (\_ -> unsafeCrashWith $ show hyps <> "|" <> show var)
            (\_ -> do
                _ <- traverse findNameAndAddToScope hyps
                LC.Variable <<< LC.Symbol <$> findNameAndAddToScope var
            )
            $ find (_ == var) hyps
    SC.Leaf _ ->
        unsafeCrashWith "wrong leaf type"
    SC.Branch SC.RAnd (hyps :=>: (_ :/\: _)) left right -> do
        a1 <- derivationToTerm left
        b1 <- derivationToTerm right
        pure $ LC.Application (LC.Application LC.Tuple a1) b1
    SC.Next SC.ROr1 (hyps :=>: (_ :\/: _)) left -> do
        a <- derivationToTerm left
        pure $ LC.Application LC.Left a
    SC.Next SC.ROr2 (hyps :=>: (_ :\/: _)) right -> do
        a <- derivationToTerm right
        pure $ LC.Application LC.Right a
    SC.Next SC.RImp (hyps :=>: (a :->: _)) deriv -> do
        b <- derivationToTerm deriv
        aName <- lookup a
        pure $ LC.Abstraction (LC.Symbol aName) b

    _ -> unsafeCrashWith "rest"

lookup :: SC.Formula String -> State DerivationState String
lookup it =
    fromMaybe'
        (\_ -> unsafeCrashWith "Could not find expected formula")
        <<< M.lookup it <$> gets _.scope

substitute
    :: { term :: LC.Term, from :: LC.Term, to :: LC.Term }
    -> State DerivationState LC.Term
substitute { term } = pure term

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
