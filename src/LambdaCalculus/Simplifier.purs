module LambdaCalculus.Simplifier where

import Prelude

import Data.Array (elem, (:))
import Data.Bifunctor (lmap)
import Data.Enum (enumFromTo)
import Data.Foldable (oneOf)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe')
import LambdaCalculus.Syntax as S
import Partial.Unsafe (unsafeCrashWith)

simplify :: S.Term -> S.Term
simplify = fromLocallyNamed <<< recurseWhileNotEq <<< toLocallyNamed
  where
    recurseWhileNotEq :: S.Term -> S.Term
    recurseWhileNotEq t =
        let t' = simplify' t
        in if t == t'
            then t
            else recurseWhileNotEq t'

simplify' :: S.Term -> S.Term
simplify' =
    case _ of
        x@(S.Variable _) -> x
        S.Abstraction variable term ->
            S.Abstraction variable (simplify' term)
        S.Application
            S.First (S.Application (S.Application S.Tuple first) _) -> first
        S.Application
            S.Second (S.Application (S.Application S.Tuple _) second) -> second
        S.Application
            (S.Application
                (S.Application
                    S.Either
                    left
                )
                _
            )
            (S.Application S.Left value) -> S.Application left value
        S.Application
            (S.Application
                (S.Application
                    S.Either
                    _
                )
                right
            )
            (S.Application S.Right value) -> S.Application right value
        S.Application left right ->
            case simplify' left of
                S.Abstraction variable left' ->
                    simplify' $ open left' (simplify' right)
                term' -> S.Application term' (simplify' right)
        term -> term


open :: S.Term -> S.Term -> S.Term
open term replacement = go 0 term
  where
    go outer = case _ of
        S.Application left right ->
            S.Application (go outer left) (go outer right)
        S.Abstraction sym t ->
            S.Abstraction sym (go (outer + 1) t)
        v@(S.Variable (S.Symbol var)) -> v
        v@(S.Variable (S.Index idx))
            | idx == outer -> replacement
            | otherwise    -> v
        t -> t

substitute :: S.Symbol -> S.Term -> S.Term -> S.Term
substitute variable term replacement =
    case term of
        S.Variable var
            | variable == var -> replacement
            | otherwise       -> S.Variable var
        S.Application left right ->
            S.Application
                (substitute variable left replacement)
                (substitute variable right replacement)
        S.Abstraction var innerTerm ->
            S.Abstraction
                var
                (substitute variable innerTerm replacement)
        _ -> term

mapKeys :: forall a k k1. Ord k1 => (k -> k1) -> Map k a -> Map k1 a
mapKeys f = M.fromFoldable <<< mapArray (lmap f) <<< M.toUnfoldable
  where
    mapArray :: forall b c. (b -> c) -> Array b -> Array c
    mapArray = map

locallyNamedFV :: S.Term -> Array String
locallyNamedFV = case _ of
    S.Application left right   -> locallyNamedFV left <> locallyNamedFV right
    S.Abstraction _ body       -> locallyNamedFV body
    S.Variable (S.Symbol name) -> pure name
    _                          -> []

freshName :: String -> S.Term -> String
freshName name body =
    fromMaybe'
       (\_ -> unsafeCrashWith $
           "Could not find fresh name for (" <> name <> ")\n"
           <> "within (" <> show body <> ")\n"
           <> "where fv = " <> show fv <> "\n"
           <> "names = " <> show names <> "\n"
           <> "validate <$> names" <> show (validate <$> names)
           <> "oneOf = " <> show (oneOf (validate <$> names))
        )
       <<< oneOf
       $ validate <$> names
  where
    fv :: Array String
    fv = locallyNamedFV body

    names :: Array String
    names = name : ( (name <> _) <<< show <$> enumFromTo 0 1000 )

    validate :: String -> Maybe String
    validate s =
        if s `elem` fv
            then Nothing
            else Just s

fromLocallyNamed :: S.Term -> S.Term
fromLocallyNamed t = go mempty t
  where
    go :: Map Int String -> S.Term -> S.Term
    go env = case _ of
        S.Application left right ->
            S.Application (go env left) (go env right)
        S.Abstraction (S.Index _) term ->
            unsafeCrashWith
                $ "Unexpected locally named term in " <> show t
        S.Abstraction (S.Symbol var) term ->
            let var' = freshName var term
            in S.Abstraction
                    (S.Symbol var')
                    $ go (M.insert 0 var' $ mapKeys (_ + 1) env) term
        v@(S.Variable (S.Index idx)) ->
            case M.lookup idx env of
                Nothing ->
                    unsafeCrashWith
                        $ "Bad index in locally named term: " <> show idx <> ":" <> show t <> show env
                Just var ->
                    S.Variable (S.Symbol var)
        term -> term

toLocallyNamed :: S.Term -> S.Term
toLocallyNamed = go mempty
  where
    go :: Map String Int -> S.Term -> S.Term
    go env = case _ of
        S.Application left right ->
            S.Application (go env left) (go env right)
        S.Abstraction (S.Index _) term ->
            unsafeCrashWith "Unexpected locally named term."
        S.Abstraction (S.Symbol var) term ->
            S.Abstraction
                (S.Symbol var)
                $ go (M.insert var 0 $ (_ + 1) <$> env) term
        v@(S.Variable (S.Symbol var)) ->
            case M.lookup var env of
                Nothing -> v
                Just i  -> S.Variable (S.Index i)
        term -> term

