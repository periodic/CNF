module Cnf exposing (..)

import Dict exposing (Dict)
import Dict as D
import List as L
import Maybe as M
import Set exposing (Set)
import Set as S
import String as Str


import Random exposing (..)

type Expr = Conjunction Expr Expr
          | Disjunction Expr Expr
          | Implication Expr Expr
          | Negation Expr
          | Symbol Char

elem : a -> List a -> Seed -> (a, Seed)
elem e0 es seed =
    let (idx, seed') = step (int 0 (L.length es + 1)) seed
        val = L.drop idx es |> L.head |> M.withDefault e0
    in (val, seed')

generateExpr : Int -> Seed -> (Expr, Seed)
generateExpr size seed =
    if size == 0
       then generateTerminal seed
       else generateNonTerminal size seed

generateTerminal : Seed -> (Expr, Seed)
generateTerminal seed =
    let (char, seed') = elem 'A' ['B', 'C'] seed
    in (Symbol char, seed')

generateNonTerminal : Int -> Seed -> (Expr, Seed)
generateNonTerminal size seed0 =
    if size == 1
       then let
                (expr, seed1) = generateTerminal seed0
                (negate, seed2) = step bool seed1
            in
               if negate
                  then (Negation expr, seed2)
                  else (expr, seed2)
       else let
                (leftSize, seed1) = step (int 1 (size - 1)) seed0
                rightSize = size - leftSize
                (operator, seed2) = elem Conjunction [Disjunction, Implication] seed1
                (left, seed3) = generateExpr leftSize seed2
                (right, seed4) = generateExpr rightSize seed3
                (negate, seed5) = step bool seed4
            in
               if negate
                  then (Negation (operator left right), seed5)
                  else (operator left right, seed5)

prettyPrint : Expr -> String
prettyPrint expr =
    case expr of
        Conjunction a b ->
            case (a, b) of
                (Conjunction _ _, Conjunction _ _) ->
                    prettyPrint a ++ " ∧ " ++ prettyPrint b
                (Conjunction _ _, _) ->
                    prettyPrint a ++ " ∧ " ++ withParens b
                (_, Conjunction _ _) ->
                    withParens a ++ " ∧ " ++ prettyPrint b
                _ ->
                    withParens a ++ " ∧ " ++ withParens b
        Disjunction a b ->
            case (a, b) of
                (Disjunction _ _, Disjunction _ _) ->
                    prettyPrint a ++ " ∨ " ++ prettyPrint b
                (Disjunction _ _, b) ->
                    prettyPrint a ++ " ∨ " ++ withParens b
                (a, Disjunction _ _) ->
                    withParens a ++ " ∨ " ++ prettyPrint b
                _ ->
                 withParens a ++ " ∨ " ++ withParens b
        Implication a b ->
             withParens a ++ " ⇒ " ++ withParens b
        Negation a ->
             "¬" ++ withParens a
        Symbol c ->
             Str.fromChar c

withParens : Expr -> String
withParens expr =
    case expr of
        (Symbol _) -> prettyPrint expr
        (Negation _) -> prettyPrint expr
        _ -> "(" ++ prettyPrint expr ++ ")"


freeVars : Expr -> Set Char
freeVars expr =
    case expr of
        Conjunction a b ->
            S.union (freeVars a) (freeVars b)
        Disjunction a b ->
            S.union (freeVars a) (freeVars b)
        Implication a b ->
            S.union (freeVars a) (freeVars b)
        Negation a ->
            freeVars a
        Symbol c ->
            S.singleton c

transformImplications : Expr -> Expr
transformImplications expr =
    case expr of
        Implication a b ->
            case a of
                Negation a' ->
                    Disjunction (transformImplications a') (transformImplications b)
                _ ->
                    Disjunction (Negation (transformImplications a)) (transformImplications b)
        Conjunction a b ->
            Conjunction (transformImplications a) (transformImplications b)
        Disjunction a b ->
            Disjunction (transformImplications a) (transformImplications b)
        Negation e ->
            Negation (transformImplications e)
        symb ->
            symb

moveNegation : Expr -> Expr
moveNegation expr =
    case expr of
        (Negation (Negation e)) ->
            moveNegation e
        (Negation (Conjunction a b)) ->
            Disjunction (moveNegation (Negation a)) (moveNegation (Negation b))
        (Negation (Disjunction a b)) ->
            Conjunction (moveNegation (Negation a)) (moveNegation (Negation b))
        (Conjunction a b) ->
            Conjunction (moveNegation a) (moveNegation b)
        (Disjunction a b) ->
            Disjunction (moveNegation a) (moveNegation b)
        (Implication a b) ->
            Implication (moveNegation a) (moveNegation b)
        (Negation e) ->
            Negation (moveNegation e)
        (Symbol c) ->
            Symbol c

distribute : Expr -> Expr
distribute expr =
    case expr of
        (Disjunction (Conjunction a b) c) ->
            Conjunction (distribute <| Disjunction a c) (distribute <| Disjunction b c)
        (Disjunction a (Conjunction b c)) ->
            Conjunction (distribute <| Disjunction a b) (distribute <| Disjunction a c)
        (Conjunction a b) ->
            Conjunction (distribute a) (distribute b)
        (Disjunction a b) ->
            let
                a' = distribute a
                b' = distribute b
            in case a' of
                (Conjunction _ _) ->
                    distribute <| Disjunction a' b'
                _ ->
                    case b' of
                        (Conjunction _ _) ->
                            distribute <| Disjunction a' b'
                        _ ->
                            Disjunction a' b'
        (Implication a b) ->
            Implication (distribute a) (distribute b)
        (Negation e) ->
            Negation (distribute e)
        (Symbol c) ->
            Symbol c

type alias ValueDict = Dict Char Bool
type Formula = Formula ValueDict Expr


