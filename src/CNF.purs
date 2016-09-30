{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CNF where

import Prelude
import Data.Map as M
import Data.Set as S
import Data.Array as A
import Data.Maybe
import Data.String as Str
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Control.Monad.Reader (Reader, ask)

data Expr = Conjunction Expr Expr
          | Disjunction Expr Expr
          | Implication Expr Expr
          | Negation Expr
          | Symbol Char

prettyPrint :: Expr -> String
prettyPrint (Conjunction a@(Conjunction _ _) b@(Conjunction _ _)) = prettyPrint a <> " ∧ " <> prettyPrint b
prettyPrint (Conjunction a@(Conjunction _ _) b) = prettyPrint a <> " ∧ " <> withParens b
prettyPrint (Conjunction a b@(Conjunction _ _)) = withParens a <> " ∧ " <> prettyPrint b
prettyPrint (Conjunction a b) = withParens a <> " ∧ " <> withParens b
prettyPrint (Disjunction a@(Disjunction _ _) b@(Disjunction _ _)) = prettyPrint a <> " ∨ " <> prettyPrint b
prettyPrint (Disjunction a@(Disjunction _ _) b) = prettyPrint a <> " ∨ " <> withParens b
prettyPrint (Disjunction a b@(Disjunction _ _)) = withParens a <> " ∨ " <> prettyPrint b
prettyPrint (Disjunction a b) = withParens a <> " ∨ " <> withParens b
prettyPrint (Implication a b) = withParens a <> " ⇒ " <> withParens b
prettyPrint (Negation a) = "¬" <> withParens a
prettyPrint (Symbol c) = Str.singleton c

withParens :: Expr -> String
withParens s@(Symbol _) = prettyPrint s
withParens s@(Negation _) = prettyPrint s
withParens expr = "(" <> prettyPrint expr <> ")"

instance arbitraryExpr :: Arbitrary Expr where
    arbitrary = sized sizedExpr

sizedExpr :: Int -> Gen Expr
sizedExpr = expr
    where
        symb = Symbol <$> elements 'A' ['B', 'C']
        neg n = Negation <$> expr (n - 1)
        conj n = Conjunction <$> expr (n `div` 2) <*> expr (n `div` 2)
        disj n = Disjunction <$> expr (n `div` 2) <*> expr (n `div` 2)
        impl n = Implication <$> expr (n `div` 2) <*> expr (n `div` 2)
        expr 0 = symb
        expr n = oneOf (neg n) [conj n, disj n, impl n]

freeVars :: Expr -> S.Set Char
freeVars (Conjunction a b) = S.union (freeVars a) (freeVars b)
freeVars (Disjunction a b) = S.union (freeVars a) (freeVars b)
freeVars (Implication a b) = S.union (freeVars a) (freeVars b)
freeVars (Negation a) = freeVars a
freeVars (Symbol c) = S.singleton c

genValues :: S.Set Char -> Gen (M.Map Char Boolean)
genValues vars = do
    values <- S.size vars `vectorOf` arbitrary
    pure <<< M.fromFoldable $ A.zip (S.toUnfoldable vars) values

data Formula = Formula (M.Map Char Boolean) Expr

instance arbitraryFormula :: Arbitrary Formula where
    arbitrary = sized sizedFormula

sizedFormula :: Int -> Gen Formula
sizedFormula n = do
    expr <- sizedExpr n
    let vars = freeVars expr
    Formula <$> genValues vars <*> pure expr


{-- Evaluation
 -----------------------------------------}

type Eval a = Reader (M.Map Char Boolean) a

lookupSymbol :: Char -> Eval Boolean
lookupSymbol c = fromMaybe false <<< M.lookup c <$> ask

evaluate :: Expr -> Eval Boolean
evaluate (Conjunction a b) = (&&) <$> evaluate a <*> evaluate b
evaluate (Disjunction a b) = (||) <$> evaluate a <*> evaluate b
evaluate (Implication a b) = do
    pred <- evaluate a
    if pred
        then evaluate b
        else pure true
evaluate (Negation e) = (not) <$> evaluate e
evaluate (Symbol c) = lookupSymbol c

{-- Conversion
 -----------------------------------------}

removeImplication (Conjunction a b) = (removeImplication a) `Conjunction` (removeImplication b)
removeImplication (Disjunction a b) = (removeImplication a) `Disjunction` (removeImplication b)
removeImplication (Implication a b) = (Negation $ removeImplication a) `Disjunction` removeImplication b
removeImplication (Negation e) = Negation (removeImplication e)
removeImplication (Symbol c) = Symbol c

moveNegation (Negation (Negation e)) = moveNegation e
moveNegation (Negation (Conjunction a b)) = moveNegation (Negation a) `Disjunction` moveNegation (Negation b)
moveNegation (Negation (Disjunction a b)) = moveNegation (Negation a) `Conjunction` moveNegation (Negation b)
moveNegation (Conjunction a b) = (moveNegation a) `Conjunction` (moveNegation b)
moveNegation (Disjunction a b) = (moveNegation a) `Disjunction` (moveNegation b)
moveNegation (Implication a b) = (moveNegation a) `Implication` (moveNegation b)
moveNegation (Negation e) = Negation (moveNegation e)
moveNegation (Symbol c) = Symbol c

distribute (Disjunction (Conjunction a b) c) = (distribute $ Disjunction a c) `Conjunction` (distribute $ Disjunction b c)
distribute (Disjunction a (Conjunction b c)) = (distribute $ Disjunction a b) `Conjunction` (distribute $ Disjunction a c)
distribute (Conjunction a b) = (distribute a) `Conjunction` (distribute b)
distribute (Disjunction a b) =
    let a' = distribute a
        b' = distribute b
    in case a' of
        (Conjunction _ _) -> distribute $ a' `Disjunction` b'
        _ -> case b' of
            (Conjunction _ _) -> distribute $ a' `Disjunction` b'
            _ -> a' `Disjunction` b'

distribute (Implication a b) = (distribute a) `Implication` (distribute b)
distribute (Negation e) = Negation (distribute e)
distribute (Symbol c) = Symbol c

toCnf = distribute <<< moveNegation <<< removeImplication

isCnf :: Expr -> Boolean
isCnf = conj
    where
        conj (Implication _ _) = false
        conj (Conjunction a b) = conj a && conj b
        conj e = disj e
        disj (Implication _ _) = false
        disj (Conjunction _ _) = false
        disj (Disjunction a b) = disj a && disj b
        disj e = neg e
        neg (Implication _ _) = false
        neg (Conjunction _ _) = false
        neg (Disjunction _ _) = false
        neg (Negation e) = neg e
        neg e = true
