module Program exposing (..)

import Random exposing (Seed)

import Cnf

type alias Model =
    { seed : Maybe Seed
    , exprSize : Int
    , wExpr : Maybe (Cnf.Expr, Cnf.ExprWithValue)
    , iExpr : Maybe (Cnf.Expr, Cnf.ExprWithValue)
    , nExpr : Maybe (Cnf.Expr, Cnf.ExprWithValue)
    , dExpr : Maybe (Cnf.Expr, Cnf.ExprWithValue)
    , hover : Hover
    , values : Cnf.ValueDict
    }

type Msg
    = Initialize Int
    | ChangeSize Int
    | ShowImplicationStep
    | ShowNegationStep
    | ShowDisjunctionStep
    | NewFormula
    | UpdateValue Char Bool
    | DoNothing
    | NewHover Hover

type Hover
    = HoverWExpr TreePath
    | HoverIExpr TreePath
    | HoverNExpr TreePath
    | HoverDExpr TreePath
    | NoHover

type TreePath
    = Left TreePath
    | Right TreePath
    | Straight TreePath
    | Selected
