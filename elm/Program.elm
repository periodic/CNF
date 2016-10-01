module Program exposing (..)

import Random exposing (Seed)

import Cnf

type alias Model =
    { seed : Maybe Seed
    , exprSize : Int
    , wExpr : Maybe Cnf.Expr
    , iExpr : Maybe Cnf.Expr
    , nExpr : Maybe Cnf.Expr
    , dExpr : Maybe Cnf.Expr
    }

type Msg
    = Initialize Int
    | ChangeSize Int
    | ShowImplicationStep
    | ShowNegationStep
    | ShowDisjunctionStep
    | ShowValue
    | NewFormula
    | DoNothing

