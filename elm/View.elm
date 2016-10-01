module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (Seed)
import String as Str

import Cnf as Cnf
import Program exposing (..)


initializing : Html Msg
initializing =
    div []
        [ text "Initializing"
        ]

renderForm : Model -> Html Msg
renderForm { wExpr, iExpr, nExpr, dExpr, exprSize } =
    div []
        [ renderHeader exprSize
        , renderWExpr wExpr
        , renderIExpr iExpr
        , renderNExpr nExpr
        , renderDExpr dExpr
        ]

renderHeader : Int -> Html Msg
renderHeader exprSize =
    div [ class "header" ]
        [ div [ class "exprSizeInput" ]
            [ label [ for "exprSize"] [ text "Expression Size" ]
            , input [ type' "input", value <| toString exprSize, onInput parseSize ] []
            ]
        , div [ class "newExprButton" ]
            [ input [ type' "button", value "New Expression", onClick NewFormula ] [] ]
        ]

parseSize : String -> Msg
parseSize str =
    case Str.toInt str of
        Ok size ->
            ChangeSize size
        _ ->
            DoNothing

renderWExpr : Maybe Cnf.Expr -> Html Msg
renderWExpr maybeExpr =
    div [ class "wExpr" ]
        [ text "Expression:"
        , renderExpr maybeExpr
        ]

renderIExpr : Maybe Cnf.Expr -> Html Msg
renderIExpr maybeExpr =
    div [ class "iExpr" ]
        [ text "Remove implications:"
        , input [ type' "button", value "Check", onClick ShowImplicationStep ] []
        , renderExpr maybeExpr
        ]

renderNExpr : Maybe Cnf.Expr -> Html Msg
renderNExpr maybeExpr =
    div [ class "nExpr" ]
        [ text "Move negations:"
        , input [ type' "button", value "Check", onClick ShowNegationStep ] []
        , renderExpr maybeExpr
        ]

renderDExpr : Maybe Cnf.Expr -> Html Msg
renderDExpr maybeExpr =
    div [ class "dExpr" ]
        [ text "Distribute disjunctions:"
        , input [ type' "button", value "Check", onClick ShowDisjunctionStep ] []
        , renderExpr maybeExpr
        ]

renderExpr :  Maybe Cnf.Expr -> Html Msg
renderExpr expr =
    case expr of
        Nothing ->
            div [ class "expr-empty" ] []
        Just expr ->
            div [ class "expr" ]
                [ text <| Cnf.prettyPrint expr ]

