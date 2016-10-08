import Dict
import Html exposing (Html)
import Html.App as App
import Maybe
import Random exposing (Seed, minInt, maxInt)
import Platform.Cmd as Cmd
import Platform.Sub as Sub

import Cnf as Cnf
import Program exposing (..)
import View as View

model : Model
model =
    { seed = Nothing
    , exprSize = 4
    , wExpr = Nothing
    , iExpr = Nothing
    , nExpr = Nothing
    , dExpr = Nothing
    , hover = NoHover
    , values = Dict.fromList [('A', False), ('B', False), ('C', False)]
    }

view : Model -> Html Msg
view model =
    case model.seed of
        Nothing ->
            View.initializing
        Just seed ->
            View.renderForm model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Initialize val ->
            let seed = Random.initialSeed val
                (expr, seed') = Cnf.generateExpr model.exprSize seed
                expr' = Cnf.withValues model.values expr
            in ({ model | seed = Just seed', wExpr = Just (expr, expr') }, Cmd.none)
        ChangeSize newSize ->
            ({ model | exprSize = newSize }, Cmd.none)
        NewFormula ->
            case model.seed of
                Nothing -> 
                    (model, Cmd.none)
                Just seed ->
                    let
                        (expr, seed') = Cnf.generateExpr model.exprSize seed
                        exprWithValues = Cnf.withValues model.values expr
                    in
                       ({model
                        | seed = Just seed'
                        , wExpr = Just (expr, exprWithValues)
                        , iExpr = Nothing
                        , nExpr = Nothing
                        , dExpr = Nothing
                        }, Cmd.none)
        ShowImplicationStep ->
            case model.wExpr of
                Nothing ->
                    (model, Cmd.none)
                Just (expr, _) ->
                    let
                        expr' = Cnf.transformImplications expr
                        exprWithValues' = Cnf.withValues model.values expr'
                    in
                        ({model | iExpr = Just (expr', exprWithValues') }, Cmd.none)
        ShowNegationStep ->
            case model.iExpr of
                Nothing ->
                    (model, Cmd.none)
                Just (expr, _) ->
                    let
                        expr' = Cnf.moveNegation expr
                        exprWithValues' = Cnf.withValues model.values expr'
                    in
                        ({model | nExpr = Just (expr', exprWithValues') }, Cmd.none)
        ShowDisjunctionStep ->
            case model.nExpr of
                Nothing ->
                    (model, Cmd.none)
                Just (expr, _) ->
                    let
                        expr' = Cnf.distribute expr
                        exprWithValues' = Cnf.withValues model.values expr'
                    in
                        ({model | dExpr = Just (expr', exprWithValues') }, Cmd.none)
        NewHover hover ->
            ({ model | hover = hover }, Cmd.none)
        UpdateValue c v ->
            let
                values' = Dict.insert c v model.values
                wExpr' = Maybe.map (\(expr, _) -> (expr, Cnf.withValues values' expr)) model.wExpr
                iExpr' = Maybe.map (\(expr, _) -> (expr, Cnf.withValues values' expr)) model.iExpr
                nExpr' = Maybe.map (\(expr, _) -> (expr, Cnf.withValues values' expr)) model.nExpr
                dExpr' = Maybe.map (\(expr, _) -> (expr, Cnf.withValues values' expr)) model.dExpr
            in
                ({ model
                    | values = values'
                    , wExpr = wExpr'
                    , iExpr = iExpr'
                    , nExpr = nExpr'
                    , dExpr = dExpr'
                    }, Cmd.none)
        DoNothing ->
            (model, Cmd.none)

subscriptions :  Model -> Sub Msg
subscriptions model = Sub.none

main =
    App.program
        { init = (model, Random.generate Initialize (Random.int minInt maxInt))
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
