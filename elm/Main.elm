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
            in ({ model | seed = Just seed', wExpr = Just expr }, Cmd.none)
        ChangeSize newSize ->
            ({ model | exprSize = newSize }, Cmd.none)
        NewFormula ->
            case model.seed of
                Nothing -> 
                    (model, Cmd.none)
                Just seed ->
                    let
                        (expr, seed') = Cnf.generateExpr model.exprSize seed
                    in
                       ({model
                        | seed = Just seed'
                        , wExpr = Just expr
                        , iExpr = Nothing
                        , nExpr = Nothing
                        , dExpr = Nothing
                        }, Cmd.none)
        ShowImplicationStep ->
            case model.wExpr of
                Nothing ->
                    (model, Cmd.none)
                Just expr ->
                    ({model | iExpr = Just <| Cnf.transformImplications expr }, Cmd.none)
        ShowNegationStep ->
            case model.iExpr of
                Nothing ->
                    (model, Cmd.none)
                Just expr ->
                    ({model | nExpr = Just <| Cnf.moveNegation expr }, Cmd.none)
        ShowDisjunctionStep ->
            case model.nExpr of
                Nothing ->
                    (model, Cmd.none)
                Just expr ->
                    ({model | dExpr = Just <| Cnf.distribute expr }, Cmd.none)
        _ ->
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
