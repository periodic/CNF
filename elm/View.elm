module View exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (Seed)
import String as Str

import Cnf as Cnf
import Program exposing (..)
import Json.Decode exposing (succeed)


initializing : Html Msg
initializing =
    div []
        [ text "Initializing"
        ]

renderForm : Model -> Html Msg
renderForm { wExpr, iExpr, nExpr, dExpr, exprSize, hover, values } =
    div []
        [ renderHeader exprSize
        , renderValues values
        , renderWExpr (Maybe.map snd wExpr) hover
        , renderIExpr (Maybe.map snd iExpr) hover
        , renderNExpr (Maybe.map snd nExpr) hover
        , renderDExpr (Maybe.map snd dExpr) hover
        ]

renderValues : Cnf.ValueDict -> Html Msg
renderValues values =
    div [ class "valueForm" ]
        [ text "Values:"
        , div [ class "valueForm-values" ]
            (List.map renderValue <| Dict.toList values)
        ]

renderValue : (Char, Bool) -> Html Msg
renderValue (char, val) =
    let
        c = Str.fromChar char
        true = toString True
        false = toString False
        callback str =
            if str == true
               then UpdateValue char True
               else UpdateValue char False
    in
        div [ class "valueForm-value" ]
            [ label [ for c ]
                [ text c ]
            , select [ onInput callback ]
                [ option [ selected (val), value true ] [ text true ]
                , option [ selected (not val), value false ] [ text false ]
                ]
            ]

renderHeader : Int -> Html Msg
renderHeader exprSize =
    div [ class "header" ]
        [ div [ class "exprSizeInput" ]
            [ label [ for "exprSize"] [ text "Expression Size: " ]
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

renderWExpr : Maybe Cnf.ExprWithValue -> Hover -> Html Msg
renderWExpr maybeExpr hover =
    div [ class "wExpr" ]
        [ text "Expression:"
        , case hover of
            HoverWExpr path ->
                renderMaybeExpr maybeExpr (renderWithHover HoverWExpr path) (text "Loading...")
            _ ->
                renderMaybeExpr maybeExpr (renderExpr HoverWExpr) (text "Loading...")
        ]

renderIExpr : Maybe Cnf.ExprWithValue -> Hover -> Html Msg
renderIExpr maybeExpr hover =
    div [ class "iExpr" ]
        [ text "Remove implications:"
        , case hover of
            HoverIExpr path ->
                renderMaybeExpr maybeExpr (renderWithHover HoverIExpr path) (showButton ShowImplicationStep)
            _ ->
                renderMaybeExpr maybeExpr (renderExpr HoverIExpr) (showButton ShowImplicationStep)
        ]

renderNExpr : Maybe Cnf.ExprWithValue -> Hover -> Html Msg
renderNExpr maybeExpr hover =
    div [ class "nExpr" ]
        [ text "Move negations:"
        , case hover of
            HoverNExpr path ->
                renderMaybeExpr maybeExpr (renderWithHover HoverNExpr path) (showButton ShowNegationStep)
            _ ->
                renderMaybeExpr maybeExpr (renderExpr HoverNExpr) (showButton ShowNegationStep)
        ]

renderDExpr : Maybe Cnf.ExprWithValue -> Hover -> Html Msg
renderDExpr maybeExpr hover =
    div [ class "dExpr" ]
        [ text "Distribute disjunctions:"
        , case hover of
            HoverDExpr path ->
                renderMaybeExpr maybeExpr (renderWithHover HoverDExpr path) (showButton ShowDisjunctionStep)
            _ ->
                renderMaybeExpr maybeExpr (renderExpr HoverDExpr) (showButton ShowDisjunctionStep)
        ]

showButton : Msg -> Html Msg
showButton msg =
    input [ type' "button", value "Reveal", onClick msg ] []

renderMaybeExpr : Maybe Cnf.ExprWithValue -> (Cnf.ExprWithValue -> Html Msg) -> Html Msg -> Html Msg
renderMaybeExpr maybeExpr renderer child =
    div [ class "exprContainer" ]
        [ case maybeExpr of
            Nothing ->
                div [ class "expr-empty" ]
                    [ child ]
            Just expr ->
                div [ class "" ]
                    [ renderer expr ]
        ]

renderExpr : (TreePath -> Hover) -> Cnf.ExprWithValue -> Html Msg
renderExpr path expr =
    case expr of
        Cnf.ConjValue v a b ->
            span
                [ class "expr complex conjunction"
                , onHover (path Selected)
                ]
                [ renderExpr (Left >> path) a
                , text "∧"
                , renderExpr (Right >> path) b
                , tooltip v
                ]
        Cnf.DisjValue v a b ->
            span
                [ class "expr complex disjunction"
                , onHover (path Selected)
                ]
                [ renderExpr (Left >> path) a
                , text "∨"
                , renderExpr (Right >> path) b
                , tooltip v
                ]
        Cnf.ImplValue v a b ->
            span
                [ class "expr complex implication"
                , onHover (path Selected)
                ]
                [ renderExpr (Left >> path) a
                , text "⇒"
                , renderExpr (Right >> path) b
                , tooltip v
                ]
        Cnf.NegValue v e ->
            span
                [ class "expr negation"
                , onHover (path Selected)
                ]
                [ text "¬"
                , renderExpr (Straight >> path) e
                , tooltip v
                ]
        Cnf.SymbValue v c ->
            span
                [ class "expr symbol"
                , onHover (path Selected)
                ]
                [ text <| Str.fromChar c 
                , tooltip v
                ]

goLeft : (TreePath -> Hover) -> TreePath -> Cnf.ExprWithValue -> Html Msg
goLeft path hoverPath expr =
    case hoverPath of
        Left hoverPath' ->
            renderWithHover (Left >> path) hoverPath' expr
        _ ->
            renderExpr (Left >> path) expr

goRight : (TreePath -> Hover) -> TreePath -> Cnf.ExprWithValue -> Html Msg
goRight path hoverPath expr =
    case hoverPath of
        Right hoverPath' ->
            renderWithHover (Right >> path) hoverPath' expr
        _ ->
            renderExpr (Right >> path) expr

goStraight : (TreePath -> Hover) -> TreePath -> Cnf.ExprWithValue -> Html Msg
goStraight path hoverPath expr =
    case hoverPath of
        Straight hoverPath' ->
            renderWithHover (Straight >> path) hoverPath' expr
        _ ->
            renderExpr (Straight >> path) expr

tooltip : Bool -> Html Msg
tooltip v =
    span [ class "tooltip" ] [ text <| toString v ]

renderWithHover : (TreePath -> Hover) -> TreePath -> Cnf.ExprWithValue -> Html Msg
renderWithHover path hoverPath expr =
    let
        isHovered = case hoverPath of
            Selected ->
                True
            _ ->
                False
    in
        case expr of
            Cnf.ConjValue v a b ->
                span
                    [ class ("expr complex conjunction" ++ if isHovered then " hovered" else "")
                    , onHover (path Selected)
                    ]
                    [ goLeft path hoverPath a
                    , text "∧"
                    , goRight path hoverPath b
                    , tooltip v
                    ]
            Cnf.DisjValue v a b ->
                span
                    [ class ("expr complex disjunction" ++ if isHovered then " hovered" else "")
                    , onHover (path Selected)
                    ]
                    [ goLeft path hoverPath a
                    , text "∨"
                    , goRight path hoverPath b
                    , tooltip v
                    ]
            Cnf.ImplValue v a b ->
                span
                    [ class ("expr complex implication" ++ if isHovered then " hovered" else "")
                    , onHover (path Selected)
                    ]
                    [ goLeft path hoverPath a
                    , text "⇒"
                    , goRight path hoverPath b
                    , tooltip v
                    ]
            Cnf.NegValue v e ->
                span
                    [ class ("expr negation" ++ if isHovered then " hovered" else "")
                    , onHover (path Selected)
                    ]
                    [ text "¬"
                    , goStraight path hoverPath e
                    , tooltip v
                    ]
            Cnf.SymbValue v c ->
                span
                    [ class ("expr symbol" ++ if isHovered then " hovered" else "")
                    , onHover (path Selected)
                    ]
                    [ text <| Str.fromChar c
                    , tooltip v
                    ]

onHover : Hover -> Attribute Msg
onHover path =
    onWithOptions
        "mouseover"
        { stopPropagation = True
        , preventDefault = True
        }
        (succeed <| NewHover path)
