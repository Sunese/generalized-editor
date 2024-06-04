module Conditionals exposing (..)

import Elm exposing (apply)
import Elm.Annotation as Type exposing (..)
import Elm.Case as Case exposing (..)
import Elm.Case.Branch as Branch exposing (ignore)
import Elm.Let
import Elm.Op
import Gen.Decomposable exposing (..)
import Gen.Substitutable exposing (..)
import Parser exposing (..)
import RawSyntaxP exposing (..)
import Syntax exposing (..)


createEditorCondDecls : CLessSyntax -> WellFormedSyntax -> List Elm.Declaration
createEditorCondDecls clessSyntax wellformedSyntax =
    [ createEditorCondType clessSyntax
    , createDecomposedType
    , createEvalCondFun
    , createAtOpFun clessSyntax
    , creategetOpAtCursorFun wellformedSyntax
    , createpossiblyFun
    , createNecessityFun clessSyntax
    ]
        ++ createSameOpFuns clessSyntax


sameOpFunName : String -> String
sameOpFunName syncat =
    "same_" ++ syncat


createEditorCondType : CLessSyntax -> Elm.Declaration
createEditorCondType clessSyntax =
    Elm.customType
        "EditorCond"
        [ Elm.variantWith "Neg" [ Type.named [] "EditorCond" ]
        , Elm.variantWith "Conjunction"
            [ Type.named [] "EditorCond", Type.named [] "EditorCond" ]
        , Elm.variantWith "Disjunction"
            [ Type.named [] "EditorCond", Type.named [] "EditorCond" ]
        , Elm.variantWith "At" [ Type.named [] "CursorLess" ]
        , Elm.variantWith "Possibly" [ Type.named [] "CursorLess" ]
        , Elm.variantWith "Necessarily" [ Type.named [] "CursorLess" ]
        ]


createDecomposedType : Elm.Declaration
createDecomposedType =
    Elm.alias "Decomposed"
        (Type.tuple (Type.named [] "Cctx") (Type.named [] "Wellformed"))


createEvalCondFun : Elm.Declaration
createEvalCondFun =
    Elm.declaration
        "evalCond"
    <|
        Elm.withType
            (Type.function
                [ Type.named [] "Decomposed", Type.named [] "EditorCond" ]
                Type.bool
            )
        <|
            Elm.fn2
                ( "decomposed", Just <| Type.named [] "Decomposed" )
                ( "cond", Just <| Type.named [] "EditorCond" )
            <|
                \decomposed cond ->
                    Case.custom cond
                        (Type.named [] "EditorCond")
                        [ branch1 "Neg" ( "arg1", Type.named [] "EditorCond" ) <|
                            \e ->
                                apply (Elm.val "not")
                                    [ apply
                                        (Elm.val "evalCond")
                                        [ decomposed, e ]
                                    ]
                        , branch2 "Conjunction"
                            ( "arg1", Type.named [] "EditorCond" )
                            ( "arg2", Type.named [] "EditorCond" )
                          <|
                            \e1 e2 ->
                                Elm.Op.and
                                    (apply
                                        (Elm.val "evalCond")
                                        [ decomposed, e1 ]
                                    )
                                    (apply
                                        (Elm.val "evalCond")
                                        [ decomposed, e2 ]
                                    )
                        , branch2 "Disjunction"
                            ( "arg1", Type.named [] "EditorCond" )
                            ( "arg2", Type.named [] "EditorCond" )
                          <|
                            \e1 e2 ->
                                Elm.Op.or
                                    (apply
                                        (Elm.val "evalCond")
                                        [ decomposed, e1 ]
                                    )
                                    (apply
                                        (Elm.val "evalCond")
                                        [ decomposed, e2 ]
                                    )
                        , branch1 "At"
                            ( "cursorlessOp"
                            , Type.named [] "CursorLess"
                            )
                          <|
                            \clessop ->
                                apply
                                    (Elm.val "atOp")
                                    [ clessop, Elm.just decomposed ]
                        , branch1 "Possibly"
                            ( "cursorlessOp"
                            , Type.named [] "CursorLess"
                            )
                          <|
                            \clessop ->
                                apply
                                    (Elm.val "possibly")
                                    [ clessop, Elm.just decomposed ]
                        , branch1 "Necessarily"
                            ( "cursorlessOp"
                            , Type.named [] "CursorLess"
                            )
                          <|
                            \clessop ->
                                apply
                                    (Elm.val "necessity")
                                    [ clessop, decomposed ]
                        ]


createAtOpFun : CLessSyntax -> Elm.Declaration
createAtOpFun clessSyntax =
    Elm.declaration
        "atOp"
    <|
        Elm.withType
            (Type.function
                [ Type.named [] "CursorLess", Type.maybe <| Type.named [] "Decomposed" ]
                Type.bool
            )
        <|
            Elm.fn2
                ( "cursorlessop", Just <| Type.named [] "CursorLess" )
                ( "maybedecomposed", Just <| Type.maybe <| Type.named [] "Decomposed" )
            <|
                \cursorlessop maybedecomposed ->
                    Case.custom
                        maybedecomposed
                        (Type.maybe <| Type.named [] "Decomposed")
                    <|
                        [ Branch.nothing <| Elm.val "False"
                        , Branch.variant1 "Just" (Branch.var "decomposed") <|
                            \decomposed ->
                                Case.custom
                                    (Elm.tuple
                                        cursorlessop
                                        (Elm.apply (Elm.val "getOpAtCursor") [ decomposed ])
                                    )
                                    (Type.tuple (Type.named [] "CursorLess") (Type.named [] "CursorLess"))
                                <|
                                    List.map
                                        (\syncatOp ->
                                            Branch.tuple
                                                (Branch.variant1 syncatOp.synCat (Branch.var "query") <| \_ -> Elm.val "dummy")
                                                (Branch.variant1 syncatOp.synCat (Branch.var "op") <| \_ -> Elm.val "dummy")
                                                |> Branch.map
                                                    (\( _, _ ) ->
                                                        Elm.apply (Elm.val <| sameOpFunName syncatOp.synCat)
                                                            [ Elm.val "query", Elm.val "op" ]
                                                    )
                                        )
                                        clessSyntax.synCatOps
                                        ++ [ Branch.ignore <| Elm.val "False" ]
                        ]


creategetOpAtCursorFun : WellFormedSyntax -> Elm.Declaration
creategetOpAtCursorFun wellformedSyntax =
    Elm.declaration
        "getOpAtCursor"
    <|
        Elm.withType
            (Type.function
                [ Type.named [] "Decomposed" ]
                (Type.named [] "CursorLess")
            )
        <|
            Elm.fn
                ( "decomposed", Just <| Type.named [] "Decomposed" )
            <|
                \decomposed ->
                    Elm.Let.letIn
                        (\( _, wellformed ) ->
                            Case.custom
                                wellformed
                                (Type.named [] "Wellformed")
                                (List.map
                                    (\op ->
                                        Branch.variant1 op.name (Branch.var "arg1") <|
                                            \arg ->
                                                Elm.apply
                                                    (Elm.val
                                                        (firstCharToUpper <|
                                                            String.dropLeft 5 op.name
                                                        )
                                                    )
                                                    [ arg ]
                                    )
                                    (List.concatMap (\x -> x.ops) wellformedSyntax.synCatOps)
                                )
                        )
                        |> Elm.Let.tuple "cctx" "wellformed" decomposed
                        |> Elm.Let.toExpression


createpossiblyFun : Elm.Declaration
createpossiblyFun =
    Elm.declaration
        "possibly"
    <|
        Elm.withType
            (Type.function
                [ Type.named [] "CursorLess", Type.maybe <| Type.named [] "Decomposed" ]
                Type.bool
            )
        <|
            Elm.fn2
                ( "cursorlessop", Just <| Type.named [] "CursorLess" )
                ( "maybedecomposed", Just <| Type.maybe <| Type.named [] "Decomposed" )
            <|
                \c maybed ->
                    Case.custom
                        maybed
                        (Type.maybe <| Type.named [] "Decomposed")
                    <|
                        [ Branch.nothing <| Elm.val "False"
                        , Branch.variant1 "Just" (Branch.var "decomposed") <|
                            \d ->
                                Elm.Op.or
                                    (Elm.apply (Elm.val "atOp") [ c, Elm.just d ])
                                    (Elm.Op.or
                                        (possiblychildi 1 c d)
                                        (Elm.Op.or
                                            (possiblychildi 2 c d)
                                            (Elm.Op.or
                                                (possiblychildi 3 c d)
                                                (Elm.Op.or
                                                    (possiblychildi 4 c d)
                                                    (possiblychildi 5 c d)
                                                )
                                            )
                                        )
                                    )
                        ]


possiblychildi : Int -> Elm.Expression -> Elm.Expression -> Elm.Expression
possiblychildi i cursorless decomposed =
    Elm.apply
        (Elm.val "possibly")
        [ cursorless
        , Elm.apply (Elm.val "child") [ Elm.int i, decomposed ]
        ]


createNecessityFun : CLessSyntax -> Elm.Declaration
createNecessityFun clessSyntax =
    Elm.declaration
        "necessity"
    <|
        Elm.withType
            (Type.function
                [ Type.named [] "CursorLess", Type.named [] "Decomposed" ]
                Type.bool
            )
        <|
            Elm.fn2
                ( "cursorlessop", Just <| Type.named [] "CursorLess" )
                ( "decomposed", Just <| Type.named [] "Decomposed" )
            <|
                \cursorlessop decomposed ->
                    Case.custom
                        (Elm.apply (Elm.val "getOpAtCursor") [ decomposed ])
                        (Type.named [] "CursorLess")
                        (List.map
                            (\syncatOp ->
                                Branch.variant1 syncatOp.synCat (Branch.var "arg1") <|
                                    \clesssort ->
                                        Case.custom
                                            clesssort
                                            (Type.named [] (firstCharToUpper syncatOp.synCat))
                                            (List.map
                                                (\op ->
                                                    case op.literal of
                                                        Just lit ->
                                                            Branch.variant1 op.name (Branch.ignore "dummy") <| \_ -> Elm.val "False"

                                                        Nothing ->
                                                            case List.length op.arity of
                                                                0 ->
                                                                    Branch.variant0
                                                                        op.name
                                                                        (Elm.val "False")

                                                                1 ->
                                                                    Branch.variant1
                                                                        op.name
                                                                        (Branch.ignore "dummy")
                                                                    <|
                                                                        \_ ->
                                                                            Elm.apply
                                                                                (Elm.val "possibly")
                                                                                [ cursorlessop
                                                                                , Elm.apply
                                                                                    (Elm.val "child")
                                                                                    [ Elm.int 1, Elm.val "decomposed" ]
                                                                                ]

                                                                2 ->
                                                                    Branch.variant2
                                                                        op.name
                                                                        (Branch.ignore "dummy")
                                                                        (Branch.ignore "dummy")
                                                                    <|
                                                                        \_ _ ->
                                                                            Elm.Op.and
                                                                                (Elm.apply
                                                                                    (Elm.val "possibly")
                                                                                    [ cursorlessop
                                                                                    , Elm.apply
                                                                                        (Elm.val "child")
                                                                                        [ Elm.int 1, Elm.val "decomposed" ]
                                                                                    ]
                                                                                )
                                                                                (Elm.apply
                                                                                    (Elm.val "possibly")
                                                                                    [ cursorlessop
                                                                                    , Elm.apply
                                                                                        (Elm.val "child")
                                                                                        [ Elm.int 2, Elm.val "decomposed" ]
                                                                                    ]
                                                                                )

                                                                3 ->
                                                                    Branch.variant3
                                                                        op.name
                                                                        (Branch.ignore "dummy")
                                                                        (Branch.ignore "dummy")
                                                                        (Branch.ignore "dummy")
                                                                    <|
                                                                        \_ _ _ ->
                                                                            Elm.Op.and
                                                                                (Elm.Op.and
                                                                                    (Elm.apply
                                                                                        (Elm.val "possibly")
                                                                                        [ cursorlessop
                                                                                        , Elm.apply
                                                                                            (Elm.val "child")
                                                                                            [ Elm.int 1, Elm.val "decomposed" ]
                                                                                        ]
                                                                                    )
                                                                                    (Elm.apply
                                                                                        (Elm.val "possibly")
                                                                                        [ cursorlessop
                                                                                        , Elm.apply
                                                                                            (Elm.val "child")
                                                                                            [ Elm.int 2, Elm.val "decomposed" ]
                                                                                        ]
                                                                                    )
                                                                                )
                                                                                (Elm.apply
                                                                                    (Elm.val "possibly")
                                                                                    [ cursorlessop
                                                                                    , Elm.apply
                                                                                        (Elm.val "child")
                                                                                        [ Elm.int 3, Elm.val "decomposed" ]
                                                                                    ]
                                                                                )

                                                                4 ->
                                                                    Branch.variant4
                                                                        op.name
                                                                        (Branch.ignore "dummy")
                                                                        (Branch.ignore "dummy")
                                                                        (Branch.ignore "dummy")
                                                                        (Branch.ignore "dummy")
                                                                    <|
                                                                        \_ _ _ _ ->
                                                                            Elm.Op.and
                                                                                (Elm.Op.and
                                                                                    (Elm.Op.and
                                                                                        (Elm.apply
                                                                                            (Elm.val "possibly")
                                                                                            [ cursorlessop
                                                                                            , Elm.apply
                                                                                                (Elm.val "child")
                                                                                                [ Elm.int 1, Elm.val "decomposed" ]
                                                                                            ]
                                                                                        )
                                                                                        (Elm.apply
                                                                                            (Elm.val "possibly")
                                                                                            [ cursorlessop
                                                                                            , Elm.apply
                                                                                                (Elm.val "child")
                                                                                                [ Elm.int 2, Elm.val "decomposed" ]
                                                                                            ]
                                                                                        )
                                                                                    )
                                                                                    (Elm.apply
                                                                                        (Elm.val "possibly")
                                                                                        [ cursorlessop
                                                                                        , Elm.apply
                                                                                            (Elm.val "child")
                                                                                            [ Elm.int 3, Elm.val "decomposed" ]
                                                                                        ]
                                                                                    )
                                                                                )
                                                                                (Elm.apply
                                                                                    (Elm.val "possibly")
                                                                                    [ cursorlessop
                                                                                    , Elm.apply
                                                                                        (Elm.val "child")
                                                                                        [ Elm.int 4, Elm.val "decomposed" ]
                                                                                    ]
                                                                                )

                                                                5 ->
                                                                    Branch.variant5
                                                                        op.name
                                                                        (Branch.ignore "dummy")
                                                                        (Branch.ignore "dummy")
                                                                        (Branch.ignore "dummy")
                                                                        (Branch.ignore "dummy")
                                                                        (Branch.ignore "dummy")
                                                                    <|
                                                                        \_ _ _ _ _ ->
                                                                            Elm.Op.and
                                                                                (Elm.Op.and
                                                                                    (Elm.Op.and
                                                                                        (Elm.Op.and
                                                                                            (Elm.apply
                                                                                                (Elm.val "possibly")
                                                                                                [ cursorlessop
                                                                                                , Elm.apply
                                                                                                    (Elm.val "child")
                                                                                                    [ Elm.int 1, Elm.val "decomposed" ]
                                                                                                ]
                                                                                            )
                                                                                            (Elm.apply
                                                                                                (Elm.val "possibly")
                                                                                                [ cursorlessop
                                                                                                , Elm.apply
                                                                                                    (Elm.val "child")
                                                                                                    [ Elm.int 2, Elm.val "decomposed" ]
                                                                                                ]
                                                                                            )
                                                                                        )
                                                                                        (Elm.apply
                                                                                            (Elm.val "possibly")
                                                                                            [ cursorlessop
                                                                                            , Elm.apply
                                                                                                (Elm.val "child")
                                                                                                [ Elm.int 3, Elm.val "decomposed" ]
                                                                                            ]
                                                                                        )
                                                                                    )
                                                                                    (Elm.apply
                                                                                        (Elm.val "possibly")
                                                                                        [ cursorlessop
                                                                                        , Elm.apply
                                                                                            (Elm.val "child")
                                                                                            [ Elm.int 4, Elm.val "decomposed" ]
                                                                                        ]
                                                                                    )
                                                                                )
                                                                                (Elm.apply
                                                                                    (Elm.val "possibly")
                                                                                    [ cursorlessop
                                                                                    , Elm.apply
                                                                                        (Elm.val "child")
                                                                                        [ Elm.int 5, Elm.val "decomposed" ]
                                                                                    ]
                                                                                )

                                                                _ ->
                                                                    Debug.todo "operator with arity > 5"
                                                )
                                                syncatOp.ops
                                            )
                            )
                            clessSyntax.synCatOps
                        )


createSameOpFuns : CLessSyntax -> List Elm.Declaration
createSameOpFuns clessSyntax =
    List.map
        (\syncatOp ->
            Elm.declaration
                (sameOpFunName syncatOp.synCat)
            <|
                Elm.withType
                    (Type.function
                        [ Type.named [] syncatOp.synCat, Type.named [] syncatOp.synCat ]
                        Type.bool
                    )
                <|
                    Elm.fn2
                        ( "query", Just <| Type.named [] syncatOp.synCat )
                        ( "op", Just <| Type.named [] syncatOp.synCat )
                    <|
                        \query op ->
                            Case.custom
                                (Elm.tuple query op)
                                (Type.tuple (Type.named [] syncatOp.synCat) (Type.named [] syncatOp.synCat))
                            <|
                                List.map
                                    (\clessop ->
                                        case clessop.literal of
                                            Just _ ->
                                                Branch.tuple
                                                    (Branch.variant1 clessop.name (Branch.ignore "dummy") <| \_ -> Elm.val "dummy")
                                                    (Branch.variant1 clessop.name (Branch.ignore "dummy") <| \_ -> Elm.val "dummy")
                                                    |> Branch.map
                                                        (\( _, _ ) ->
                                                            Elm.val "True"
                                                        )

                                            Nothing ->
                                                case List.length clessop.arity of
                                                    0 ->
                                                        Branch.tuple
                                                            (Branch.variant0 clessop.name (Elm.val "dummy"))
                                                            (Branch.variant0 clessop.name (Elm.val "dummy"))
                                                            |> Branch.map
                                                                (\( _, _ ) ->
                                                                    Elm.val "True"
                                                                )

                                                    1 ->
                                                        Branch.tuple
                                                            (Branch.variant1 clessop.name (Branch.ignore "dummy") <| \_ -> Elm.val "dummy")
                                                            (Branch.variant1 clessop.name (Branch.ignore "dummy") <| \_ -> Elm.val "dummy")
                                                            |> Branch.map
                                                                (\( _, _ ) ->
                                                                    Elm.val "True"
                                                                )

                                                    2 ->
                                                        Branch.tuple
                                                            (Branch.variant2 clessop.name (Branch.ignore "dummy") (Branch.ignore "dummy") <| \_ _ -> Elm.val "dummy")
                                                            (Branch.variant2 clessop.name (Branch.ignore "dummy") (Branch.ignore "dummy") <| \_ _ -> Elm.val "dummy")
                                                            |> Branch.map
                                                                (\( _, _ ) ->
                                                                    Elm.val "True"
                                                                )

                                                    3 ->
                                                        Branch.tuple
                                                            (Branch.variant3 clessop.name (Branch.ignore "dummy") (Branch.ignore "dummy") (Branch.ignore "dummy") <| \_ _ _ -> Elm.val "dummy")
                                                            (Branch.variant3 clessop.name (Branch.ignore "dummy") (Branch.ignore "dummy") (Branch.ignore "dummy") <| \_ _ _ -> Elm.val "dummy")
                                                            |> Branch.map
                                                                (\( _, _ ) ->
                                                                    Elm.val "True"
                                                                )

                                                    4 ->
                                                        Branch.tuple
                                                            (Branch.variant4 clessop.name (Branch.ignore "dummy") (Branch.ignore "dummy") (Branch.ignore "dummy") (Branch.ignore "dummy") <| \_ _ _ _ -> Elm.val "dummy")
                                                            (Branch.variant4 clessop.name (Branch.ignore "dummy") (Branch.ignore "dummy") (Branch.ignore "dummy") (Branch.ignore "dummy") <| \_ _ _ _ -> Elm.val "dummy")
                                                            |> Branch.map
                                                                (\( _, _ ) ->
                                                                    Elm.val "True"
                                                                )

                                                    5 ->
                                                        Branch.tuple
                                                            (Branch.variant5 clessop.name (Branch.ignore "dummy") (Branch.ignore "dummy") (Branch.ignore "dummy") (Branch.ignore "dummy") (Branch.ignore "dummy") <| \_ _ _ _ _ -> Elm.val "dummy")
                                                            (Branch.variant5 clessop.name (Branch.ignore "dummy") (Branch.ignore "dummy") (Branch.ignore "dummy") (Branch.ignore "dummy") (Branch.ignore "dummy") <| \_ _ _ _ _ -> Elm.val "dummy")
                                                            |> Branch.map
                                                                (\( _, _ ) ->
                                                                    Elm.val "True"
                                                                )

                                                    _ ->
                                                        Debug.todo "operator with arity > 5"
                                    )
                                    syncatOp.ops
                                    ++ [ Branch.ignore <| Elm.val "False" ]
        )
        clessSyntax.synCatOps
