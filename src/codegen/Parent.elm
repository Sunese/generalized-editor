module Parent exposing (..)

import Array
import Elm
import Elm.Annotation as Type exposing (..)
import Elm.Case exposing (..)
import Elm.Case.Branch as Branch exposing (ignore)
import Elm.Let
import Elm.Op
import Elm.ToString
import Gen.Decomposable exposing (..)
import Gen.Dict exposing (remove)
import Gen.Substitutable exposing (..)
import Html exposing (a)
import Parser exposing (..)
import RawSyntaxP exposing (..)
import String exposing (replace)
import Syntax exposing (..)


createParentFuns : CLessSyntax -> CCtxSyntax -> List Elm.Declaration
createParentFuns clessSyntax cctxSyntax =
    [ createGetCctxPathFun cctxSyntax
    , createMoveCctxHoleUpFun cctxSyntax
    , createAddParentFun clessSyntax cctxSyntax
    , createParentFun
    ]


createParentFun : Elm.Declaration
createParentFun =
    Elm.declaration "parent" <|
        Elm.withType
            (Type.function
                [ Type.tuple (Type.named [] "Cctx") (Type.named [] "Wellformed")
                ]
                (Type.maybe <| Type.tuple (Type.named [] "Cctx") (Type.named [] "Wellformed"))
            )
        <|
            Elm.fn
                ( "decomposed", Just <| Type.tuple (Type.named [] "Cctx") (Type.named [] "Wellformed") )
                (\decomposed ->
                    Elm.Let.letIn
                        (\( cctx, wellformed ) ->
                            Elm.Case.custom
                                (Elm.apply
                                    (Elm.val "moveCCtxHoleUp")
                                    [ cctx
                                    , Elm.apply (Elm.val "getCctxPath")
                                        [ cctx
                                        , Elm.list []
                                        ]
                                    ]
                                )
                                (Type.maybe <| Type.tuple (Type.named [] "Cctx") (Type.named [] "Wellformed"))
                                [ Branch.variant0 "Nothing" <| Elm.nothing
                                , Branch.variant1
                                    "Just"
                                    (Branch.tuple (Branch.var "newCctx") (Branch.var "removedCctx"))
                                  <|
                                    \( newcctx, removedCCtx ) ->
                                        Elm.Case.custom
                                            (Elm.apply
                                                (Elm.val "addParent")
                                                [ Elm.val "removedCctx"
                                                , Elm.val "wellformed"
                                                ]
                                            )
                                            (Type.maybe <| Type.named [] "Wellformed")
                                            [ Branch.variant0 "Nothing" <| Elm.nothing
                                            , Branch.variant1
                                                "Just"
                                                (Branch.var "newWellformed")
                                              <|
                                                \newWellformed -> Elm.just <| Elm.tuple newcctx newWellformed
                                            ]
                                ]
                        )
                        |> Elm.Let.tuple "cctx" "wellformed" (Elm.val "decomposed")
                        |> Elm.Let.toExpression
                )


createAddParentFun : CLessSyntax -> CCtxSyntax -> Elm.Declaration
createAddParentFun clessSyntax cctxSyntax =
    Elm.declaration "addParent" <|
        Elm.withType
            (Type.function
                [ Type.named [] "Cctx"
                , Type.named [] "Wellformed"
                ]
                (Type.maybe <| Type.named [] "Wellformed")
            )
        <|
            Elm.fn2
                ( "cctx", Just <| Type.named [] "Cctx" )
                ( "wellformed", Just <| Type.named [] "Wellformed" )
                (\cctx wellformed ->
                    Elm.Case.custom
                        cctx
                        (Type.named [] "Cctx")
                        (List.map
                            (\cctxop ->
                                let
                                    patterns =
                                        List.indexedMap
                                            (\i ( boundVars, _ ) ->
                                                if List.isEmpty boundVars then
                                                    "arg" ++ String.fromInt (i + 1)

                                                else
                                                    "(boundVars" ++ String.fromInt (i + 1) ++ ", arg" ++ String.fromInt (i + 1) ++ ")"
                                            )
                                            cctxop.arity

                                    patternsArray =
                                        Array.fromList patterns

                                    clessArgsArray =
                                        Array.fromList <| List.map Tuple.second <| .arity <| getCLessEquivalentCCtxOp cctxop clessSyntax

                                    argsArray =
                                        Array.fromList <| List.map Tuple.second cctxop.arity

                                    annotationArray =
                                        argsAnnotation cctxop.arity

                                    replacePatternsArray =
                                        Array.fromList <|
                                            List.indexedMap
                                                (\i ( boundVars, _ ) ->
                                                    if List.isEmpty boundVars then
                                                        Elm.val <| "underCursor"

                                                    else
                                                        Elm.val <| "(boundVars" ++ String.fromInt (i + 1) ++ ", underCursor )"
                                                )
                                                cctxop.arity
                                in
                                case List.length cctxop.arity of
                                    0 ->
                                        branch0 cctxop.name <| Elm.nothing

                                    1 ->
                                        branch1 cctxop.name ( getPattern 0 patternsArray, getAnnotation 0 annotationArray ) <|
                                            \_ ->
                                                case getCCtxNum cctxop of
                                                    1 ->
                                                        Elm.Case.custom
                                                            wellformed
                                                            (Type.named [] "Wellformed")
                                                            [ branch1
                                                                ("Root_" ++ getArg 0 clessArgsArray)
                                                                ( "underCursor", Type.named [] <| firstCharToUpper <| getArg 0 argsArray )
                                                              <|
                                                                \undercursor ->
                                                                    Elm.just <|
                                                                        Elm.withType
                                                                            (Type.named [] "Wellformed")
                                                                            (Elm.apply
                                                                                (Elm.val <| "Root_" ++ getCLessSortOfCCtxOp clessSyntax cctxop)
                                                                                [ Elm.apply (Elm.val <| getCLessEquivalentCCtxOpName cctxop)
                                                                                    [ getPatternExp 0 replacePatternsArray ]
                                                                                ]
                                                                            )
                                                            , ignore <| Elm.nothing
                                                            ]

                                                    _ ->
                                                        Debug.todo "invalid cctx number"

                                    2 ->
                                        branch2
                                            cctxop.name
                                            ( getPattern 0 patternsArray, getAnnotation 0 annotationArray )
                                            ( getPattern 1 patternsArray, getAnnotation 1 annotationArray )
                                        <|
                                            \arg1 arg2 ->
                                                case getCCtxNum cctxop of
                                                    1 ->
                                                        Elm.Case.custom
                                                            wellformed
                                                            (Type.named [] "Wellformed")
                                                            [ branch1
                                                                ("Root_" ++ getArg 0 clessArgsArray)
                                                                ( "underCursor", Type.named [] <| firstCharToUpper <| getArg 0 clessArgsArray )
                                                              <|
                                                                \undercursor ->
                                                                    Elm.just <|
                                                                        Elm.withType
                                                                            (Type.named [] "Wellformed")
                                                                            (Elm.apply
                                                                                (Elm.val <| "Root_" ++ getCLessSortOfCCtxOp clessSyntax cctxop)
                                                                                [ Elm.apply (Elm.val <| getCLessEquivalentCCtxOpName cctxop)
                                                                                    [ getPatternExp 0 replacePatternsArray
                                                                                    , getPatternExp_ 1 patternsArray
                                                                                    ]
                                                                                ]
                                                                            )
                                                            , ignore <| Elm.nothing
                                                            ]

                                                    2 ->
                                                        Elm.Case.custom
                                                            wellformed
                                                            (Type.named [] "Wellformed")
                                                            [ branch1
                                                                ("Root_" ++ getArg 1 clessArgsArray)
                                                                ( "underCursor", Type.named [] <| firstCharToUpper <| getArg 1 clessArgsArray )
                                                              <|
                                                                \undercursor ->
                                                                    Elm.just <|
                                                                        Elm.withType
                                                                            (Type.named [] "Wellformed")
                                                                            (Elm.apply
                                                                                (Elm.val <| "Root_" ++ getCLessSortOfCCtxOp clessSyntax cctxop)
                                                                                [ Elm.apply (Elm.val <| getCLessEquivalentCCtxOpName cctxop)
                                                                                    [ getPatternExp_ 0 patternsArray
                                                                                    , getPatternExp 1 replacePatternsArray
                                                                                    ]
                                                                                ]
                                                                            )
                                                            , ignore <| Elm.nothing
                                                            ]

                                                    _ ->
                                                        Elm.val "todo"

                                    3 ->
                                        branch3 cctxop.name
                                            ( getPattern 0 patternsArray, getAnnotation 0 annotationArray )
                                            ( getPattern 1 patternsArray, getAnnotation 1 annotationArray )
                                            ( getPattern 2 patternsArray, getAnnotation 2 annotationArray )
                                        <|
                                            \arg1 arg2 arg3 ->
                                                case getCCtxNum cctxop of
                                                    1 ->
                                                        Elm.Case.custom
                                                            wellformed
                                                            (Type.named [] "Wellformed")
                                                            [ branch1
                                                                ("Root_" ++ getArg 0 clessArgsArray)
                                                                ( "underCursor", Type.named [] <| firstCharToUpper <| getArg 0 clessArgsArray )
                                                              <|
                                                                \undercursor ->
                                                                    Elm.just <|
                                                                        Elm.withType
                                                                            (Type.named [] "Wellformed")
                                                                            (Elm.apply
                                                                                (Elm.val <| "Root_" ++ getCLessSortOfCCtxOp clessSyntax cctxop)
                                                                                [ Elm.apply (Elm.val <| getCLessEquivalentCCtxOpName cctxop)
                                                                                    [ getPatternExp 0 replacePatternsArray
                                                                                    , getPatternExp_ 1 patternsArray
                                                                                    , getPatternExp_ 2 patternsArray
                                                                                    ]
                                                                                ]
                                                                            )
                                                            , ignore <| Elm.nothing
                                                            ]

                                                    2 ->
                                                        Elm.Case.custom
                                                            wellformed
                                                            (Type.named [] "Wellformed")
                                                            [ branch1
                                                                ("Root_" ++ getArg 1 clessArgsArray)
                                                                ( "underCursor", Type.named [] <| firstCharToUpper <| getArg 1 clessArgsArray )
                                                              <|
                                                                \undercursor ->
                                                                    Elm.just <|
                                                                        Elm.withType
                                                                            (Type.named [] "Wellformed")
                                                                            (Elm.apply
                                                                                (Elm.val <| "Root_" ++ getCLessSortOfCCtxOp clessSyntax cctxop)
                                                                                [ Elm.apply (Elm.val <| getCLessEquivalentCCtxOpName cctxop)
                                                                                    [ getPatternExp_ 0 patternsArray
                                                                                    , getPatternExp 1 replacePatternsArray
                                                                                    , getPatternExp_ 2 patternsArray
                                                                                    ]
                                                                                ]
                                                                            )
                                                            , ignore <| Elm.nothing
                                                            ]

                                                    3 ->
                                                        Elm.Case.custom
                                                            wellformed
                                                            (Type.named [] "Wellformed")
                                                            [ branch1
                                                                ("Root_" ++ getArg 2 clessArgsArray)
                                                                ( "underCursor", Type.named [] <| firstCharToUpper <| getArg 1 clessArgsArray )
                                                              <|
                                                                \undercursor ->
                                                                    Elm.just <|
                                                                        Elm.withType
                                                                            (Type.named [] "Wellformed")
                                                                            (Elm.apply
                                                                                (Elm.val <| "Root_" ++ getCLessSortOfCCtxOp clessSyntax cctxop)
                                                                                [ Elm.apply (Elm.val <| getCLessEquivalentCCtxOpName cctxop)
                                                                                    [ getPatternExp_ 0 patternsArray
                                                                                    , getPatternExp_ 1 patternsArray
                                                                                    , getPatternExp 2 replacePatternsArray
                                                                                    ]
                                                                                ]
                                                                            )
                                                            , ignore <| Elm.nothing
                                                            ]

                                                    _ ->
                                                        Debug.todo "invalid cctx number"

                                    4 ->
                                        branch4 cctxop.name
                                            ( getPattern 0 patternsArray, getAnnotation 0 annotationArray )
                                            ( getPattern 1 patternsArray, getAnnotation 1 annotationArray )
                                            ( getPattern 2 patternsArray, getAnnotation 2 annotationArray )
                                            ( getPattern 3 patternsArray, getAnnotation 3 annotationArray )
                                        <|
                                            \arg1 arg2 arg3 arg4 ->
                                                case getCCtxNum cctxop of
                                                    1 ->
                                                        Elm.Case.custom
                                                            wellformed
                                                            (Type.named [] "Wellformed")
                                                            [ branch1
                                                                ("Root_" ++ getArg 0 clessArgsArray)
                                                                ( "underCursor", Type.named [] <| firstCharToUpper <| getArg 0 clessArgsArray )
                                                              <|
                                                                \undercursor ->
                                                                    Elm.just <|
                                                                        Elm.withType
                                                                            (Type.named [] "Wellformed")
                                                                            (Elm.apply
                                                                                (Elm.val <| "Root_" ++ getCLessSortOfCCtxOp clessSyntax cctxop)
                                                                                [ Elm.apply (Elm.val <| getCLessEquivalentCCtxOpName cctxop)
                                                                                    [ getPatternExp 0 replacePatternsArray
                                                                                    , getPatternExp_ 1 patternsArray
                                                                                    , getPatternExp_ 2 patternsArray
                                                                                    , getPatternExp_ 3 patternsArray
                                                                                    ]
                                                                                ]
                                                                            )
                                                            , ignore <| Elm.nothing
                                                            ]

                                                    2 ->
                                                        Elm.Case.custom
                                                            wellformed
                                                            (Type.named [] "Wellformed")
                                                            [ branch1
                                                                ("Root_" ++ getArg 1 clessArgsArray)
                                                                ( "underCursor", Type.named [] <| firstCharToUpper <| getArg 1 clessArgsArray )
                                                              <|
                                                                \undercursor ->
                                                                    Elm.just <|
                                                                        Elm.withType
                                                                            (Type.named [] "Wellformed")
                                                                            (Elm.apply
                                                                                (Elm.val <| "Root_" ++ getCLessSortOfCCtxOp clessSyntax cctxop)
                                                                                [ Elm.apply (Elm.val <| getCLessEquivalentCCtxOpName cctxop)
                                                                                    [ getPatternExp_ 0 patternsArray
                                                                                    , getPatternExp 1 replacePatternsArray
                                                                                    , getPatternExp_ 2 patternsArray
                                                                                    , getPatternExp_ 3 patternsArray
                                                                                    ]
                                                                                ]
                                                                            )
                                                            , ignore <| Elm.nothing
                                                            ]

                                                    3 ->
                                                        Elm.Case.custom
                                                            wellformed
                                                            (Type.named [] "Wellformed")
                                                            [ branch1
                                                                ("Root_" ++ getArg 2 clessArgsArray)
                                                                ( "underCursor", Type.named [] <| firstCharToUpper <| getArg 1 clessArgsArray )
                                                              <|
                                                                \undercursor ->
                                                                    Elm.just <|
                                                                        Elm.withType
                                                                            (Type.named [] "Wellformed")
                                                                            (Elm.apply
                                                                                (Elm.val <| "Root_" ++ getCLessSortOfCCtxOp clessSyntax cctxop)
                                                                                [ Elm.apply (Elm.val <| getCLessEquivalentCCtxOpName cctxop)
                                                                                    [ getPatternExp_ 0 patternsArray
                                                                                    , getPatternExp_ 1 patternsArray
                                                                                    , getPatternExp 2 replacePatternsArray
                                                                                    , getPatternExp_ 3 patternsArray
                                                                                    ]
                                                                                ]
                                                                            )
                                                            , ignore <| Elm.nothing
                                                            ]

                                                    4 ->
                                                        Elm.Case.custom
                                                            wellformed
                                                            (Type.named [] "Wellformed")
                                                            [ branch1
                                                                ("Root_" ++ getArg 3 clessArgsArray)
                                                                ( "underCursor", Type.named [] <| firstCharToUpper <| getArg 1 clessArgsArray )
                                                              <|
                                                                \undercursor ->
                                                                    Elm.just <|
                                                                        Elm.withType
                                                                            (Type.named [] "Wellformed")
                                                                            (Elm.apply
                                                                                (Elm.val <| "Root_" ++ getCLessSortOfCCtxOp clessSyntax cctxop)
                                                                                [ Elm.apply (Elm.val <| getCLessEquivalentCCtxOpName cctxop)
                                                                                    [ getPatternExp_ 0 patternsArray
                                                                                    , getPatternExp_ 1 patternsArray
                                                                                    , getPatternExp_ 2 patternsArray
                                                                                    , getPatternExp 3 replacePatternsArray
                                                                                    ]
                                                                                ]
                                                                            )
                                                            , ignore <| Elm.nothing
                                                            ]

                                                    _ ->
                                                        Debug.todo "invalid cctx number"

                                    5 ->
                                        branch5 cctxop.name
                                            ( getPattern 0 patternsArray, getAnnotation 0 annotationArray )
                                            ( getPattern 1 patternsArray, getAnnotation 1 annotationArray )
                                            ( getPattern 2 patternsArray, getAnnotation 2 annotationArray )
                                            ( getPattern 3 patternsArray, getAnnotation 3 annotationArray )
                                            ( getPattern 4 patternsArray, getAnnotation 4 annotationArray )
                                        <|
                                            \arg1 arg2 arg3 arg4 arg5 ->
                                                case getCCtxNum cctxop of
                                                    1 ->
                                                        Elm.Case.custom
                                                            wellformed
                                                            (Type.named [] "Wellformed")
                                                            [ branch1
                                                                ("Root_" ++ getArg 0 clessArgsArray)
                                                                ( "underCursor", Type.named [] <| firstCharToUpper <| getArg 0 clessArgsArray )
                                                              <|
                                                                \undercursor ->
                                                                    Elm.just <|
                                                                        Elm.withType
                                                                            (Type.named [] "Wellformed")
                                                                            (Elm.apply
                                                                                (Elm.val <| "Root_" ++ getCLessSortOfCCtxOp clessSyntax cctxop)
                                                                                [ Elm.apply (Elm.val <| getCLessEquivalentCCtxOpName cctxop)
                                                                                    [ getPatternExp 0 replacePatternsArray
                                                                                    , getPatternExp_ 1 patternsArray
                                                                                    , getPatternExp_ 2 patternsArray
                                                                                    , getPatternExp_ 3 patternsArray
                                                                                    , getPatternExp_ 4 patternsArray
                                                                                    ]
                                                                                ]
                                                                            )
                                                            , ignore <| Elm.nothing
                                                            ]

                                                    2 ->
                                                        Elm.Case.custom
                                                            wellformed
                                                            (Type.named [] "Wellformed")
                                                            [ branch1
                                                                ("Root_" ++ getArg 1 clessArgsArray)
                                                                ( "underCursor", Type.named [] <| firstCharToUpper <| getArg 1 clessArgsArray )
                                                              <|
                                                                \undercursor ->
                                                                    Elm.just <|
                                                                        Elm.withType
                                                                            (Type.named [] "Wellformed")
                                                                            (Elm.apply
                                                                                (Elm.val <| "Root_" ++ getCLessSortOfCCtxOp clessSyntax cctxop)
                                                                                [ Elm.apply (Elm.val <| getCLessEquivalentCCtxOpName cctxop)
                                                                                    [ getPatternExp_ 0 patternsArray
                                                                                    , getPatternExp 1 replacePatternsArray
                                                                                    , getPatternExp_ 2 patternsArray
                                                                                    , getPatternExp_ 3 patternsArray
                                                                                    , getPatternExp_ 4 patternsArray
                                                                                    ]
                                                                                ]
                                                                            )
                                                            , ignore <| Elm.nothing
                                                            ]

                                                    3 ->
                                                        Elm.Case.custom
                                                            wellformed
                                                            (Type.named [] "Wellformed")
                                                            [ branch1
                                                                ("Root_" ++ getArg 2 clessArgsArray)
                                                                ( "underCursor", Type.named [] <| firstCharToUpper <| getArg 1 clessArgsArray )
                                                              <|
                                                                \undercursor ->
                                                                    Elm.just <|
                                                                        Elm.withType
                                                                            (Type.named [] "Wellformed")
                                                                            (Elm.apply
                                                                                (Elm.val <| "Root_" ++ getCLessSortOfCCtxOp clessSyntax cctxop)
                                                                                [ Elm.apply (Elm.val <| getCLessEquivalentCCtxOpName cctxop)
                                                                                    [ getPatternExp_ 0 patternsArray
                                                                                    , getPatternExp_ 1 patternsArray
                                                                                    , getPatternExp 2 replacePatternsArray
                                                                                    , getPatternExp_ 3 patternsArray
                                                                                    , getPatternExp_ 4 patternsArray
                                                                                    ]
                                                                                ]
                                                                            )
                                                            , ignore <| Elm.nothing
                                                            ]

                                                    4 ->
                                                        Elm.Case.custom
                                                            wellformed
                                                            (Type.named [] "Wellformed")
                                                            [ branch1
                                                                ("Root_" ++ getArg 3 clessArgsArray)
                                                                ( "underCursor", Type.named [] <| firstCharToUpper <| getArg 1 clessArgsArray )
                                                              <|
                                                                \undercursor ->
                                                                    Elm.just <|
                                                                        Elm.withType
                                                                            (Type.named [] "Wellformed")
                                                                            (Elm.apply
                                                                                (Elm.val <| "Root_" ++ getCLessSortOfCCtxOp clessSyntax cctxop)
                                                                                [ Elm.apply (Elm.val <| getCLessEquivalentCCtxOpName cctxop)
                                                                                    [ getPatternExp_ 0 patternsArray
                                                                                    , getPatternExp_ 1 patternsArray
                                                                                    , getPatternExp_ 2 patternsArray
                                                                                    , getPatternExp 3 replacePatternsArray
                                                                                    , getPatternExp_ 4 patternsArray
                                                                                    ]
                                                                                ]
                                                                            )
                                                            , ignore <| Elm.nothing
                                                            ]

                                                    5 ->
                                                        Elm.Case.custom
                                                            wellformed
                                                            (Type.named [] "Wellformed")
                                                            [ branch1
                                                                ("Root_" ++ getArg 4 clessArgsArray)
                                                                ( "underCursor", Type.named [] <| firstCharToUpper <| getArg 1 clessArgsArray )
                                                              <|
                                                                \undercursor ->
                                                                    Elm.just <|
                                                                        Elm.withType
                                                                            (Type.named [] "Wellformed")
                                                                            (Elm.apply
                                                                                (Elm.val <| "Root_" ++ getCLessSortOfCCtxOp clessSyntax cctxop)
                                                                                [ Elm.apply (Elm.val <| getCLessEquivalentCCtxOpName cctxop)
                                                                                    [ getPatternExp_ 0 patternsArray
                                                                                    , getPatternExp_ 1 patternsArray
                                                                                    , getPatternExp_ 2 patternsArray
                                                                                    , getPatternExp_ 3 patternsArray
                                                                                    , getPatternExp 4 replacePatternsArray
                                                                                    ]
                                                                                ]
                                                                            )
                                                            , ignore <| Elm.nothing
                                                            ]

                                                    _ ->
                                                        Debug.todo "invalid cctx number"

                                    _ ->
                                        Debug.todo "operator with more than 5 args not supported"
                            )
                            (List.concatMap .ops <| cctxSyntax.synCatOps)
                        )
                )


getCLessEquivalentCCtxOpName : Operator -> String
getCLessEquivalentCCtxOpName cctxOp =
    firstCharToUpper <| String.dropRight 6 cctxOp.name


getCLessEquivalentCCtxOp : Operator -> CLessSyntax -> Operator
getCLessEquivalentCCtxOp cctxOp clessSyntax =
    let
        clessOpName =
            String.dropRight 6 cctxOp.name
    in
    Maybe.withDefault
        { term = "ERROR"
        , arity = []
        , name = "ERROR"
        , synCat = "ERROR"
        }
    <|
        List.head <|
            List.filter (\op -> op.name == clessOpName) (List.concatMap .ops <| clessSyntax.synCatOps)


getCLessSortOfCCtxOp : CLessSyntax -> Operator -> String
getCLessSortOfCCtxOp clessSyntax cctxOp =
    let
        baseSyncat =
            List.head <|
                List.map .synCat <|
                    List.filter
                        (\op -> op.name == String.dropRight 6 cctxOp.name)
                        (List.concatMap .ops <| clessSyntax.synCatOps)
    in
    Maybe.withDefault "ERROR" <| baseSyncat


createGetCctxPathFun : CCtxSyntax -> Elm.Declaration
createGetCctxPathFun cctxSyntax =
    Elm.declaration "getCctxPath" <|
        Elm.withType
            (Type.function
                [ Type.named [] "Cctx"
                , Type.list Type.int
                ]
                (Type.list Type.int)
            )
        <|
            Elm.fn2
                ( "cctx", Just <| Type.named [] "Cctx" )
                ( "path", Just <| Type.list Type.int )
                (\cctx path ->
                    Elm.Case.custom
                        cctx
                        (Type.named [] "Cctx")
                        (List.map
                            (\cctxop ->
                                let
                                    patterns =
                                        List.indexedMap
                                            (\i ( boundVars, _ ) ->
                                                if List.isEmpty boundVars then
                                                    "arg" ++ String.fromInt (i + 1)

                                                else
                                                    "(boundVars" ++ String.fromInt (i + 1) ++ ", arg" ++ String.fromInt (i + 1) ++ ")"
                                            )
                                            cctxop.arity

                                    patternsArray =
                                        Array.fromList patterns

                                    annotationArray =
                                        argsAnnotation cctxop.arity
                                in
                                case List.length cctxop.arity of
                                    0 ->
                                        branch0 cctxop.name <| path

                                    1 ->
                                        branch1 cctxop.name ( getPattern 0 patternsArray, getAnnotation 0 annotationArray ) <|
                                            \arg1 ->
                                                Elm.apply
                                                    (Elm.val "getCctxPath")
                                                    [ Elm.val <| "arg" ++ getCCtxNumAsStr cctxop
                                                    , Elm.Op.append path <| Elm.list [ Elm.int <| getCCtxNum cctxop ]
                                                    ]

                                    2 ->
                                        branch2
                                            cctxop.name
                                            ( getPattern 0 patternsArray, getAnnotation 0 annotationArray )
                                            ( getPattern 1 patternsArray, getAnnotation 1 annotationArray )
                                        <|
                                            \arg1 arg2 ->
                                                Elm.apply
                                                    (Elm.val "getCctxPath")
                                                    [ Elm.val <| "arg" ++ getCCtxNumAsStr cctxop
                                                    , Elm.Op.append path <| Elm.list [ Elm.int <| getCCtxNum cctxop ]
                                                    ]

                                    3 ->
                                        branch3
                                            cctxop.name
                                            ( getPattern 0 patternsArray, getAnnotation 0 annotationArray )
                                            ( getPattern 1 patternsArray, getAnnotation 1 annotationArray )
                                            ( getPattern 2 patternsArray, getAnnotation 2 annotationArray )
                                        <|
                                            \arg1 arg2 arg3 ->
                                                Elm.apply
                                                    (Elm.val "getCctxPath")
                                                    [ Elm.val <| "arg" ++ getCCtxNumAsStr cctxop
                                                    , Elm.Op.append path <| Elm.list [ Elm.int <| getCCtxNum cctxop ]
                                                    ]

                                    4 ->
                                        branch4
                                            cctxop.name
                                            ( getPattern 0 patternsArray, getAnnotation 0 annotationArray )
                                            ( getPattern 1 patternsArray, getAnnotation 1 annotationArray )
                                            ( getPattern 2 patternsArray, getAnnotation 2 annotationArray )
                                            ( getPattern 3 patternsArray, getAnnotation 3 annotationArray )
                                        <|
                                            \arg1 arg2 arg3 arg4 ->
                                                Elm.apply
                                                    (Elm.val "getCctxPath")
                                                    [ Elm.val <| "arg" ++ getCCtxNumAsStr cctxop
                                                    , Elm.Op.append path <| Elm.list [ Elm.int <| getCCtxNum cctxop ]
                                                    ]

                                    5 ->
                                        branch5
                                            cctxop.name
                                            ( getPattern 0 patternsArray, getAnnotation 0 annotationArray )
                                            ( getPattern 1 patternsArray, getAnnotation 1 annotationArray )
                                            ( getPattern 2 patternsArray, getAnnotation 2 annotationArray )
                                            ( getPattern 3 patternsArray, getAnnotation 3 annotationArray )
                                            ( getPattern 4 patternsArray, getAnnotation 4 annotationArray )
                                        <|
                                            \arg1 arg2 arg3 arg4 arg5 ->
                                                Elm.apply
                                                    (Elm.val "getCctxPath")
                                                    [ Elm.val <| "arg" ++ getCCtxNumAsStr cctxop
                                                    , Elm.Op.append path <| Elm.list [ Elm.int <| getCCtxNum cctxop ]
                                                    ]

                                    _ ->
                                        Debug.todo "operator with more than 5 args not supported"
                            )
                            (List.concatMap .ops <| cctxSyntax.synCatOps)
                        )
                )


createMoveCctxHoleUpFun : CCtxSyntax -> Elm.Declaration
createMoveCctxHoleUpFun cctxSyntax =
    Elm.declaration "moveCCtxHoleUp" <|
        Elm.withType
            (Type.function
                [ Type.named [] "Cctx"
                , Type.list Type.int
                ]
                (Type.maybe <| Type.tuple (Type.named [] "Cctx") (Type.named [] "Cctx"))
            )
        <|
            Elm.fn2
                ( "cctx", Just <| Type.named [] "Cctx" )
                ( "path", Just <| Type.list Type.int )
                (\cctx path ->
                    Elm.Case.custom
                        path
                        (Type.list Type.int)
                        [ branch0 "[_,_]" <|
                            moveCCtxParentCases cctx cctxSyntax
                        , branch0 "[_]" <|
                            Elm.just <|
                                Elm.tuple (Elm.val "Cctx_hole") cctx
                        , branch0 "_ :: rest" <| moveCCtxRestCases cctx cctxSyntax
                        , branchList 0 <|
                            \xs -> Elm.nothing
                        ]
                )


moveCCtxRestCases : Elm.Expression -> CCtxSyntax -> Elm.Expression
moveCCtxRestCases cctx cctxSyntax =
    custom
        cctx
        (Type.named [] "Cctx")
        (List.map
            (\cctxop ->
                let
                    patterns =
                        List.indexedMap
                            (\i ( boundVars, _ ) ->
                                if List.isEmpty boundVars then
                                    "arg" ++ String.fromInt (i + 1)

                                else
                                    "(boundVars" ++ String.fromInt (i + 1) ++ ", arg" ++ String.fromInt (i + 1) ++ ")"
                            )
                            cctxop.arity

                    replacePatterns =
                        List.indexedMap
                            (\i ( boundVars, _ ) ->
                                if List.isEmpty boundVars then
                                    Elm.val <| "newCctx"

                                else
                                    Elm.val <| "(boundVars" ++ String.fromInt (i + 1) ++ ", newCctx )"
                            )
                            cctxop.arity

                    replacePatternsArray =
                        Array.fromList replacePatterns

                    patternsArray =
                        Array.fromList patterns

                    annotationArray =
                        argsAnnotation cctxop.arity
                in
                case List.length cctxop.arity of
                    0 ->
                        branch0 cctxop.name <| Elm.nothing

                    1 ->
                        branch1 cctxop.name ( getPattern 0 patternsArray, getAnnotation 0 annotationArray ) <|
                            \arg1 ->
                                Elm.Op.pipe
                                    (Elm.apply maybeMap
                                        [ Elm.fn
                                            ( "(newCctx, removedCctx)", Just <| Type.tuple (Type.named [] "Cctx") (Type.named [] "Cctx") )
                                          <|
                                            \res ->
                                                Elm.tuple
                                                    (Elm.apply (Elm.val <| firstCharToUpper cctxop.name)
                                                        [ Elm.val "newCctx" ]
                                                    )
                                                    (Elm.val "removedCctx")
                                        ]
                                    )
                                    (Elm.apply
                                        (Elm.val "moveCCtxHoleUp")
                                        [ Elm.val "arg1", Elm.val "rest" ]
                                    )

                    2 ->
                        branch2
                            cctxop.name
                            ( getPattern 0 patternsArray, getAnnotation 0 annotationArray )
                            ( getPattern 1 patternsArray, getAnnotation 1 annotationArray )
                        <|
                            \arg1 arg2 ->
                                case getCCtxNum cctxop of
                                    1 ->
                                        Elm.Op.pipe
                                            (Elm.apply maybeMap
                                                [ Elm.fn
                                                    ( "(newCctx, removedCctx)", Just <| Type.tuple (Type.named [] "Cctx") (Type.named [] "Cctx") )
                                                  <|
                                                    \res ->
                                                        Elm.tuple
                                                            (Elm.apply (Elm.val <| firstCharToUpper cctxop.name)
                                                                [ getPatternExp 0 replacePatternsArray
                                                                , arg2
                                                                ]
                                                            )
                                                            (Elm.val "removedCctx")
                                                ]
                                            )
                                            (Elm.apply
                                                (Elm.val "moveCCtxHoleUp")
                                                [ Elm.val "arg1", Elm.val "rest" ]
                                            )

                                    2 ->
                                        Elm.Op.pipe
                                            (Elm.apply maybeMap
                                                [ Elm.fn
                                                    ( "(newCctx, removedCctx)", Just <| Type.tuple (Type.named [] "Cctx") (Type.named [] "Cctx") )
                                                  <|
                                                    \res ->
                                                        Elm.tuple
                                                            (Elm.apply (Elm.val <| firstCharToUpper cctxop.name)
                                                                [ arg1
                                                                , getPatternExp 1 replacePatternsArray
                                                                ]
                                                            )
                                                            (Elm.val "removedCctx")
                                                ]
                                            )
                                            (Elm.apply
                                                (Elm.val "moveCCtxHoleUp")
                                                [ Elm.val "arg2", Elm.val "rest" ]
                                            )

                                    _ ->
                                        Debug.todo "invalid cctx number"

                    3 ->
                        branch3
                            cctxop.name
                            ( getPattern 0 patternsArray, getAnnotation 0 annotationArray )
                            ( getPattern 1 patternsArray, getAnnotation 1 annotationArray )
                            ( getPattern 2 patternsArray, getAnnotation 2 annotationArray )
                        <|
                            \arg1 arg2 arg3 ->
                                case getCCtxNum cctxop of
                                    1 ->
                                        Elm.Op.pipe
                                            (Elm.apply maybeMap
                                                [ Elm.fn
                                                    ( "(newCctx, removedCctx)", Just <| Type.tuple (Type.named [] "Cctx") (Type.named [] "Cctx") )
                                                  <|
                                                    \res ->
                                                        Elm.tuple
                                                            (Elm.apply (Elm.val <| firstCharToUpper cctxop.name)
                                                                [ getPatternExp 0 replacePatternsArray
                                                                , arg2
                                                                , arg3
                                                                ]
                                                            )
                                                            (Elm.val "removedCctx")
                                                ]
                                            )
                                            (Elm.apply
                                                (Elm.val "moveCCtxHoleUp")
                                                [ Elm.val "arg1", Elm.val "rest" ]
                                            )

                                    2 ->
                                        Elm.Op.pipe
                                            (Elm.apply maybeMap
                                                [ Elm.fn
                                                    ( "(newCctx, removedCctx)", Just <| Type.tuple (Type.named [] "Cctx") (Type.named [] "Cctx") )
                                                  <|
                                                    \res ->
                                                        Elm.tuple
                                                            (Elm.apply (Elm.val <| firstCharToUpper cctxop.name)
                                                                [ arg1
                                                                , getPatternExp 1 replacePatternsArray
                                                                , arg3
                                                                ]
                                                            )
                                                            (Elm.val "removedCctx")
                                                ]
                                            )
                                            (Elm.apply
                                                (Elm.val "moveCCtxHoleUp")
                                                [ Elm.val "arg2", Elm.val "rest" ]
                                            )

                                    3 ->
                                        Elm.Op.pipe
                                            (Elm.apply maybeMap
                                                [ Elm.fn
                                                    ( "(newCctx, removedCctx)", Just <| Type.tuple (Type.named [] "Cctx") (Type.named [] "Cctx") )
                                                  <|
                                                    \res ->
                                                        Elm.tuple
                                                            (Elm.apply (Elm.val <| firstCharToUpper cctxop.name)
                                                                [ arg1
                                                                , arg2
                                                                , getPatternExp 2 replacePatternsArray
                                                                ]
                                                            )
                                                            (Elm.val "removedCctx")
                                                ]
                                            )
                                            (Elm.apply
                                                (Elm.val "moveCCtxHoleUp")
                                                [ Elm.val "arg3", Elm.val "rest" ]
                                            )

                                    _ ->
                                        Debug.todo "invalid cctx number"

                    4 ->
                        branch4
                            cctxop.name
                            ( getPattern 0 patternsArray, getAnnotation 0 annotationArray )
                            ( getPattern 1 patternsArray, getAnnotation 1 annotationArray )
                            ( getPattern 2 patternsArray, getAnnotation 2 annotationArray )
                            ( getPattern 3 patternsArray, getAnnotation 3 annotationArray )
                        <|
                            \arg1 arg2 arg3 arg4 ->
                                case getCCtxNum cctxop of
                                    1 ->
                                        Elm.Op.pipe
                                            (Elm.apply maybeMap
                                                [ Elm.fn
                                                    ( "(newCctx, removedCctx)", Just <| Type.tuple (Type.named [] "Cctx") (Type.named [] "Cctx") )
                                                  <|
                                                    \res ->
                                                        Elm.tuple
                                                            (Elm.apply (Elm.val <| firstCharToUpper cctxop.name)
                                                                [ getPatternExp 0 replacePatternsArray
                                                                , arg2
                                                                , arg3
                                                                , arg4
                                                                ]
                                                            )
                                                            (Elm.val "removedCctx")
                                                ]
                                            )
                                            (Elm.apply
                                                (Elm.val "moveCCtxHoleUp")
                                                [ Elm.val "arg1", Elm.val "rest" ]
                                            )

                                    2 ->
                                        Elm.Op.pipe
                                            (Elm.apply maybeMap
                                                [ Elm.fn
                                                    ( "(newCctx, removedCctx)", Just <| Type.tuple (Type.named [] "Cctx") (Type.named [] "Cctx") )
                                                  <|
                                                    \res ->
                                                        Elm.tuple
                                                            (Elm.apply (Elm.val <| firstCharToUpper cctxop.name)
                                                                [ arg1
                                                                , getPatternExp 1 replacePatternsArray
                                                                , arg3
                                                                , arg4
                                                                ]
                                                            )
                                                            (Elm.val "removedCctx")
                                                ]
                                            )
                                            (Elm.apply
                                                (Elm.val "moveCCtxHoleUp")
                                                [ Elm.val "arg2", Elm.val "rest" ]
                                            )

                                    3 ->
                                        Elm.Op.pipe
                                            (Elm.apply maybeMap
                                                [ Elm.fn
                                                    ( "(newCctx, removedCctx)", Just <| Type.tuple (Type.named [] "Cctx") (Type.named [] "Cctx") )
                                                  <|
                                                    \res ->
                                                        Elm.tuple
                                                            (Elm.apply (Elm.val <| firstCharToUpper cctxop.name)
                                                                [ arg1
                                                                , arg2
                                                                , getPatternExp 2 replacePatternsArray
                                                                , arg4
                                                                ]
                                                            )
                                                            (Elm.val "removedCctx")
                                                ]
                                            )
                                            (Elm.apply
                                                (Elm.val "moveCCtxHoleUp")
                                                [ Elm.val "arg3", Elm.val "rest" ]
                                            )

                                    4 ->
                                        Elm.Op.pipe
                                            (Elm.apply maybeMap
                                                [ Elm.fn
                                                    ( "(newCctx, removedCctx)", Just <| Type.tuple (Type.named [] "Cctx") (Type.named [] "Cctx") )
                                                  <|
                                                    \res ->
                                                        Elm.tuple
                                                            (Elm.apply (Elm.val <| firstCharToUpper cctxop.name)
                                                                [ arg1
                                                                , arg2
                                                                , arg3
                                                                , getPatternExp 3 replacePatternsArray
                                                                ]
                                                            )
                                                            (Elm.val "removedCctx")
                                                ]
                                            )
                                            (Elm.apply
                                                (Elm.val "moveCCtxHoleUp")
                                                [ Elm.val "arg4", Elm.val "rest" ]
                                            )

                                    _ ->
                                        Debug.todo "invalid cctx number"

                    5 ->
                        branch5
                            cctxop.name
                            ( getPattern 0 patternsArray, getAnnotation 0 annotationArray )
                            ( getPattern 1 patternsArray, getAnnotation 1 annotationArray )
                            ( getPattern 2 patternsArray, getAnnotation 2 annotationArray )
                            ( getPattern 3 patternsArray, getAnnotation 3 annotationArray )
                            ( getPattern 4 patternsArray, getAnnotation 4 annotationArray )
                        <|
                            \arg1 arg2 arg3 arg4 arg5 ->
                                case getCCtxNum cctxop of
                                    1 ->
                                        Elm.Op.pipe
                                            (Elm.apply maybeMap
                                                [ Elm.fn
                                                    ( "(newCctx, removedCctx)", Just <| Type.tuple (Type.named [] "Cctx") (Type.named [] "Cctx") )
                                                  <|
                                                    \res ->
                                                        Elm.tuple
                                                            (Elm.apply (Elm.val <| firstCharToUpper cctxop.name)
                                                                [ getPatternExp 0 replacePatternsArray
                                                                , arg2
                                                                , arg3
                                                                , arg4
                                                                , arg5
                                                                ]
                                                            )
                                                            (Elm.val "removedCctx")
                                                ]
                                            )
                                            (Elm.apply
                                                (Elm.val "moveCCtxHoleUp")
                                                [ Elm.val "arg1", Elm.val "rest" ]
                                            )

                                    2 ->
                                        Elm.Op.pipe
                                            (Elm.apply maybeMap
                                                [ Elm.fn
                                                    ( "(newCctx, removedCctx)", Just <| Type.tuple (Type.named [] "Cctx") (Type.named [] "Cctx") )
                                                  <|
                                                    \res ->
                                                        Elm.tuple
                                                            (Elm.apply (Elm.val <| firstCharToUpper cctxop.name)
                                                                [ arg1
                                                                , getPatternExp 1 replacePatternsArray
                                                                , arg3
                                                                , arg4
                                                                , arg5
                                                                ]
                                                            )
                                                            (Elm.val "removedCctx")
                                                ]
                                            )
                                            (Elm.apply
                                                (Elm.val "moveCCtxHoleUp")
                                                [ Elm.val "arg2", Elm.val "rest" ]
                                            )

                                    3 ->
                                        Elm.Op.pipe
                                            (Elm.apply maybeMap
                                                [ Elm.fn
                                                    ( "(newCctx, removedCctx)", Just <| Type.tuple (Type.named [] "Cctx") (Type.named [] "Cctx") )
                                                  <|
                                                    \res ->
                                                        Elm.tuple
                                                            (Elm.apply (Elm.val <| firstCharToUpper cctxop.name)
                                                                [ arg1
                                                                , arg2
                                                                , getPatternExp 2 replacePatternsArray
                                                                , arg4
                                                                , arg5
                                                                ]
                                                            )
                                                            (Elm.val "removedCctx")
                                                ]
                                            )
                                            (Elm.apply
                                                (Elm.val "moveCCtxHoleUp")
                                                [ Elm.val "arg3", Elm.val "rest" ]
                                            )

                                    4 ->
                                        Elm.Op.pipe
                                            (Elm.apply maybeMap
                                                [ Elm.fn
                                                    ( "(newCctx, removedCctx)", Just <| Type.tuple (Type.named [] "Cctx") (Type.named [] "Cctx") )
                                                  <|
                                                    \res ->
                                                        Elm.tuple
                                                            (Elm.apply (Elm.val <| firstCharToUpper cctxop.name)
                                                                [ arg1
                                                                , arg2
                                                                , arg3
                                                                , getPatternExp 3 replacePatternsArray
                                                                , arg5
                                                                ]
                                                            )
                                                            (Elm.val "removedCctx")
                                                ]
                                            )
                                            (Elm.apply
                                                (Elm.val "moveCCtxHoleUp")
                                                [ Elm.val "arg4", Elm.val "rest" ]
                                            )

                                    5 ->
                                        Elm.Op.pipe
                                            (Elm.apply maybeMap
                                                [ Elm.fn
                                                    ( "(newCctx, removedCctx)", Just <| Type.tuple (Type.named [] "Cctx") (Type.named [] "Cctx") )
                                                  <|
                                                    \res ->
                                                        Elm.tuple
                                                            (Elm.apply (Elm.val <| firstCharToUpper cctxop.name)
                                                                [ arg1
                                                                , arg2
                                                                , arg3
                                                                , arg4
                                                                , getPatternExp 4 replacePatternsArray
                                                                ]
                                                            )
                                                            (Elm.val "removedCctx")
                                                ]
                                            )
                                            (Elm.apply
                                                (Elm.val "moveCCtxHoleUp")
                                                [ Elm.val "arg5", Elm.val "rest" ]
                                            )

                                    _ ->
                                        Debug.todo "invalid cctx number"

                    _ ->
                        branchWith cctxop.name (List.length cctxop.arity) <| \exps -> Elm.just <| Elm.tuple (Elm.val "Cctx_hole") (Elm.val "Cctx_hole")
            )
            (List.concatMap .ops <| cctxSyntax.synCatOps)
        )


maybeMap : Elm.Expression
maybeMap =
    Elm.value
        { importFrom = [ "Maybe" ]
        , name = "map"
        , annotation = Nothing
        }


moveCCtxParentCases : Elm.Expression -> CCtxSyntax -> Elm.Expression
moveCCtxParentCases cctx cctxSyntax =
    custom
        cctx
        (Type.named [] "Cctx")
        (List.map
            (\cctxop ->
                let
                    patterns =
                        List.indexedMap
                            (\i ( boundVars, _ ) ->
                                if List.isEmpty boundVars then
                                    "arg" ++ String.fromInt (i + 1)

                                else
                                    "(boundVars" ++ String.fromInt (i + 1) ++ ", arg" ++ String.fromInt (i + 1) ++ ")"
                            )
                            cctxop.arity

                    replacePatterns =
                        List.indexedMap
                            (\i ( boundVars, _ ) ->
                                if List.isEmpty boundVars then
                                    "Cctx_hole"

                                else
                                    "(boundVars" ++ String.fromInt (i + 1) ++ ", Cctx_hole )"
                            )
                            cctxop.arity

                    replacePatternsArray =
                        Array.fromList replacePatterns

                    patternsArray =
                        Array.fromList patterns

                    annotationArray =
                        argsAnnotation cctxop.arity
                in
                case List.length cctxop.arity of
                    0 ->
                        branch0 cctxop.name <| Elm.nothing

                    1 ->
                        branch1 cctxop.name ( getPattern 0 patternsArray, getAnnotation 0 annotationArray ) <|
                            \arg1 ->
                                Elm.just <|
                                    Elm.tuple
                                        (Elm.apply (Elm.val <| firstCharToUpper cctxop.name) [ Elm.val "Cctx_hole" ])
                                        ((Elm.val <| "arg" ++ getCCtxNumAsStr cctxop) |> Elm.withType (Type.named [] "Cctx"))

                    2 ->
                        branch2
                            cctxop.name
                            ( getPattern 0 patternsArray, getAnnotation 0 annotationArray )
                            ( getPattern 1 patternsArray, getAnnotation 1 annotationArray )
                        <|
                            \arg1 arg2 ->
                                case getCCtxNum cctxop of
                                    1 ->
                                        Elm.just <|
                                            Elm.tuple
                                                (Elm.apply
                                                    (Elm.val <| firstCharToUpper cctxop.name)
                                                    [ Elm.val <| getPattern 0 replacePatternsArray
                                                    , arg2
                                                    ]
                                                )
                                                ((Elm.val <| "arg1")
                                                    |> Elm.withType (Type.named [] "Cctx")
                                                )

                                    2 ->
                                        Elm.just <|
                                            Elm.tuple
                                                (Elm.apply
                                                    (Elm.val <| firstCharToUpper cctxop.name)
                                                    [ arg1
                                                    , Elm.val <| getPattern 1 replacePatternsArray
                                                    ]
                                                )
                                                ((Elm.val <| "arg2")
                                                    |> Elm.withType (Type.named [] "Cctx")
                                                )

                                    _ ->
                                        Debug.todo "invalid cctx number"

                    3 ->
                        branch3
                            cctxop.name
                            ( getPattern 0 patternsArray, getAnnotation 0 annotationArray )
                            ( getPattern 1 patternsArray, getAnnotation 1 annotationArray )
                            ( getPattern 2 patternsArray, getAnnotation 2 annotationArray )
                        <|
                            \arg1 arg2 arg3 ->
                                case getCCtxNum cctxop of
                                    1 ->
                                        Elm.just <|
                                            Elm.tuple
                                                (Elm.apply
                                                    (Elm.val <| firstCharToUpper cctxop.name)
                                                    [ Elm.val <| getPattern 0 replacePatternsArray
                                                    , arg2
                                                    , arg3
                                                    ]
                                                )
                                                ((Elm.val <| "arg1")
                                                    |> Elm.withType (Type.named [] "Cctx")
                                                )

                                    2 ->
                                        Elm.just <|
                                            Elm.tuple
                                                (Elm.apply
                                                    (Elm.val <| firstCharToUpper cctxop.name)
                                                    [ arg1
                                                    , Elm.val <| getPattern 1 replacePatternsArray
                                                    , arg3
                                                    ]
                                                )
                                                ((Elm.val <| "arg2")
                                                    |> Elm.withType (Type.named [] "Cctx")
                                                )

                                    3 ->
                                        Elm.just <|
                                            Elm.tuple
                                                (Elm.apply
                                                    (Elm.val <| firstCharToUpper cctxop.name)
                                                    [ arg1
                                                    , arg2
                                                    , Elm.val <| getPattern 2 replacePatternsArray
                                                    ]
                                                )
                                                ((Elm.val <| "arg3")
                                                    |> Elm.withType (Type.named [] "Cctx")
                                                )

                                    _ ->
                                        Debug.todo "invalid cctx number"

                    4 ->
                        branch4
                            cctxop.name
                            ( getPattern 0 patternsArray, getAnnotation 0 annotationArray )
                            ( getPattern 1 patternsArray, getAnnotation 1 annotationArray )
                            ( getPattern 2 patternsArray, getAnnotation 2 annotationArray )
                            ( getPattern 3 patternsArray, getAnnotation 3 annotationArray )
                        <|
                            \arg1 arg2 arg3 arg4 ->
                                case getCCtxNum cctxop of
                                    1 ->
                                        Elm.just <|
                                            Elm.tuple
                                                (Elm.apply
                                                    (Elm.val <| firstCharToUpper cctxop.name)
                                                    [ Elm.val <| getPattern 0 replacePatternsArray
                                                    , arg2
                                                    , arg3
                                                    , arg4
                                                    ]
                                                )
                                                ((Elm.val <| "arg1")
                                                    |> Elm.withType (Type.named [] "Cctx")
                                                )

                                    2 ->
                                        Elm.just <|
                                            Elm.tuple
                                                (Elm.apply
                                                    (Elm.val <| firstCharToUpper cctxop.name)
                                                    [ arg1
                                                    , Elm.val <| getPattern 1 replacePatternsArray
                                                    , arg3
                                                    , arg4
                                                    ]
                                                )
                                                ((Elm.val <| "arg2")
                                                    |> Elm.withType (Type.named [] "Cctx")
                                                )

                                    3 ->
                                        Elm.just <|
                                            Elm.tuple
                                                (Elm.apply
                                                    (Elm.val <| firstCharToUpper cctxop.name)
                                                    [ arg1
                                                    , arg2
                                                    , Elm.val <| getPattern 2 replacePatternsArray
                                                    , arg4
                                                    ]
                                                )
                                                ((Elm.val <| "arg3")
                                                    |> Elm.withType (Type.named [] "Cctx")
                                                )

                                    4 ->
                                        Elm.just <|
                                            Elm.tuple
                                                (Elm.apply
                                                    (Elm.val <| firstCharToUpper cctxop.name)
                                                    [ arg1
                                                    , arg2
                                                    , arg3
                                                    , Elm.val <| getPattern 3 replacePatternsArray
                                                    ]
                                                )
                                                ((Elm.val <| "arg4")
                                                    |> Elm.withType (Type.named [] "Cctx")
                                                )

                                    _ ->
                                        Debug.todo "invalid cctx number"

                    5 ->
                        branch5
                            cctxop.name
                            ( getPattern 0 patternsArray, getAnnotation 0 annotationArray )
                            ( getPattern 1 patternsArray, getAnnotation 1 annotationArray )
                            ( getPattern 2 patternsArray, getAnnotation 2 annotationArray )
                            ( getPattern 3 patternsArray, getAnnotation 3 annotationArray )
                            ( getPattern 4 patternsArray, getAnnotation 4 annotationArray )
                        <|
                            \arg1 arg2 arg3 arg4 arg5 ->
                                case getCCtxNum cctxop of
                                    1 ->
                                        Elm.just <|
                                            Elm.tuple
                                                (Elm.apply
                                                    (Elm.val <| firstCharToUpper cctxop.name)
                                                    [ Elm.val <| getPattern 0 replacePatternsArray
                                                    , arg2
                                                    , arg3
                                                    , arg4
                                                    , arg5
                                                    ]
                                                )
                                                ((Elm.val <| "arg1")
                                                    |> Elm.withType (Type.named [] "Cctx")
                                                )

                                    2 ->
                                        Elm.just <|
                                            Elm.tuple
                                                (Elm.apply
                                                    (Elm.val <| firstCharToUpper cctxop.name)
                                                    [ arg1
                                                    , Elm.val <| getPattern 1 replacePatternsArray
                                                    , arg3
                                                    , arg4
                                                    , arg5
                                                    ]
                                                )
                                                ((Elm.val <| "arg2")
                                                    |> Elm.withType (Type.named [] "Cctx")
                                                )

                                    3 ->
                                        Elm.just <|
                                            Elm.tuple
                                                (Elm.apply
                                                    (Elm.val <| firstCharToUpper cctxop.name)
                                                    [ arg1
                                                    , arg2
                                                    , Elm.val <| getPattern 2 replacePatternsArray
                                                    , arg4
                                                    , arg5
                                                    ]
                                                )
                                                ((Elm.val <| "arg3")
                                                    |> Elm.withType (Type.named [] "Cctx")
                                                )

                                    4 ->
                                        Elm.just <|
                                            Elm.tuple
                                                (Elm.apply
                                                    (Elm.val <| firstCharToUpper cctxop.name)
                                                    [ arg1
                                                    , arg2
                                                    , arg3
                                                    , Elm.val <| getPattern 3 replacePatternsArray
                                                    , arg5
                                                    ]
                                                )
                                                ((Elm.val <| "arg4")
                                                    |> Elm.withType (Type.named [] "Cctx")
                                                )

                                    5 ->
                                        Elm.just <|
                                            Elm.tuple
                                                (Elm.apply
                                                    (Elm.val <| firstCharToUpper cctxop.name)
                                                    [ arg1
                                                    , arg2
                                                    , arg3
                                                    , arg4
                                                    , Elm.val <| getPattern 4 replacePatternsArray
                                                    ]
                                                )
                                                ((Elm.val <| "arg5")
                                                    |> Elm.withType (Type.named [] "Cctx")
                                                )

                                    _ ->
                                        Debug.todo "invalid cctx number"

                    _ ->
                        Debug.todo "operator with more than 5 args not supported"
            )
            (List.concatMap .ops <| cctxSyntax.synCatOps)
        )


getAnnotation : Int -> Array.Array Annotation -> Annotation
getAnnotation index annotationArray =
    Maybe.withDefault (Type.named [] "ERROR") <| Array.get index annotationArray


argsAnnotation : Arity -> Array.Array Annotation
argsAnnotation arity =
    List.map
        (\arg ->
            case arg of
                ( [], arg_ ) ->
                    Type.named [] arg_

                ( boundVars, arg_ ) ->
                    Type.tuple
                        (Type.list <|
                            Type.named [] <|
                                Maybe.withDefault "ERROR" <|
                                    List.head boundVars
                        )
                        (Type.named [] arg_)
        )
        arity
        |> Array.fromList


getArg : Int -> Array.Array String -> String
getArg index argsArray =
    Maybe.withDefault "ERROR" <| Array.get index argsArray


getPattern : Int -> Array.Array String -> String
getPattern index patternsArray =
    Maybe.withDefault "ERROR" <| Array.get index patternsArray


getPatternExp : Int -> Array.Array Elm.Expression -> Elm.Expression
getPatternExp index patternsArray =
    Maybe.withDefault (Elm.val "ERROR") <| Array.get index patternsArray


getPatternExp_ : Int -> Array.Array String -> Elm.Expression
getPatternExp_ index patternsArray =
    Elm.val <| Maybe.withDefault "ERROR" <| Array.get index patternsArray


getCCtxNum : Operator -> Int
getCCtxNum op =
    String.right 1 op.name |> String.toInt |> Maybe.withDefault 500


getCCtxNumAsStr : Operator -> String
getCCtxNumAsStr op =
    String.right 1 op.name
