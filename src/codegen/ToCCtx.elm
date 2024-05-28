module ToCCtx exposing (..)

import Array
import Elm
import Elm.Annotation as Type exposing (..)
import Elm.Case exposing (..)
import Elm.Case.Branch as Branch
import Elm.Let
import Elm.Op
import Gen.Convertable
import Gen.Decomposable exposing (..)
import Gen.Dict exposing (remove)
import Gen.Substitutable exposing (..)
import Html exposing (a)
import Parser exposing (..)
import RawSyntaxP exposing (..)
import Syntax exposing (..)



-- This file defines the functions to be generated
-- This includes functions for getting the path to a cursor in a tree
-- And functions for decomposing an AST into a CCTX,Wellformed tuple etc.alias
{-
   Given a Base syntax, convert it to a CursorLess syntax
-}


createToCCtxFuns : Syntax -> List Elm.Declaration
createToCCtxFuns syntax =
    List.map createToCCtxFun syntax.synCatOps
        ++ [ Elm.declaration "toCCtx" <|
                Elm.withType
                    (Type.function
                        [ Type.named [] "Base", Type.list Type.int ]
                        (Type.tuple (Type.named [] "Cctx") (Type.named [] "Base"))
                    )
                <|
                    Elm.fn2
                        ( "base", Nothing )
                        ( "path", Nothing )
                        (\base path ->
                            Elm.Case.custom base
                                (Type.named [] "Base")
                                (List.map
                                    (\synCatOp ->
                                        Elm.Case.branchWith
                                            synCatOp.synCat
                                            1
                                            (\exps ->
                                                Elm.apply (Elm.val <| "toCCtx_" ++ synCatOp.synCat) (exps ++ [ path ])
                                            )
                                    )
                                    syntax.synCatOps
                                )
                        )
           ]


createToCCtxFun : SynCatOps -> Elm.Declaration
createToCCtxFun synCatOp =
    Elm.declaration ("toCCtx_" ++ synCatOp.synCat) <|
        Elm.withType
            (Type.function
                [ Type.named [] synCatOp.synCat
                , Type.list Type.int
                ]
                (Type.tuple (Type.named [] "Cctx") (Type.named [] "Base"))
            )
        <|
            Elm.fn2
                ( synCatOp.synCat, Nothing )
                ( "path", Nothing )
                (\op path ->
                    custom path
                        (Type.list Type.int)
                        [ Branch.variant0 "[]"
                            (Elm.tuple
                                (Elm.val "Cctx_hole")
                                (Elm.apply (Elm.val <| firstCharToUpper synCatOp.synCat) [ op ])
                            )
                        , Branch.listWithRemaining
                            { patterns =
                                [ Branch.var "i" ]
                            , remaining = Branch.var "rest"
                            , gather = \_ _ -> Elm.val ""
                            , startWith = Elm.val ""
                            , finally =
                                \i rest ->
                                    custom op
                                        (Type.named [] synCatOp.synCat)
                                        (getBranchListSynCatOp synCatOp)
                            }
                        ]
                )



-- pathBranches : List Branch.Branch
-- pathBranches =


getBranchListSynCatOp : SynCatOps -> List Branch.Branch
getBranchListSynCatOp synCatOp =
    List.map getBranchFromOp synCatOp.ops


getBranchFromOp : Operator -> Branch.Branch
getBranchFromOp op =
    let
        patterns =
            List.indexedMap
                (\i_ ( boundVars, arg ) ->
                    case getPatternFromArg (i_ + 1) ( boundVars, arg ) of
                        Just pattern ->
                            pattern

                        Nothing ->
                            Branch.var ("arg" ++ String.fromInt (i_ + 1))
                )
                op.arity

        patternsArray =
            Array.fromList patterns

        argsArray =
            op.arity |> Array.fromList
    in
    if String.contains "cursor" op.name then
        Branch.variant1 op.name
            (Branch.ignore "_")
            (\_ ->
                Elm.apply
                    (Elm.value
                        { importFrom = [ "Debug" ]
                        , name = "todo"
                        , annotation = Nothing
                        }
                    )
                    [ Elm.string "Invalid path: we hit a cursor but path list is non-empty" ]
            )

    else
        case List.length op.arity of
            0 ->
                case op.literal of
                    Nothing ->
                        Branch.variant0 op.name
                            (Elm.apply
                                (Elm.value
                                    { importFrom = [ "Debug" ]
                                    , name = "todo"
                                    , annotation = Nothing
                                    }
                                )
                                [ Elm.string "Invalid path: we hit a 0-arity operator but path list is non-empty" ]
                            )

                    Just lit ->
                        Branch.variant1 op.name (Branch.var "lit") <|
                            \x ->
                                Elm.apply
                                    (Elm.value
                                        { importFrom = [ "Debug" ]
                                        , name = "todo"
                                        , annotation = Nothing
                                        }
                                    )
                                    [ Elm.string "Invalid path: we hit a 0-arity operator but path list is non-empty" ]

            1 ->
                let
                    arg1 =
                        Maybe.withDefault ( [], "ERROR" ) (Array.get 0 argsArray)
                in
                Branch.variant1
                    op.name
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                    (\arg ->
                        custom
                            (Elm.val "i")
                            Type.int
                            [ Branch.int 1
                                (Elm.Let.letIn
                                    (\( cctxChild, restTree ) ->
                                        Elm.tuple
                                            (Elm.apply
                                                (Elm.val <| firstCharToUpper <| op.name ++ "_CLess_cctx1")
                                                [ argToCctxTransformation_ 1 arg1
                                                ]
                                            )
                                            restTree
                                    )
                                    |> Elm.Let.tuple
                                        "cctxChild"
                                        "restTree"
                                        (Elm.apply
                                            (Elm.val <| "toCCtx" ++ "_" ++ Tuple.second arg1)
                                            [ Elm.val "arg1", Elm.val "rest" ]
                                        )
                                    |> Elm.Let.toExpression
                                )
                            , Branch.ignore
                                (Elm.apply
                                    (Elm.value
                                        { importFrom = [ "Debug" ]
                                        , name = "todo"
                                        , annotation = Nothing
                                        }
                                    )
                                    [ Elm.string "Invalid path" ]
                                )
                            ]
                    )

            2 ->
                let
                    arg1 =
                        Maybe.withDefault ( [], "ERROR" ) (Array.get 0 argsArray)

                    arg2 =
                        Maybe.withDefault ( [], "ERROR" ) (Array.get 1 argsArray)
                in
                Branch.variant2
                    op.name
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 1 patternsArray)
                    (\arg1Exp arg2Exp ->
                        custom
                            (Elm.val "i")
                            Type.int
                            [ Branch.int 1 <|
                                (Elm.Let.letIn
                                    (\( cctxChild, restTree ) ->
                                        Elm.tuple
                                            (Elm.apply
                                                (Elm.val <| firstCharToUpper <| op.name ++ "_CLess_cctx1")
                                                [ argToCctxTransformation_ 1 arg1
                                                , argToCLessTransformation 2 arg2
                                                ]
                                            )
                                            restTree
                                    )
                                    |> Elm.Let.tuple
                                        "cctxChild"
                                        "restTree"
                                        (Elm.apply
                                            (Elm.val <| "toCCtx" ++ "_" ++ Tuple.second arg1)
                                            [ Elm.val "arg1", Elm.val "rest" ]
                                        )
                                    |> Elm.Let.toExpression
                                )
                            , Branch.int 2 <|
                                (Elm.Let.letIn
                                    (\( cctxChild, restTree ) ->
                                        Elm.tuple
                                            (Elm.apply
                                                (Elm.val <| firstCharToUpper <| op.name ++ "_CLess_cctx2")
                                                [ argToCLessTransformation 1 arg1
                                                , argToCctxTransformation_ 2 arg2
                                                ]
                                            )
                                            restTree
                                    )
                                    |> Elm.Let.tuple
                                        "cctxChild"
                                        "restTree"
                                        (Elm.apply
                                            (Elm.val <| "toCCtx" ++ "_" ++ Tuple.second arg2)
                                            [ Elm.val "arg2", Elm.val "rest" ]
                                        )
                                    |> Elm.Let.toExpression
                                )
                            , Branch.ignore
                                (Elm.apply
                                    (Elm.value
                                        { importFrom = [ "Debug" ]
                                        , name = "todo"
                                        , annotation = Nothing
                                        }
                                    )
                                    [ Elm.string "Invalid path" ]
                                )
                            ]
                    )

            3 ->
                let
                    arg1 =
                        Maybe.withDefault ( [], "ERROR" ) (Array.get 0 argsArray)

                    arg2 =
                        Maybe.withDefault ( [], "ERROR" ) (Array.get 1 argsArray)

                    arg3 =
                        Maybe.withDefault ( [], "ERROR" ) (Array.get 2 argsArray)
                in
                Branch.variant3
                    op.name
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 1 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 2 patternsArray)
                    (\arg1Exp arg2Exp arg3Exp ->
                        custom
                            (Elm.val "i")
                            Type.int
                            [ Branch.int 1 <|
                                (Elm.Let.letIn
                                    (\( cctxChild, restTree ) ->
                                        Elm.tuple
                                            (Elm.apply
                                                (Elm.val <| firstCharToUpper <| op.name ++ "_CLess_cctx1")
                                                [ argToCctxTransformation_ 1 arg1
                                                , argToCLessTransformation 2 arg2
                                                , argToCLessTransformation 3 arg3
                                                ]
                                            )
                                            restTree
                                    )
                                    |> Elm.Let.tuple
                                        "cctxChild"
                                        "restTree"
                                        (Elm.apply
                                            (Elm.val <| "toCCtx" ++ "_" ++ Tuple.second arg1)
                                            [ Elm.val "arg1", Elm.val "rest" ]
                                        )
                                    |> Elm.Let.toExpression
                                )
                            , Branch.int 2 <|
                                (Elm.Let.letIn
                                    (\( cctxChild, restTree ) ->
                                        Elm.tuple
                                            (Elm.apply
                                                (Elm.val <| firstCharToUpper <| op.name ++ "_CLess_cctx2")
                                                [ argToCLessTransformation 1 arg1
                                                , argToCctxTransformation_ 2 arg2
                                                , argToCLessTransformation 3 arg3
                                                ]
                                            )
                                            restTree
                                    )
                                    |> Elm.Let.tuple
                                        "cctxChild"
                                        "restTree"
                                        (Elm.apply
                                            (Elm.val <| "toCCtx" ++ "_" ++ Tuple.second arg2)
                                            [ Elm.val "arg2"
                                            , Elm.val "rest"
                                            ]
                                        )
                                    |> Elm.Let.toExpression
                                )
                            , Branch.int 3 <|
                                (Elm.Let.letIn
                                    (\( cctxChild, restTree ) ->
                                        Elm.tuple
                                            (Elm.apply
                                                (Elm.val <| firstCharToUpper <| op.name ++ "_CLess_cctx3")
                                                [ argToCLessTransformation 1 arg1
                                                , argToCLessTransformation 2 arg2
                                                , argToCctxTransformation_ 3 arg3
                                                ]
                                            )
                                            restTree
                                    )
                                    |> Elm.Let.tuple
                                        "cctxChild"
                                        "restTree"
                                        (Elm.apply
                                            (Elm.val <| "toCCtx" ++ "_" ++ Tuple.second arg3)
                                            [ Elm.val "arg3", Elm.val "rest" ]
                                        )
                                    |> Elm.Let.toExpression
                                )
                            , Branch.ignore
                                (Elm.apply
                                    (Elm.value
                                        { importFrom = [ "Debug" ]
                                        , name = "todo"
                                        , annotation = Nothing
                                        }
                                    )
                                    [ Elm.string "Invalid path" ]
                                )
                            ]
                    )

            4 ->
                let
                    arg1 =
                        Maybe.withDefault ( [], "ERROR" ) (Array.get 0 argsArray)

                    arg2 =
                        Maybe.withDefault ( [], "ERROR" ) (Array.get 1 argsArray)

                    arg3 =
                        Maybe.withDefault ( [], "ERROR" ) (Array.get 2 argsArray)

                    arg4 =
                        Maybe.withDefault ( [], "ERROR" ) (Array.get 3 argsArray)
                in
                Branch.variant4
                    op.name
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 1 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 2 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 3 patternsArray)
                    (\arg1Exp arg2Exp arg3Exp arg4Exp ->
                        custom
                            (Elm.val "i")
                            Type.int
                            [ Branch.int 1 <|
                                (Elm.Let.letIn
                                    (\( cctxChild, restTree ) ->
                                        Elm.tuple
                                            (Elm.apply
                                                (Elm.val <| firstCharToUpper <| op.name ++ "_CLess_cctx1")
                                                [ argToCctxTransformation_ 1 arg1
                                                , argToCLessTransformation 2 arg2
                                                , argToCLessTransformation 3 arg3
                                                , argToCLessTransformation 4 arg4
                                                ]
                                            )
                                            restTree
                                    )
                                    |> Elm.Let.tuple
                                        "cctxChild"
                                        "restTree"
                                        (Elm.apply
                                            (Elm.val <| "toCCtx" ++ "_" ++ Tuple.second arg1)
                                            [ Elm.val "arg1", Elm.val "rest" ]
                                        )
                                    |> Elm.Let.toExpression
                                )
                            , Branch.int 2 <|
                                (Elm.Let.letIn
                                    (\( cctxChild, restTree ) ->
                                        Elm.tuple
                                            (Elm.apply
                                                (Elm.val <| firstCharToUpper <| op.name ++ "_CLess_cctx2")
                                                [ argToCLessTransformation 1 arg1
                                                , argToCctxTransformation_ 2 arg2
                                                , argToCLessTransformation 3 arg3
                                                , argToCLessTransformation 4 arg4
                                                ]
                                            )
                                            restTree
                                    )
                                    |> Elm.Let.tuple
                                        "cctxChild"
                                        "restTree"
                                        (Elm.apply
                                            (Elm.val <| "toCCtx" ++ "_" ++ Tuple.second arg2)
                                            [ Elm.val "arg2", Elm.val "rest" ]
                                        )
                                    |> Elm.Let.toExpression
                                )
                            , Branch.int 3 <|
                                (Elm.Let.letIn
                                    (\( cctxChild, restTree ) ->
                                        Elm.tuple
                                            (Elm.apply
                                                (Elm.val <| firstCharToUpper <| op.name ++ "_CLess_cctx3")
                                                [ argToCLessTransformation 1 arg1
                                                , argToCLessTransformation 2 arg2
                                                , argToCctxTransformation_ 3 arg3
                                                , argToCLessTransformation 4 arg4
                                                ]
                                            )
                                            restTree
                                    )
                                    |> Elm.Let.tuple
                                        "cctxChild"
                                        "restTree"
                                        (Elm.apply
                                            (Elm.val <| "toCCtx" ++ "_" ++ Tuple.second arg3)
                                            [ Elm.val "arg3", Elm.val "rest" ]
                                        )
                                    |> Elm.Let.toExpression
                                )
                            , Branch.int 4 <|
                                (Elm.Let.letIn
                                    (\( cctxChild, restTree ) ->
                                        Elm.tuple
                                            (Elm.apply
                                                (Elm.val <| firstCharToUpper <| op.name ++ "_CLess_cctx4")
                                                [ argToCLessTransformation 1 arg1
                                                , argToCLessTransformation 2 arg2
                                                , argToCLessTransformation 3 arg3
                                                , argToCctxTransformation_ 4 arg4
                                                ]
                                            )
                                            restTree
                                    )
                                    |> Elm.Let.tuple
                                        "cctxChild"
                                        "restTree"
                                        (Elm.apply
                                            (Elm.val <| "toCCtx" ++ "_" ++ Tuple.second arg4)
                                            [ Elm.val "arg4", Elm.val "rest" ]
                                        )
                                    |> Elm.Let.toExpression
                                )
                            , Branch.ignore
                                (Elm.apply
                                    (Elm.value
                                        { importFrom = [ "Debug" ]
                                        , name = "todo"
                                        , annotation = Nothing
                                        }
                                    )
                                    [ Elm.string "Invalid path" ]
                                )
                            ]
                    )

            5 ->
                let
                    arg1 =
                        Maybe.withDefault ( [], "ERROR" ) (Array.get 0 argsArray)

                    arg2 =
                        Maybe.withDefault ( [], "ERROR" ) (Array.get 1 argsArray)

                    arg3 =
                        Maybe.withDefault ( [], "ERROR" ) (Array.get 2 argsArray)

                    arg4 =
                        Maybe.withDefault ( [], "ERROR" ) (Array.get 3 argsArray)

                    arg5 =
                        Maybe.withDefault ( [], "ERROR" ) (Array.get 4 argsArray)
                in
                Branch.variant5
                    op.name
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 1 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 2 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 3 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 4 patternsArray)
                    (\arg1Exp arg2Exp arg3Exp arg4Exp arg5Exp ->
                        custom
                            (Elm.val "i")
                            Type.int
                            [ Branch.int 1 <|
                                (Elm.Let.letIn
                                    (\( cctxChild, restTree ) ->
                                        Elm.tuple
                                            (Elm.apply
                                                (Elm.val <| firstCharToUpper <| op.name ++ "_CLess_cctx1")
                                                [ argToCctxTransformation_ 1 arg1
                                                , argToCLessTransformation 2 arg2
                                                , argToCLessTransformation 3 arg3
                                                , argToCLessTransformation 4 arg4
                                                , argToCLessTransformation 5 arg5
                                                ]
                                            )
                                            restTree
                                    )
                                    |> Elm.Let.tuple
                                        "cctxChild"
                                        "restTree"
                                        (Elm.apply
                                            (Elm.val <| "toCCtx" ++ "_" ++ Tuple.second arg1)
                                            [ Elm.val "arg1", Elm.val "rest" ]
                                        )
                                    |> Elm.Let.toExpression
                                )
                            , Branch.int 2 <|
                                (Elm.Let.letIn
                                    (\( cctxChild, restTree ) ->
                                        Elm.tuple
                                            (Elm.apply
                                                (Elm.val <| firstCharToUpper <| op.name ++ "_CLess_cctx2")
                                                [ argToCLessTransformation 1 arg1
                                                , argToCctxTransformation_ 2 arg2
                                                , argToCLessTransformation 3 arg3
                                                , argToCLessTransformation 4 arg4
                                                , argToCLessTransformation 5 arg5
                                                ]
                                            )
                                            restTree
                                    )
                                    |> Elm.Let.tuple
                                        "cctxChild"
                                        "restTree"
                                        (Elm.apply
                                            (Elm.val <| "toCCtx" ++ "_" ++ Tuple.second arg2)
                                            [ Elm.val "arg2", Elm.val "rest" ]
                                        )
                                    |> Elm.Let.toExpression
                                )
                            , Branch.int 3 <|
                                (Elm.Let.letIn
                                    (\( cctxChild, restTree ) ->
                                        Elm.tuple
                                            (Elm.apply
                                                (Elm.val <| firstCharToUpper <| op.name ++ "_CLess_cctx3")
                                                [ argToCLessTransformation 1 arg1
                                                , argToCLessTransformation 2 arg2
                                                , argToCctxTransformation_ 3 arg3
                                                , argToCLessTransformation 4 arg4
                                                , argToCLessTransformation 5 arg5
                                                ]
                                            )
                                            restTree
                                    )
                                    |> Elm.Let.tuple
                                        "cctxChild"
                                        "restTree"
                                        (Elm.apply
                                            (Elm.val <| "toCCtx" ++ "_" ++ Tuple.second arg3)
                                            [ Elm.val "arg3", Elm.val "rest" ]
                                        )
                                    |> Elm.Let.toExpression
                                )
                            , Branch.int 4 <|
                                (Elm.Let.letIn
                                    (\( cctxChild, restTree ) ->
                                        Elm.tuple
                                            (Elm.apply
                                                (Elm.val <| firstCharToUpper <| op.name ++ "_CLess_cctx4")
                                                [ argToCLessTransformation 1 arg1
                                                , argToCLessTransformation 2 arg2
                                                , argToCLessTransformation 3 arg3
                                                , argToCctxTransformation_ 4 arg4
                                                , argToCLessTransformation 5 arg5
                                                ]
                                            )
                                            restTree
                                    )
                                    |> Elm.Let.tuple
                                        "cctxChild"
                                        "restTree"
                                        (Elm.apply
                                            (Elm.val <| "toCCtx" ++ "_" ++ Tuple.second arg4)
                                            [ Elm.val "arg4", Elm.val "rest" ]
                                        )
                                    |> Elm.Let.toExpression
                                )
                            , Branch.int 5 <|
                                (Elm.Let.letIn
                                    (\( cctxChild, restTree ) ->
                                        Elm.tuple
                                            (Elm.apply
                                                (Elm.val <| firstCharToUpper <| op.name ++ "_CLess_cctx5")
                                                [ argToCLessTransformation 1 arg1
                                                , argToCLessTransformation 2 arg2
                                                , argToCLessTransformation 3 arg3
                                                , argToCLessTransformation 4 arg4
                                                , argToCctxTransformation_ 5 arg5
                                                ]
                                            )
                                            restTree
                                    )
                                    |> Elm.Let.tuple
                                        "cctxChild"
                                        "restTree"
                                        (Elm.apply
                                            (Elm.val <| "toCCtx" ++ "_" ++ Tuple.second arg5)
                                            [ Elm.val "arg5", Elm.val "rest" ]
                                        )
                                    |> Elm.Let.toExpression
                                )
                            , Branch.ignore
                                (Elm.apply
                                    (Elm.value
                                        { importFrom = [ "Debug" ]
                                        , name = "todo"
                                        , annotation = Nothing
                                        }
                                    )
                                    [ Elm.string "Invalid path" ]
                                )
                            ]
                    )

            _ ->
                Debug.todo "Ops with more than 5 args not supported"


getPatternFromArg : Int -> ( List String, String ) -> Maybe (Branch.Pattern Elm.Expression)
getPatternFromArg i arg =
    case arg of
        ( [], arg_ ) ->
            Nothing

        ( boundVars, arg_ ) ->
            Just <| Branch.var <| "(boundVars" ++ String.fromInt i ++ ", arg" ++ String.fromInt i ++ ")"


{-| The same but this one will always assume that there is a "cctxChild" to be used
-}
argToCctxTransformation_ : Int -> ( List String, String ) -> Elm.Expression
argToCctxTransformation_ i ( boundVars, argSort ) =
    if isBinder ( boundVars, argSort ) then
        Elm.tuple
            (Elm.apply
                (Elm.value
                    { importFrom = [ "List" ]
                    , name = "map"
                    , annotation = Nothing
                    }
                )
                [ Elm.val <| "toCLess" ++ "_" ++ getBoundVarsSort boundVars
                , Elm.val <| "boundVars" ++ String.fromInt i
                ]
            )
            (Elm.val "cctxChild")

    else
        Elm.val "cctxChild"


argToCctxTransformation : Int -> ( List String, String ) -> Elm.Expression
argToCctxTransformation i ( boundVars, argSort ) =
    if isBinder ( boundVars, argSort ) then
        Elm.tuple
            (Elm.apply
                (Elm.value
                    { importFrom = [ "List" ]
                    , name = "map"
                    , annotation = Nothing
                    }
                )
                [ Elm.val <| "toCLess" ++ "_" ++ getBoundVarsSort boundVars
                , Elm.val <| "boundVars" ++ String.fromInt i
                ]
            )
            (Elm.apply
                (Elm.val <| "toCCtx" ++ "_" ++ argSort)
                [ Elm.val <| "arg" ++ String.fromInt i
                , Elm.val <| "rest"
                ]
            )

    else
        Elm.apply
            (Elm.val <| "toCCtx" ++ "_" ++ argSort)
            [ Elm.val <| "arg" ++ String.fromInt i
            , Elm.val <| "rest"
            ]


argToCLessTransformation : Int -> ( List String, String ) -> Elm.Expression
argToCLessTransformation i ( boundVars, argSort ) =
    if isBinder ( boundVars, argSort ) then
        Elm.tuple
            (Elm.apply
                (Elm.value
                    { importFrom = [ "List" ]
                    , name = "map"
                    , annotation = Nothing
                    }
                )
                [ Elm.val <| "toCLess" ++ "_" ++ getBoundVarsSort boundVars
                , Elm.val <| "boundVars" ++ String.fromInt i
                ]
            )
            (Elm.apply
                (Elm.val <| "toCLess" ++ "_" ++ argSort)
                [ Elm.val <| "arg" ++ String.fromInt i ]
            )

    else
        Elm.apply
            (Elm.val <| "toCLess" ++ "_" ++ argSort)
            [ Elm.val <| "arg" ++ String.fromInt i ]


getBoundVarsSort : List String -> String
getBoundVarsSort boundVars =
    case List.head boundVars of
        Just boundVar ->
            boundVar

        Nothing ->
            Debug.todo "no bound vars"


isBinder : ( List String, String ) -> Bool
isBinder arg =
    case arg of
        ( [], _ ) ->
            False

        _ ->
            True
