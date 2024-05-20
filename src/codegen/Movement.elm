module Movement exposing (..)

import Array
import Elm
import Elm.Annotation as Type exposing (..)
import Elm.Case exposing (..)
import Elm.Case.Branch as Branch
import Elm.Let
import Gen.Decomposable exposing (..)
import Gen.Dict exposing (remove)
import Gen.Substitutable exposing (..)
import Html exposing (a)
import Parser exposing (..)
import RawSyntaxP exposing (..)
import Syntax exposing (..)


createChildFun : Syntax -> CLessSyntax -> CCtxSyntax -> WellFormedSyntax -> Elm.Declaration
createChildFun extendedSyntax clessSyntax cctxSyntax wellformedSyntax =
    Elm.declaration "child" <|
        Elm.withType
            (Type.function
                [ Type.int
                , Type.tuple (Type.named [] "Cctx") (Type.named [] "Wellformed")
                ]
                (Type.tuple (Type.named [] "Cctx") (Type.named [] "Wellformed"))
            )
        <|
            Elm.fn2
                ( "i", Nothing )
                ( "decomposed", Nothing )
                (\i decomposed ->
                    Elm.Let.letIn
                        (\( cctx, wellformed ) ->
                            Elm.Case.custom
                                wellformed
                                (Type.named [] "Wellformed")
                                (wellformedCases
                                    clessSyntax
                                    (List.filter
                                        (\x -> x.synCat == "wellformed")
                                        wellformedSyntax.synCatOps
                                    )
                                )
                        )
                        |> Elm.Let.tuple "cctx" "wellformed" (Elm.val "decomposed")
                        |> Elm.Let.toExpression
                )


createReplaceCCtxHoleFun : Syntax -> CLessSyntax -> CCtxSyntax -> Elm.Declaration
createReplaceCCtxHoleFun extendedSyntax clessSyntax cctxSyntax =
    Elm.declaration "replaceCctxHole" <|
        Elm.withType (Type.function [ Type.int, Type.named [] "Cctx", Type.named [] "CursorLess" ] (Type.named [] "Cctx")) <|
            Elm.fn3
                ( "i", Just Type.int )
                ( "orig_cctx", Just <| Type.named [] "Cctx" )
                ( "underCursor", Just <| Type.named [] "CursorLess" )
                (\i orig_cctx underCursor ->
                    -- create a case expression for every CCtx operator
                    cctxCases extendedSyntax cctxSyntax clessSyntax i orig_cctx underCursor
                )


wellformedCases : CLessSyntax -> List SynCatOps -> List Branch.Branch
wellformedCases clessSyntax wellformedsynCatOps =
    let
        allWellformedOps =
            List.concatMap (\x -> x.ops) wellformedsynCatOps

        clessSyncatOps =
            List.filter (\x -> String.endsWith "_CLess" x.synCat) clessSyntax.synCatOps

        allCLessOps =
            List.concatMap (\x -> x.ops) clessSyncatOps
    in
    List.map
        (\op ->
            let
                clessName =
                    -- firstCharToUpper <| String.dropLeft 5 op.name
                    String.dropLeft 5 op.name

                patterns =
                    Array.fromList <| List.indexedMap getPattern_ op.arity
            in
            Branch.variant1
                op.name
                (Branch.var "underCursor")
                (\underCursor ->
                    Elm.Case.custom underCursor
                        (Type.named [] clessName)
                        (List.map
                            (getUnderCursorBranch underCursor)
                            (List.filter (\x -> x.synCat == clessName) allCLessOps)
                        )
                )
        )
        allWellformedOps


getUnderCursorBranch : Elm.Expression -> Operator -> Branch.Branch
getUnderCursorBranch underCursor op =
    let
        patterns =
            List.indexedMap
                (\i ( boundVars, arg ) ->
                    if List.isEmpty boundVars then
                        Branch.var <| "arg" ++ String.fromInt (i + 1)

                    else
                        Branch.var <| "(boundVars" ++ String.fromInt (i + 1) ++ ", arg" ++ String.fromInt (i + 1) ++ ")"
                )
                op.arity

        patternsArray =
            Array.fromList patterns

        argsArray =
            op.arity |> Array.fromList
    in
    case List.length op.arity of
        0 ->
            Branch.variant0 op.name
                (Elm.apply
                    (Elm.value
                        { importFrom = [ "Debug" ]
                        , name = "todo"
                        , annotation = Nothing
                        }
                    )
                    [ Elm.string "No children at op under cursor, cannot go deeper" ]
                )

        1 ->
            let
                arg1Sort =
                    Maybe.withDefault ( [], "ERROR" ) (Array.get 0 argsArray) |> Tuple.second
            in
            Branch.variant1
                op.name
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                (\arg ->
                    Elm.Case.custom
                        (Elm.val "i")
                        Type.int
                        [ Branch.int 1 <|
                            Elm.tuple
                                (Elm.apply
                                    (Elm.val "replaceCctxHole")
                                    [ Elm.val "i"
                                    , Elm.val "cctx"
                                    , Elm.apply (Elm.val <| firstCharToUpper op.synCat) [ underCursor ]
                                    ]
                                )
                                (Elm.apply (Elm.val <| "Root_" ++ arg1Sort) [ Elm.val "arg1" ])
                        , Branch.ignore <|
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Debug" ]
                                    , name = "todo"
                                    , annotation = Nothing
                                    }
                                )
                                [ Elm.string "Invalid child index" ]
                        ]
                )

        2 ->
            let
                arg1Sort =
                    Maybe.withDefault ( [], "ERROR" ) (Array.get 0 argsArray) |> Tuple.second

                arg2Sort =
                    Maybe.withDefault ( [], "ERROR" ) (Array.get 1 argsArray) |> Tuple.second
            in
            Branch.variant2
                op.name
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 1 patternsArray)
                (\arg1 arg2 ->
                    Elm.Case.custom
                        (Elm.val "i")
                        Type.int
                        [ Branch.int 1 <|
                            Elm.tuple
                                (Elm.apply
                                    (Elm.val "replaceCctxHole")
                                    [ Elm.val "i"
                                    , Elm.val "cctx"
                                    , Elm.apply (Elm.val <| firstCharToUpper op.synCat) [ underCursor ]
                                    ]
                                )
                                (Elm.apply (Elm.val <| "Root_" ++ arg1Sort) [ Elm.val "arg1" ])
                        , Branch.int 2 <|
                            Elm.tuple
                                (Elm.apply
                                    (Elm.val "replaceCctxHole")
                                    [ Elm.val "i"
                                    , Elm.val "cctx"
                                    , Elm.apply (Elm.val <| firstCharToUpper op.synCat) [ underCursor ]
                                    ]
                                )
                                (Elm.apply (Elm.val <| "Root_" ++ arg2Sort) [ Elm.val "arg2" ])
                        , Branch.ignore <|
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Debug" ]
                                    , name = "todo"
                                    , annotation = Nothing
                                    }
                                )
                                [ Elm.string "Invalid child index" ]
                        ]
                )

        3 ->
            let
                arg1Sort =
                    Maybe.withDefault ( [], "ERROR" ) (Array.get 0 argsArray) |> Tuple.second

                arg2Sort =
                    Maybe.withDefault ( [], "ERROR" ) (Array.get 1 argsArray) |> Tuple.second

                arg3Sort =
                    Maybe.withDefault ( [], "ERROR" ) (Array.get 2 argsArray) |> Tuple.second
            in
            Branch.variant3
                op.name
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 1 patternsArray)
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 2 patternsArray)
                (\arg1 arg2 arg3 ->
                    Elm.Case.custom
                        (Elm.val "i")
                        Type.int
                        [ Branch.int 1 <|
                            Elm.tuple
                                (Elm.apply
                                    (Elm.val "replaceCctxHole")
                                    [ Elm.val "i"
                                    , Elm.val "cctx"
                                    , Elm.apply (Elm.val <| firstCharToUpper op.synCat) [ underCursor ]
                                    ]
                                )
                                (Elm.apply (Elm.val <| "Root_" ++ arg1Sort) [ Elm.val "arg1" ])
                        , Branch.int 2 <|
                            Elm.tuple
                                (Elm.apply
                                    (Elm.val "replaceCctxHole")
                                    [ Elm.val "i"
                                    , Elm.val "cctx"
                                    , Elm.apply (Elm.val <| firstCharToUpper op.synCat) [ underCursor ]
                                    ]
                                )
                                (Elm.apply (Elm.val <| "Root_" ++ arg2Sort) [ Elm.val "arg2" ])
                        , Branch.int 3 <|
                            Elm.tuple
                                (Elm.apply
                                    (Elm.val "replaceCctxHole")
                                    [ Elm.val "i"
                                    , Elm.val "cctx"
                                    , Elm.apply (Elm.val <| firstCharToUpper op.synCat) [ underCursor ]
                                    ]
                                )
                                (Elm.apply (Elm.val <| "Root_" ++ arg3Sort) [ Elm.val "arg3" ])
                        , Branch.ignore <|
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Debug" ]
                                    , name = "todo"
                                    , annotation = Nothing
                                    }
                                )
                                [ Elm.string "Invalid child index" ]
                        ]
                )

        4 ->
            let
                arg1Sort =
                    Maybe.withDefault ( [], "ERROR" ) (Array.get 0 argsArray) |> Tuple.second

                arg2Sort =
                    Maybe.withDefault ( [], "ERROR" ) (Array.get 1 argsArray) |> Tuple.second

                arg3Sort =
                    Maybe.withDefault ( [], "ERROR" ) (Array.get 2 argsArray) |> Tuple.second

                arg4Sort =
                    Maybe.withDefault ( [], "ERROR" ) (Array.get 3 argsArray) |> Tuple.second
            in
            Branch.variant4
                op.name
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 1 patternsArray)
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 2 patternsArray)
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 3 patternsArray)
                (\arg1 arg2 arg3 arg4 ->
                    Elm.Case.custom
                        (Elm.val "i")
                        Type.int
                        [ Branch.int 1 <|
                            Elm.tuple
                                (Elm.apply
                                    (Elm.val "replaceCctxHole")
                                    [ Elm.val "i"
                                    , Elm.val "cctx"
                                    , Elm.apply (Elm.val <| firstCharToUpper op.synCat) [ underCursor ]
                                    ]
                                )
                                (Elm.apply (Elm.val <| "Root_" ++ arg1Sort) [ Elm.val "arg1" ])
                        , Branch.int 2 <|
                            Elm.tuple
                                (Elm.apply
                                    (Elm.val "replaceCctxHole")
                                    [ Elm.val "i"
                                    , Elm.val "cctx"
                                    , Elm.apply (Elm.val <| firstCharToUpper op.synCat) [ underCursor ]
                                    ]
                                )
                                (Elm.apply (Elm.val <| "Root_" ++ arg2Sort) [ Elm.val "arg2" ])
                        , Branch.int 3 <|
                            Elm.tuple
                                (Elm.apply
                                    (Elm.val "replaceCctxHole")
                                    [ Elm.val "i"
                                    , Elm.val "cctx"
                                    , Elm.apply (Elm.val <| firstCharToUpper op.synCat) [ underCursor ]
                                    ]
                                )
                                (Elm.apply (Elm.val <| "Root_" ++ arg3Sort) [ Elm.val "arg3" ])
                        , Branch.int 4 <|
                            Elm.tuple
                                (Elm.apply
                                    (Elm.val "replaceCctxHole")
                                    [ Elm.val "i"
                                    , Elm.val "cctx"
                                    , Elm.apply (Elm.val <| firstCharToUpper op.synCat) [ underCursor ]
                                    ]
                                )
                                (Elm.apply (Elm.val <| "Root_" ++ arg4Sort) [ Elm.val "arg4" ])
                        , Branch.ignore <|
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Debug" ]
                                    , name = "todo"
                                    , annotation = Nothing
                                    }
                                )
                                [ Elm.string "Invalid child index" ]
                        ]
                )

        5 ->
            let
                arg1Sort =
                    Maybe.withDefault ( [], "ERROR" ) (Array.get 0 argsArray) |> Tuple.second

                arg2Sort =
                    Maybe.withDefault ( [], "ERROR" ) (Array.get 1 argsArray) |> Tuple.second

                arg3Sort =
                    Maybe.withDefault ( [], "ERROR" ) (Array.get 2 argsArray) |> Tuple.second

                arg4Sort =
                    Maybe.withDefault ( [], "ERROR" ) (Array.get 3 argsArray) |> Tuple.second

                arg5Sort =
                    Maybe.withDefault ( [], "ERROR" ) (Array.get 4 argsArray) |> Tuple.second
            in
            Branch.variant5
                op.name
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 1 patternsArray)
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 2 patternsArray)
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 3 patternsArray)
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 4 patternsArray)
                (\arg1 arg2 arg3 arg4 arg5 ->
                    Elm.Case.custom
                        (Elm.val "i")
                        Type.int
                        [ Branch.int 1 <|
                            Elm.tuple
                                (Elm.apply
                                    (Elm.val "replaceCctxHole")
                                    [ Elm.val "i"
                                    , Elm.val "cctx"
                                    , Elm.apply (Elm.val <| firstCharToUpper op.synCat) [ underCursor ]
                                    ]
                                )
                                (Elm.apply (Elm.val <| "Root_" ++ arg1Sort) [ Elm.val "arg1" ])
                        , Branch.int 2 <|
                            Elm.tuple
                                (Elm.apply
                                    (Elm.val "replaceCctxHole")
                                    [ Elm.val "i"
                                    , Elm.val "cctx"
                                    , Elm.apply (Elm.val <| firstCharToUpper op.synCat) [ underCursor ]
                                    ]
                                )
                                (Elm.apply (Elm.val <| "Root_" ++ arg2Sort) [ Elm.val "arg2" ])
                        , Branch.int 3 <|
                            Elm.tuple
                                (Elm.apply
                                    (Elm.val "replaceCctxHole")
                                    [ Elm.val "i"
                                    , Elm.val "cctx"
                                    , Elm.apply (Elm.val <| firstCharToUpper op.synCat) [ underCursor ]
                                    ]
                                )
                                (Elm.apply (Elm.val <| "Root_" ++ arg3Sort) [ Elm.val "arg3" ])
                        , Branch.int 4 <|
                            Elm.tuple
                                (Elm.apply
                                    (Elm.val "replaceCctxHole")
                                    [ Elm.val "i"
                                    , Elm.val "cctx"
                                    , Elm.apply (Elm.val <| firstCharToUpper op.synCat) [ underCursor ]
                                    ]
                                )
                                (Elm.apply (Elm.val <| "Root_" ++ arg4Sort) [ Elm.val "arg4" ])
                        , Branch.int 5 <|
                            Elm.tuple
                                (Elm.apply
                                    (Elm.val "replaceCctxHole")
                                    [ Elm.val "i"
                                    , Elm.val "cctx"
                                    , Elm.apply (Elm.val <| firstCharToUpper op.synCat) [ underCursor ]
                                    ]
                                )
                                (Elm.apply (Elm.val <| "Root_" ++ arg5Sort) [ Elm.val "arg5" ])
                        , Branch.ignore <|
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Debug" ]
                                    , name = "todo"
                                    , annotation = Nothing
                                    }
                                )
                                [ Elm.string "Invalid child index" ]
                        ]
                )

        _ ->
            Branch.ignore <|
                Elm.apply
                    (Elm.value
                        { importFrom = [ "Debug" ]
                        , name = "todo"
                        , annotation = Nothing
                        }
                    )
                    [ Elm.string "Operators of more than 5 args not supported" ]


cctxCases :
    Syntax
    -> CCtxSyntax
    -> CLessSyntax
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
cctxCases extendedSyntax cctxSyntax clessSyntax i orig_cctx underCursor =
    Elm.Case.custom orig_cctx
        (Type.named [] "Cctx")
        (List.map
            (\cctxop ->
                if cctxop.name == "Cctx_hole" then
                    Branch.variant0 cctxop.name <|
                        Elm.Case.custom underCursor
                            (Type.named [] "CursorLess")
                            (List.map
                                getBranchFromSynCatsOp
                                clessSyntax.synCatOps
                            )

                else
                    cctxRecurseBranch cctxop
            )
            (getCctxOps cctxSyntax)
        )


getBranchPattern : Int -> Array.Array (Branch.Pattern Elm.Expression) -> Branch.Pattern Elm.Expression
getBranchPattern i array =
    Maybe.withDefault (Branch.var "Error") <| Array.get i array


getRecursionExp : Int -> Array.Array Elm.Expression -> Elm.Expression
getRecursionExp i array =
    Maybe.withDefault (Elm.val "Error") <| Array.get i array


cctxRecurseBranch : CCtxOp -> Branch.Pattern Elm.Expression
cctxRecurseBranch cctxOp =
    let
        patternsArray =
            List.indexedMap (\i x -> getPattern (i + 1) x |> Tuple.first) cctxOp.arity |> Array.fromList

        recursionExpArray =
            List.indexedMap (\i x -> getPattern (i + 1) x |> Tuple.second) cctxOp.arity |> Array.fromList
    in
    case List.length cctxOp.arity of
        0 ->
            Branch.var "ERROR"

        1 ->
            Branch.variant1
                cctxOp.name
                (Branch.var "cctx")
                (\cctx ->
                    Elm.apply
                        (Elm.val <| firstCharToUpper cctxOp.name)
                        [ Elm.apply
                            (Elm.val "replaceCctxHole")
                            [ Elm.val "i", cctx, Elm.val "underCursor" ]
                        ]
                )

        2 ->
            let
                cctx_i =
                    -- get last char in op name indicating which cctx to replace
                    Maybe.withDefault 1 <| String.toInt <| String.right 1 cctxOp.name
            in
            case cctx_i of
                1 ->
                    Branch.variant2
                        cctxOp.name
                        (getBranchPattern 0 patternsArray)
                        (getBranchPattern 1 patternsArray)
                        (\arg1 arg2 ->
                            Elm.apply
                                (Elm.val <| firstCharToUpper cctxOp.name)
                                [ getRecursionExp 0 recursionExpArray
                                , arg2
                                ]
                        )

                2 ->
                    Branch.variant2
                        cctxOp.name
                        (getBranchPattern 0 patternsArray)
                        (getBranchPattern 1 patternsArray)
                        (\arg1 arg2 ->
                            Elm.apply
                                (Elm.val <| firstCharToUpper cctxOp.name)
                                [ arg1
                                , getRecursionExp 1 recursionExpArray
                                ]
                        )

                _ ->
                    Branch.var "ERROR"

        _ ->
            Branch.var "ERROR"


getCursorLessOps : CLessSyntax -> List Operator
getCursorLessOps syntax =
    List.concatMap (\x -> x.ops) syntax.synCatOps


getCursorLessSynCatOps : Syntax -> List SynCatOps
getCursorLessSynCatOps syntax =
    List.filter (\x -> String.endsWith "_CLess" x.synCat) syntax.synCatOps


getCctxOps : CCtxSyntax -> List CCtxOp
getCctxOps syntax =
    List.concatMap (\x -> x.ops) <|
        List.filter (\x -> x.synCat == "cctx") syntax.synCatOps


getBranchFromSynCatsOp : SynCatOps -> Branch.Branch
getBranchFromSynCatsOp synCatOp =
    Branch.variant1
        synCatOp.synCat
        (Branch.var "underCursor")
        (\under_cursor ->
            Elm.Case.custom under_cursor
                (Type.named [] (firstCharToUpper synCatOp.synCat))
                (List.map
                    getBranchFromOp
                    synCatOp.ops
                )
        )


getBranchFromOp : Operator -> Branch.Branch
getBranchFromOp op =
    let
        patterns =
            List.indexedMap
                (\i ( boundVars, arg ) ->
                    case getPatternAndReplacement (i + 1) ( boundVars, arg ) of
                        Just pattern ->
                            pattern

                        Nothing ->
                            ( Branch.var ("arg" ++ String.fromInt (i + 1))
                            , Elm.val "Cctx_hole"
                            )
                )
                op.arity

        patternsArray =
            Array.fromList (List.map Tuple.first patterns)

        replacementArray =
            Array.fromList (List.map Tuple.second patterns)

        argsArray =
            op.arity |> Array.fromList
    in
    case List.length op.arity of
        0 ->
            Branch.variant0 op.name
                (Elm.apply
                    (Elm.value
                        { importFrom = [ "Debug" ]
                        , name = "todo"
                        , annotation = Nothing
                        }
                    )
                    [ Elm.string "Invalid replacement" ]
                )

        1 ->
            Branch.variant1
                op.name
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                (\arg ->
                    let
                        argSort =
                            Maybe.withDefault ( [], "ERROR" ) (Array.get 0 argsArray) |> Tuple.second
                    in
                    Elm.Case.custom (Elm.val "i")
                        Type.int
                        [ Branch.int 1 <|
                            Elm.apply
                                (Elm.val <| firstCharToUpper <| op.name ++ "_cctx1")
                                [ Elm.val "Cctx_hole" ]
                        , Branch.ignore <|
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Debug" ]
                                    , name = "todo"
                                    , annotation = Nothing
                                    }
                                )
                                [ Elm.string "Invalid arg position" ]
                        ]
                )

        2 ->
            let
                replacement1 =
                    Maybe.withDefault (Elm.val "ERROR") <| Array.get 0 replacementArray

                replacement2 =
                    Maybe.withDefault (Elm.val "ERROR") <| Array.get 1 replacementArray
            in
            Branch.variant2
                op.name
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 1 patternsArray)
                (\arg1 arg2 ->
                    Elm.Case.custom (Elm.val "i")
                        Type.int
                        [ Branch.int 1 <|
                            Elm.apply
                                (Elm.val <| firstCharToUpper <| op.name ++ "_cctx1")
                                [ replacement1, arg2 ]
                        , Branch.int 2 <|
                            Elm.apply
                                (Elm.val <| firstCharToUpper <| op.name ++ "_cctx2")
                                [ arg1, replacement2 ]
                        , Branch.ignore <|
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Debug" ]
                                    , name = "todo"
                                    , annotation = Nothing
                                    }
                                )
                                [ Elm.string "Invalid arg position" ]
                        ]
                )

        3 ->
            let
                replacement1 =
                    Maybe.withDefault (Elm.val "ERROR") <| Array.get 0 replacementArray

                replacement2 =
                    Maybe.withDefault (Elm.val "ERROR") <| Array.get 1 replacementArray

                replacement3 =
                    Maybe.withDefault (Elm.val "ERROR") <| Array.get 2 replacementArray
            in
            Branch.variant3
                op.name
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 1 patternsArray)
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 2 patternsArray)
                (\arg1 arg2 arg3 ->
                    Elm.Case.custom (Elm.val "i")
                        Type.int
                        [ Branch.int 1 <|
                            Elm.apply
                                (Elm.val <| firstCharToUpper <| op.name ++ "_cctx1")
                                [ replacement1, arg2, arg3 ]
                        , Branch.int 2 <|
                            Elm.apply
                                (Elm.val <| firstCharToUpper <| op.name ++ "_cctx2")
                                [ arg1, replacement2, arg3 ]
                        , Branch.int 3 <|
                            Elm.apply
                                (Elm.val <| firstCharToUpper <| op.name ++ "_cctx3")
                                [ arg1, arg2, replacement3 ]
                        , Branch.ignore <|
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Debug" ]
                                    , name = "todo"
                                    , annotation = Nothing
                                    }
                                )
                                [ Elm.string "Invalid arg position" ]
                        ]
                )

        4 ->
            let
                replacement1 =
                    Maybe.withDefault (Elm.val "ERROR") <| Array.get 0 replacementArray

                replacement2 =
                    Maybe.withDefault (Elm.val "ERROR") <| Array.get 1 replacementArray

                replacement3 =
                    Maybe.withDefault (Elm.val "ERROR") <| Array.get 2 replacementArray

                replacement4 =
                    Maybe.withDefault (Elm.val "ERROR") <| Array.get 3 replacementArray
            in
            Branch.variant4
                op.name
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 1 patternsArray)
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 2 patternsArray)
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 3 patternsArray)
                (\arg1 arg2 arg3 arg4 ->
                    Elm.Case.custom (Elm.val "i")
                        Type.int
                        [ Branch.int 1 <|
                            Elm.apply
                                (Elm.val <| firstCharToUpper <| op.name ++ "_cctx1")
                                [ replacement1, arg2, arg3, arg4 ]
                        , Branch.int 2 <|
                            Elm.apply
                                (Elm.val <| firstCharToUpper <| op.name ++ "_cctx2")
                                [ arg1, replacement2, arg3, arg4 ]
                        , Branch.int 3 <|
                            Elm.apply
                                (Elm.val <| firstCharToUpper <| op.name ++ "_cctx3")
                                [ arg1, arg2, replacement3, arg4 ]
                        , Branch.int 4 <|
                            Elm.apply
                                (Elm.val <| firstCharToUpper <| op.name ++ "_cctx4")
                                [ arg1, arg2, arg3, replacement4 ]
                        , Branch.ignore <|
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Debug" ]
                                    , name = "todo"
                                    , annotation = Nothing
                                    }
                                )
                                [ Elm.string "Invalid arg position" ]
                        ]
                )

        5 ->
            let
                replacement1 =
                    Maybe.withDefault (Elm.val "ERROR") <| Array.get 0 replacementArray

                replacement2 =
                    Maybe.withDefault (Elm.val "ERROR") <| Array.get 1 replacementArray

                replacement3 =
                    Maybe.withDefault (Elm.val "ERROR") <| Array.get 2 replacementArray

                replacement4 =
                    Maybe.withDefault (Elm.val "ERROR") <| Array.get 3 replacementArray

                replacement5 =
                    Maybe.withDefault (Elm.val "ERROR") <| Array.get 4 replacementArray
            in
            Branch.variant5
                op.name
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 1 patternsArray)
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 2 patternsArray)
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 3 patternsArray)
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 4 patternsArray)
                (\arg1 arg2 arg3 arg4 arg5 ->
                    Elm.Case.custom (Elm.val "i")
                        Type.int
                        [ Branch.int 1 <|
                            Elm.apply
                                (Elm.val <| firstCharToUpper <| op.name ++ "_cctx1")
                                [ replacement1, arg2, arg3, arg4, arg5 ]
                        , Branch.int 2 <|
                            Elm.apply
                                (Elm.val <| firstCharToUpper <| op.name ++ "_cctx2")
                                [ arg1, replacement2, arg3, arg4, arg5 ]
                        , Branch.int 3 <|
                            Elm.apply
                                (Elm.val <| firstCharToUpper <| op.name ++ "_cctx3")
                                [ arg1, arg2, replacement3, arg4, arg5 ]
                        , Branch.int 4 <|
                            Elm.apply
                                (Elm.val <| firstCharToUpper <| op.name ++ "_cctx4")
                                [ arg1, arg2, arg3, replacement4, arg5 ]
                        , Branch.int 5 <|
                            Elm.apply
                                (Elm.val <| firstCharToUpper <| op.name ++ "_cctx5")
                                [ arg1, arg2, arg3, arg4, replacement5 ]
                        , Branch.ignore <|
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Debug" ]
                                    , name = "todo"
                                    , annotation = Nothing
                                    }
                                )
                                [ Elm.string "Invalid arg position" ]
                        ]
                )

        _ ->
            Debug.todo "More than 5 args not supported"


getPattern : Int -> ( List String, String ) -> ( Branch.Pattern Elm.Expression, Elm.Expression )
getPattern i arg =
    case arg of
        ( [], arg_ ) ->
            ( Branch.var <| "arg" ++ String.fromInt i
            , Elm.apply
                (Elm.val "replaceCctxHole")
                [ Elm.val "i"
                , Elm.val <| "arg" ++ String.fromInt i
                , Elm.val "underCursor"
                ]
            )

        ( boundVars, arg_ ) ->
            ( Branch.var <|
                "(boundVars"
                    ++ String.fromInt i
                    ++ ", arg"
                    ++ String.fromInt i
                    ++ ")"
            , Elm.tuple
                (Elm.val <| "boundVars" ++ String.fromInt i)
                (Elm.apply
                    (Elm.val "replaceCctxHole")
                    [ Elm.val "i"
                    , Elm.val <| "arg" ++ String.fromInt i
                    , Elm.val "underCursor"
                    ]
                )
            )


getPattern_ : Int -> ( List String, String ) -> Branch.Pattern Elm.Expression
getPattern_ i arg =
    case arg of
        ( [], arg_ ) ->
            Branch.var <| "arg" ++ String.fromInt i

        ( boundVars, arg_ ) ->
            Branch.var <| "(boundVars" ++ String.fromInt i ++ ", arg" ++ String.fromInt i ++ ")"


getPatternAndReplacement : Int -> ( List String, String ) -> Maybe ( Branch.Pattern Elm.Expression, Elm.Expression )
getPatternAndReplacement i arg =
    case arg of
        ( [], arg_ ) ->
            Nothing

        ( boundVars, arg_ ) ->
            Just
                ( Branch.var <| "(boundVars" ++ String.fromInt i ++ ", arg" ++ String.fromInt i ++ ")"
                , Elm.tuple (Elm.val <| "boundVars" ++ String.fromInt i) (Elm.val "Cctx_hole")
                )
