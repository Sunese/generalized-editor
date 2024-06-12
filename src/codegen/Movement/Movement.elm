module Movement.Movement exposing (..)

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
import Syntax.RawSyntaxP exposing (..)
import Syntax.Syntax exposing (..)


createChildFun : Syntax -> CLessSyntax -> CCtxSyntax -> WellFormedSyntax -> Elm.Declaration
createChildFun extendedSyntax clessSyntax cctxSyntax wellformedSyntax =
    Elm.declaration "child" <|
        Elm.withType
            (Type.function
                [ Type.int
                , Type.tuple (Type.named [ "Syntax", "CCtx" ] "Cctx") (Type.named [ "Syntax", "Wellformed" ] "Wellformed")
                ]
                (Type.maybe <| Type.tuple (Type.named [ "Syntax", "CCtx" ] "Cctx") (Type.named [ "Syntax", "Wellformed" ] "Wellformed"))
            )
        <|
            Elm.fn2
                ( "i", Just Type.int )
                ( "decomposed", Just <| Type.tuple (Type.named [ "Syntax", "CCtx" ] "Cctx") (Type.named [ "Syntax", "Wellformed" ] "Wellformed") )
                (\i decomposed ->
                    Elm.Let.letIn
                        (\( cctx, wellformed ) ->
                            Elm.Case.custom
                                wellformed
                                (Type.named [ "Syntax", "Wellformed" ] "Wellformed")
                                (wellformedCases
                                    clessSyntax
                                    (List.filter (\x -> x.synCat == "wellformed") wellformedSyntax.synCatOps)
                                )
                        )
                        |> Elm.Let.tuple "cctx" "wellformed" (Elm.val "decomposed")
                        |> Elm.Let.toExpression
                )


createReplaceCCtxHoleFun : Syntax -> CLessSyntax -> CCtxSyntax -> Elm.Declaration
createReplaceCCtxHoleFun extendedSyntax clessSyntax cctxSyntax =
    Elm.declaration "replaceCctxHole" <|
        Elm.withType (Type.function [ Type.int, Type.named [ "Syntax", "CCtx" ] "Cctx", Type.named [ "Syntax", "Cursorless" ] "CursorLess" ] (Type.named [ "Syntax", "CCtx" ] "Cctx")) <|
            Elm.fn3
                ( "i", Just Type.int )
                ( "orig_cctx", Just <| Type.named [ "Syntax", "CCtx" ] "Cctx" )
                ( "underCursor", Just <| Type.named [ "Syntax", "Cursorless" ] "CursorLess" )
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
            in
            Branch.variant1
                op.name
                (Branch.var "underCursor")
                (\underCursor ->
                    Elm.Case.custom underCursor
                        (Type.named [ "Syntax", "Cursorless" ] (firstCharToUpper clessName))
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
            case op.literal of
                Nothing ->
                    Branch.variant0 op.name <| Elm.nothing

                Just lit ->
                    Branch.variant1 op.name (Branch.var "lit") <|
                        \x -> Elm.nothing

        1 ->
            let
                arg1Sort =
                    Maybe.withDefault ( [], "ERROR" ) (Array.get 0 argsArray) |> Tuple.second

                -- x =
                --     Debug.todo arg1Sort
            in
            Branch.variant1
                op.name
                (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                (\arg ->
                    Elm.Case.custom
                        (Elm.val "i")
                        Type.int
                    <|
                        [ Branch.int 1 <|
                            Elm.just <|
                                Elm.tuple
                                    (Elm.withType (Type.named [] "Cctx") <|
                                        Elm.apply
                                            (Elm.val "replaceCctxHole")
                                            [ Elm.val "i"
                                            , Elm.val "cctx"
                                            , Elm.apply
                                                (Elm.value
                                                    { importFrom = [ "Syntax", "Cursorless" ]
                                                    , name = firstCharToUpper op.synCat
                                                    , annotation = Nothing
                                                    }
                                                )
                                                [ underCursor ]
                                            ]
                                    )
                                    (Elm.withType
                                        (Type.named [] "Wellformed")
                                     <|
                                        Elm.apply
                                            (Elm.value
                                                { importFrom = [ "Syntax", "Wellformed" ]
                                                , name = "Root_" ++ arg1Sort
                                                , annotation = Nothing
                                                }
                                            )
                                            [ Elm.val "arg1" ]
                                    )

                        -- (Elm.apply (Elm.val <| "Root_" ++ arg1Sort) [ Elm.val "arg1" ])
                        -- (Elm.apply
                        --     (Elm.val <| "Root_" ++ arg1Sort)
                        --     [ Elm.val "arg1" ]
                        -- )
                        , Branch.ignore <|
                            -- setting it like this instead of
                            -- Elm.ignore prevents the codegen
                            -- from hanging
                            Elm.nothing
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
                            Elm.just <|
                                Elm.tuple
                                    (Elm.withType (Type.named [] "Cctx") <|
                                        Elm.apply
                                            (Elm.val "replaceCctxHole")
                                            [ Elm.val "i"
                                            , Elm.val "cctx"
                                            , Elm.apply
                                                (Elm.value
                                                    { importFrom = [ "Syntax", "Cursorless" ]
                                                    , name = firstCharToUpper op.synCat
                                                    , annotation = Nothing
                                                    }
                                                )
                                                [ underCursor ]
                                            ]
                                    )
                                    (Elm.withType (Type.named [] "Wellformed") <|
                                        Elm.apply
                                            (Elm.value
                                                { importFrom = [ "Syntax", "Wellformed" ]
                                                , name = "Root_" ++ arg1Sort
                                                , annotation = Nothing
                                                }
                                            )
                                            [ Elm.val "arg1" ]
                                    )
                        , Branch.int 2 <|
                            Elm.just <|
                                Elm.tuple
                                    (Elm.withType (Type.named [] "Cctx") <|
                                        Elm.apply
                                            (Elm.val "replaceCctxHole")
                                            [ Elm.val "i"
                                            , Elm.val "cctx"
                                            , Elm.apply
                                                (Elm.value
                                                    { importFrom = [ "Syntax", "Cursorless" ]
                                                    , name = firstCharToUpper op.synCat
                                                    , annotation = Nothing
                                                    }
                                                )
                                                [ underCursor ]
                                            ]
                                    )
                                    (Elm.withType (Type.named [] "Wellformed") <|
                                        Elm.apply
                                            (Elm.value
                                                { importFrom = [ "Syntax", "Wellformed" ]
                                                , name = "Root_" ++ arg2Sort
                                                , annotation = Nothing
                                                }
                                            )
                                            [ Elm.val "arg2" ]
                                    )
                        , Branch.ignore <| Elm.nothing
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
                            Elm.just <|
                                Elm.tuple
                                    (Elm.withType (Type.named [] "Cctx") <|
                                        Elm.apply
                                            (Elm.val "replaceCctxHole")
                                            [ Elm.val "i"
                                            , Elm.val "cctx"
                                            , Elm.apply
                                                (Elm.value
                                                    { importFrom = [ "Syntax", "Cursorless" ]
                                                    , name = firstCharToUpper op.synCat
                                                    , annotation = Nothing
                                                    }
                                                )
                                                [ underCursor ]
                                            ]
                                    )
                                    (Elm.withType (Type.named [] "Wellformed") <|
                                        Elm.apply
                                            (Elm.value
                                                { importFrom = [ "Syntax", "Wellformed" ]
                                                , name = "Root_" ++ arg1Sort
                                                , annotation = Nothing
                                                }
                                            )
                                            [ Elm.val "arg1" ]
                                    )
                        , Branch.int 2 <|
                            Elm.just <|
                                Elm.tuple
                                    (Elm.withType (Type.named [] "Cctx") <|
                                        Elm.apply
                                            (Elm.val "replaceCctxHole")
                                            [ Elm.val "i"
                                            , Elm.val "cctx"
                                            , Elm.apply
                                                (Elm.value
                                                    { importFrom = [ "Syntax", "Cursorless" ]
                                                    , name = firstCharToUpper op.synCat
                                                    , annotation = Nothing
                                                    }
                                                )
                                                [ underCursor ]
                                            ]
                                    )
                                    (Elm.withType (Type.named [] "Wellformed") <|
                                        Elm.apply
                                            (Elm.value
                                                { importFrom = [ "Syntax", "Wellformed" ]
                                                , name = "Root_" ++ arg2Sort
                                                , annotation = Nothing
                                                }
                                            )
                                            [ Elm.val "arg2" ]
                                    )
                        , Branch.int 3 <|
                            Elm.just <|
                                Elm.tuple
                                    (Elm.withType (Type.named [] "Cctx") <|
                                        Elm.apply
                                            (Elm.val "replaceCctxHole")
                                            [ Elm.val "i"
                                            , Elm.val "cctx"
                                            , Elm.apply
                                                (Elm.value
                                                    { importFrom = [ "Syntax", "Cursorless" ]
                                                    , name = firstCharToUpper op.synCat
                                                    , annotation = Nothing
                                                    }
                                                )
                                                [ underCursor ]
                                            ]
                                    )
                                    (Elm.withType (Type.named [] "Wellformed") <|
                                        Elm.apply
                                            (Elm.value
                                                { importFrom = [ "Syntax", "Wellformed" ]
                                                , name = "Root_" ++ arg3Sort
                                                , annotation = Nothing
                                                }
                                            )
                                            [ Elm.val "arg3" ]
                                    )
                        , Branch.ignore <| Elm.nothing
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
                            Elm.just <|
                                Elm.tuple
                                    (Elm.withType (Type.named [] "Cctx") <|
                                        Elm.apply
                                            (Elm.val "replaceCctxHole")
                                            [ Elm.val "i"
                                            , Elm.val "cctx"
                                            , Elm.apply
                                                (Elm.value
                                                    { importFrom = [ "Syntax", "Cursorless" ]
                                                    , name = firstCharToUpper op.synCat
                                                    , annotation = Nothing
                                                    }
                                                )
                                                [ underCursor ]
                                            ]
                                    )
                                    (Elm.withType (Type.named [] "Wellformed") <|
                                        Elm.apply
                                            (Elm.value
                                                { importFrom = [ "Syntax", "Wellformed" ]
                                                , name = "Root_" ++ arg1Sort
                                                , annotation = Nothing
                                                }
                                            )
                                            [ Elm.val "arg1" ]
                                    )
                        , Branch.int 2 <|
                            Elm.just <|
                                Elm.tuple
                                    (Elm.withType (Type.named [] "Cctx") <|
                                        Elm.apply
                                            (Elm.val "replaceCctxHole")
                                            [ Elm.val "i"
                                            , Elm.val "cctx"
                                            , Elm.apply
                                                (Elm.value
                                                    { importFrom = [ "Syntax", "Cursorless" ]
                                                    , name = firstCharToUpper op.synCat
                                                    , annotation = Nothing
                                                    }
                                                )
                                                [ underCursor ]
                                            ]
                                    )
                                    (Elm.withType (Type.named [] "Wellformed") <|
                                        Elm.apply
                                            (Elm.value
                                                { importFrom = [ "Syntax", "Wellformed" ]
                                                , name = "Root_" ++ arg2Sort
                                                , annotation = Nothing
                                                }
                                            )
                                            [ Elm.val "arg2" ]
                                    )
                        , Branch.int 3 <|
                            Elm.just <|
                                Elm.tuple
                                    (Elm.withType (Type.named [] "Cctx") <|
                                        Elm.apply
                                            (Elm.val "replaceCctxHole")
                                            [ Elm.val "i"
                                            , Elm.val "cctx"
                                            , Elm.apply
                                                (Elm.value
                                                    { importFrom = [ "Syntax", "Cursorless" ]
                                                    , name = firstCharToUpper op.synCat
                                                    , annotation = Nothing
                                                    }
                                                )
                                                [ underCursor ]
                                            ]
                                    )
                                    (Elm.withType (Type.named [] "Wellformed") <|
                                        Elm.apply
                                            (Elm.value
                                                { importFrom = [ "Syntax", "Wellformed" ]
                                                , name = "Root_" ++ arg3Sort
                                                , annotation = Nothing
                                                }
                                            )
                                            [ Elm.val "arg3" ]
                                    )
                        , Branch.int 4 <|
                            Elm.just <|
                                Elm.tuple
                                    (Elm.withType (Type.named [] "Cctx") <|
                                        Elm.apply
                                            (Elm.val "replaceCctxHole")
                                            [ Elm.val "i"
                                            , Elm.val "cctx"
                                            , Elm.apply
                                                (Elm.value
                                                    { importFrom = [ "Syntax", "Cursorless" ]
                                                    , name = firstCharToUpper op.synCat
                                                    , annotation = Nothing
                                                    }
                                                )
                                                [ underCursor ]
                                            ]
                                    )
                                    (Elm.withType (Type.named [] "Wellformed") <|
                                        Elm.apply
                                            (Elm.value
                                                { importFrom = [ "Syntax", "Wellformed" ]
                                                , name = "Root_" ++ arg4Sort
                                                , annotation = Nothing
                                                }
                                            )
                                            [ Elm.val "arg4" ]
                                    )
                        , Branch.ignore <| Elm.nothing
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
                            Elm.just <|
                                Elm.tuple
                                    (Elm.withType (Type.named [] "Cctx") <|
                                        Elm.apply
                                            (Elm.val "replaceCctxHole")
                                            [ Elm.val "i"
                                            , Elm.val "cctx"
                                            , Elm.apply
                                                (Elm.value
                                                    { importFrom = [ "Syntax", "Cursorless" ]
                                                    , name = firstCharToUpper op.synCat
                                                    , annotation = Nothing
                                                    }
                                                )
                                                [ underCursor ]
                                            ]
                                    )
                                    (Elm.withType (Type.named [] "Wellformed") <|
                                        Elm.apply
                                            (Elm.value
                                                { importFrom = [ "Syntax", "Wellformed" ]
                                                , name = "Root_" ++ arg1Sort
                                                , annotation = Nothing
                                                }
                                            )
                                            [ Elm.val "arg1" ]
                                    )
                        , Branch.int 2 <|
                            Elm.just <|
                                Elm.tuple
                                    (Elm.withType (Type.named [] "Cctx") <|
                                        Elm.apply
                                            (Elm.val "replaceCctxHole")
                                            [ Elm.val "i"
                                            , Elm.val "cctx"
                                            , Elm.apply
                                                (Elm.value
                                                    { importFrom = [ "Syntax", "Cursorless" ]
                                                    , name = firstCharToUpper op.synCat
                                                    , annotation = Nothing
                                                    }
                                                )
                                                [ underCursor ]
                                            ]
                                    )
                                    (Elm.withType (Type.named [] "Wellformed") <|
                                        Elm.apply
                                            (Elm.value
                                                { importFrom = [ "Syntax", "Wellformed" ]
                                                , name = "Root_" ++ arg2Sort
                                                , annotation = Nothing
                                                }
                                            )
                                            [ Elm.val "arg2" ]
                                    )
                        , Branch.int 3 <|
                            Elm.just <|
                                Elm.tuple
                                    (Elm.withType (Type.named [] "Cctx") <|
                                        Elm.apply
                                            (Elm.val "replaceCctxHole")
                                            [ Elm.val "i"
                                            , Elm.val "cctx"
                                            , Elm.apply
                                                (Elm.value
                                                    { importFrom = [ "Syntax", "Cursorless" ]
                                                    , name = firstCharToUpper op.synCat
                                                    , annotation = Nothing
                                                    }
                                                )
                                                [ underCursor ]
                                            ]
                                    )
                                    (Elm.withType (Type.named [] "Wellformed") <|
                                        Elm.apply
                                            (Elm.value
                                                { importFrom = [ "Syntax", "Wellformed" ]
                                                , name = "Root_" ++ arg3Sort
                                                , annotation = Nothing
                                                }
                                            )
                                            [ Elm.val "arg3" ]
                                    )
                        , Branch.int 4 <|
                            Elm.just <|
                                Elm.tuple
                                    (Elm.withType (Type.named [] "Cctx") <|
                                        Elm.apply
                                            (Elm.val "replaceCctxHole")
                                            [ Elm.val "i"
                                            , Elm.val "cctx"
                                            , Elm.apply
                                                (Elm.value
                                                    { importFrom = [ "Syntax", "Cursorless" ]
                                                    , name = firstCharToUpper op.synCat
                                                    , annotation = Nothing
                                                    }
                                                )
                                                [ underCursor ]
                                            ]
                                    )
                                    (Elm.withType (Type.named [] "Wellformed") <|
                                        Elm.apply
                                            (Elm.value
                                                { importFrom = [ "Syntax", "Wellformed" ]
                                                , name = "Root_" ++ arg4Sort
                                                , annotation = Nothing
                                                }
                                            )
                                            [ Elm.val "arg4" ]
                                    )
                        , Branch.int 5 <|
                            Elm.just <|
                                Elm.tuple
                                    (Elm.withType (Type.named [] "Cctx") <|
                                        Elm.apply
                                            (Elm.val "replaceCctxHole")
                                            [ Elm.val "i"
                                            , Elm.val "cctx"
                                            , Elm.apply
                                                (Elm.value
                                                    { importFrom = [ "Syntax", "Cursorless" ]
                                                    , name = firstCharToUpper op.synCat
                                                    , annotation = Nothing
                                                    }
                                                )
                                                [ underCursor ]
                                            ]
                                    )
                                    (Elm.withType (Type.named [] "Wellformed") <|
                                        Elm.apply
                                            (Elm.value
                                                { importFrom = [ "Syntax", "Wellformed" ]
                                                , name = "Root_" ++ arg5Sort
                                                , annotation = Nothing
                                                }
                                            )
                                            [ Elm.val "arg5" ]
                                    )
                        , Branch.ignore <| Elm.nothing
                        ]
                )

        _ ->
            Debug.todo "ops with more than 5 args not supported"


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
        (Type.named [ "Syntax", "CCtx" ] "Cctx")
        (List.map
            (\cctxop ->
                if cctxop.name == "Cctx_hole" then
                    Branch.variant0 cctxop.name <|
                        Elm.Case.custom underCursor
                            (Type.named [ "Syntax", "Cursorless" ] "CursorLess")
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
            Debug.todo "no cctx op has 0 arity"

        1 ->
            Branch.variant1
                cctxOp.name
                (Branch.var "cctx")
                (\cctx ->
                    Elm.apply
                        (Elm.value
                            { importFrom = [ "Syntax", "CCtx" ]
                            , name = firstCharToUpper cctxOp.name
                            , annotation = Nothing
                            }
                        )
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
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper cctxOp.name
                                    , annotation = Nothing
                                    }
                                )
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
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper cctxOp.name
                                    , annotation = Nothing
                                    }
                                )
                                [ arg1
                                , getRecursionExp 1 recursionExpArray
                                ]
                        )

                _ ->
                    Debug.todo "op has arity 2 but cctx_i is not 1 or 2"

        3 ->
            let
                cctx_i =
                    -- get last char in op name indicating which cctx to replace
                    Maybe.withDefault 1 <| String.toInt <| String.right 1 cctxOp.name
            in
            case cctx_i of
                1 ->
                    Branch.variant3
                        cctxOp.name
                        (getBranchPattern 0 patternsArray)
                        (getBranchPattern 1 patternsArray)
                        (getBranchPattern 2 patternsArray)
                        (\arg1 arg2 arg3 ->
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper cctxOp.name
                                    , annotation = Nothing
                                    }
                                )
                                [ getRecursionExp 0 recursionExpArray
                                , arg2
                                , arg3
                                ]
                        )

                2 ->
                    Branch.variant3
                        cctxOp.name
                        (getBranchPattern 0 patternsArray)
                        (getBranchPattern 1 patternsArray)
                        (getBranchPattern 2 patternsArray)
                        (\arg1 arg2 arg3 ->
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper cctxOp.name
                                    , annotation = Nothing
                                    }
                                )
                                [ arg1
                                , getRecursionExp 1 recursionExpArray
                                , arg3
                                ]
                        )

                3 ->
                    Branch.variant3
                        cctxOp.name
                        (getBranchPattern 0 patternsArray)
                        (getBranchPattern 1 patternsArray)
                        (getBranchPattern 2 patternsArray)
                        (\arg1 arg2 arg3 ->
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper cctxOp.name
                                    , annotation = Nothing
                                    }
                                )
                                [ arg1
                                , arg2
                                , getRecursionExp 2 recursionExpArray
                                ]
                        )

                _ ->
                    Debug.todo "op has arity 3 but cctx_i is not 1, 2 or 3"

        4 ->
            let
                cctx_i =
                    -- get last char in op name indicating which cctx to replace
                    Maybe.withDefault 1 <| String.toInt <| String.right 1 cctxOp.name
            in
            case cctx_i of
                1 ->
                    Branch.variant4
                        cctxOp.name
                        (getBranchPattern 0 patternsArray)
                        (getBranchPattern 1 patternsArray)
                        (getBranchPattern 2 patternsArray)
                        (getBranchPattern 3 patternsArray)
                        (\arg1 arg2 arg3 arg4 ->
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper cctxOp.name
                                    , annotation = Nothing
                                    }
                                )
                                [ getRecursionExp 0 recursionExpArray
                                , arg2
                                , arg3
                                , arg4
                                ]
                        )

                2 ->
                    Branch.variant4
                        cctxOp.name
                        (getBranchPattern 0 patternsArray)
                        (getBranchPattern 1 patternsArray)
                        (getBranchPattern 2 patternsArray)
                        (getBranchPattern 3 patternsArray)
                        (\arg1 arg2 arg3 arg4 ->
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper cctxOp.name
                                    , annotation = Nothing
                                    }
                                )
                                [ arg1
                                , getRecursionExp 1 recursionExpArray
                                , arg3
                                , arg4
                                ]
                        )

                3 ->
                    Branch.variant4
                        cctxOp.name
                        (getBranchPattern 0 patternsArray)
                        (getBranchPattern 1 patternsArray)
                        (getBranchPattern 2 patternsArray)
                        (getBranchPattern 3 patternsArray)
                        (\arg1 arg2 arg3 arg4 ->
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper cctxOp.name
                                    , annotation = Nothing
                                    }
                                )
                                [ arg1
                                , arg2
                                , getRecursionExp 2 recursionExpArray
                                , arg4
                                ]
                        )

                4 ->
                    Branch.variant4
                        cctxOp.name
                        (getBranchPattern 0 patternsArray)
                        (getBranchPattern 1 patternsArray)
                        (getBranchPattern 2 patternsArray)
                        (getBranchPattern 3 patternsArray)
                        (\arg1 arg2 arg3 arg4 ->
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper cctxOp.name
                                    , annotation = Nothing
                                    }
                                )
                                [ arg1
                                , arg2
                                , arg3
                                , getRecursionExp 3 recursionExpArray
                                ]
                        )

                _ ->
                    Debug.todo "op has arity 4 but cctx_i is not 1, 2, 3 or 4"

        5 ->
            let
                cctx_i =
                    -- get last char in op name indicating which cctx to replace
                    Maybe.withDefault 1 <| String.toInt <| String.right 1 cctxOp.name
            in
            case cctx_i of
                1 ->
                    Branch.variant5
                        cctxOp.name
                        (getBranchPattern 0 patternsArray)
                        (getBranchPattern 1 patternsArray)
                        (getBranchPattern 2 patternsArray)
                        (getBranchPattern 3 patternsArray)
                        (getBranchPattern 4 patternsArray)
                        (\arg1 arg2 arg3 arg4 arg5 ->
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper cctxOp.name
                                    , annotation = Nothing
                                    }
                                )
                                [ getRecursionExp 0 recursionExpArray
                                , arg2
                                , arg3
                                , arg4
                                , arg5
                                ]
                        )

                2 ->
                    Branch.variant5
                        cctxOp.name
                        (getBranchPattern 0 patternsArray)
                        (getBranchPattern 1 patternsArray)
                        (getBranchPattern 2 patternsArray)
                        (getBranchPattern 3 patternsArray)
                        (getBranchPattern 4 patternsArray)
                        (\arg1 arg2 arg3 arg4 arg5 ->
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper cctxOp.name
                                    , annotation = Nothing
                                    }
                                )
                                [ arg1
                                , getRecursionExp 1 recursionExpArray
                                , arg3
                                , arg4
                                , arg5
                                ]
                        )

                3 ->
                    Branch.variant5
                        cctxOp.name
                        (getBranchPattern 0 patternsArray)
                        (getBranchPattern 1 patternsArray)
                        (getBranchPattern 2 patternsArray)
                        (getBranchPattern 3 patternsArray)
                        (getBranchPattern 4 patternsArray)
                        (\arg1 arg2 arg3 arg4 arg5 ->
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper cctxOp.name
                                    , annotation = Nothing
                                    }
                                )
                                [ arg1
                                , arg2
                                , getRecursionExp 2 recursionExpArray
                                , arg4
                                , arg5
                                ]
                        )

                4 ->
                    Branch.variant5
                        cctxOp.name
                        (getBranchPattern 0 patternsArray)
                        (getBranchPattern 1 patternsArray)
                        (getBranchPattern 2 patternsArray)
                        (getBranchPattern 3 patternsArray)
                        (getBranchPattern 4 patternsArray)
                        (\arg1 arg2 arg3 arg4 arg5 ->
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper cctxOp.name
                                    , annotation = Nothing
                                    }
                                )
                                [ arg1
                                , arg2
                                , arg3
                                , getRecursionExp 3 recursionExpArray
                                , arg5
                                ]
                        )

                5 ->
                    Branch.variant5
                        cctxOp.name
                        (getBranchPattern 0 patternsArray)
                        (getBranchPattern 1 patternsArray)
                        (getBranchPattern 2 patternsArray)
                        (getBranchPattern 3 patternsArray)
                        (getBranchPattern 4 patternsArray)
                        (\arg1 arg2 arg3 arg4 arg5 ->
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper cctxOp.name
                                    , annotation = Nothing
                                    }
                                )
                                [ arg1
                                , arg2
                                , arg3
                                , arg4
                                , getRecursionExp 4 recursionExpArray
                                ]
                        )

                _ ->
                    Debug.todo "op has arity 5 but cctx_i is not 1, 2, 3, 4 or 5"

        _ ->
            Debug.todo "Ops with arity > 5 not supported"


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
                (Type.named [ "Syntax", "Cursorless" ] (firstCharToUpper synCatOp.synCat))
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
                            , Elm.value
                                { importFrom = [ "Syntax", "CCtx" ]
                                , name = "Cctx_hole"
                                , annotation = Nothing
                                }
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
                            [ Elm.string "Invalid replacement" ]
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
                                [ Elm.string "Invalid replacement" ]

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
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper <| op.name ++ "_cctx1"
                                    , annotation = Nothing
                                    }
                                )
                                [ Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = "Cctx_hole"
                                    , annotation = Nothing
                                    }
                                ]
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
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper <| op.name ++ "_cctx1"
                                    , annotation = Nothing
                                    }
                                )
                                [ replacement1, arg2 ]
                        , Branch.int 2 <|
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper <| op.name ++ "_cctx2"
                                    , annotation = Nothing
                                    }
                                )
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
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper <| op.name ++ "_cctx1"
                                    , annotation = Nothing
                                    }
                                )
                                [ replacement1, arg2, arg3 ]
                        , Branch.int 2 <|
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper <| op.name ++ "_cctx2"
                                    , annotation = Nothing
                                    }
                                )
                                [ arg1, replacement2, arg3 ]
                        , Branch.int 3 <|
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper <| op.name ++ "_cctx3"
                                    , annotation = Nothing
                                    }
                                )
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
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper <| op.name ++ "_cctx1"
                                    , annotation = Nothing
                                    }
                                )
                                [ replacement1, arg2, arg3, arg4 ]
                        , Branch.int 2 <|
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper <| op.name ++ "_cctx2"
                                    , annotation = Nothing
                                    }
                                )
                                [ arg1, replacement2, arg3, arg4 ]
                        , Branch.int 3 <|
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper <| op.name ++ "_cctx3"
                                    , annotation = Nothing
                                    }
                                )
                                [ arg1, arg2, replacement3, arg4 ]
                        , Branch.int 4 <|
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper <| op.name ++ "_cctx4"
                                    , annotation = Nothing
                                    }
                                )
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
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper <| op.name ++ "_cctx1"
                                    , annotation = Nothing
                                    }
                                )
                                [ replacement1, arg2, arg3, arg4, arg5 ]
                        , Branch.int 2 <|
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper <| op.name ++ "_cctx2"
                                    , annotation = Nothing
                                    }
                                )
                                [ arg1, replacement2, arg3, arg4, arg5 ]
                        , Branch.int 3 <|
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper <| op.name ++ "_cctx3"
                                    , annotation = Nothing
                                    }
                                )
                                [ arg1, arg2, replacement3, arg4, arg5 ]
                        , Branch.int 4 <|
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper <| op.name ++ "_cctx4"
                                    , annotation = Nothing
                                    }
                                )
                                [ arg1, arg2, arg3, replacement4, arg5 ]
                        , Branch.int 5 <|
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Syntax", "CCtx" ]
                                    , name = firstCharToUpper <| op.name ++ "_cctx5"
                                    , annotation = Nothing
                                    }
                                )
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
                , Elm.tuple (Elm.val <| "boundVars" ++ String.fromInt i)
                    (Elm.value
                        { importFrom = [ "Syntax", "CCtx" ]
                        , name = "Cctx_hole"
                        , annotation = Nothing
                        }
                    )
                )
