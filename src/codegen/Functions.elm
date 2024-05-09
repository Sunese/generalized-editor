module Functions exposing (..)

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
   Given a Base syntax, get the path to the cursor in the tree
-}


createGetCursorPath : Syntax -> Elm.Declaration
createGetCursorPath syntax =
    Elm.declaration "getCursorPath" <|
        Elm.withType (Type.function [ Type.list Type.int, Type.named [] "Base" ] (Type.list Type.int)) <|
            Elm.fn2
                ( "path", Nothing )
                ( "base", Nothing )
                (\path base ->
                    Elm.Case.custom base
                        (Type.named [] "Base")
                        (getBranchList
                            syntax
                            path
                        )
                )


getBranchList : Syntax -> Elm.Expression -> List Branch.Branch
getBranchList syntax path =
    -- [ Branch.variant0 "Empty" <| Elm.int 0 ]
    List.map
        (\synCatOp ->
            Branch.variant1 synCatOp.synCat (Branch.var synCatOp.synCat) <|
                \s ->
                    Elm.Case.custom s
                        (Type.named [] synCatOp.synCat)
                        (getBranchListSynCatOp synCatOp)
        )
        syntax.synCatOps


getBranchListSynCatOp : SynCatOps -> List Branch.Branch
getBranchListSynCatOp synCatOp =
    List.map getBranchFromOp synCatOp.ops


getBranchFromOp : Operator -> Branch.Branch
getBranchFromOp op =
    let
        patterns =
            List.indexedMap
                (\i ( boundVars, arg ) ->
                    case getPatternFromArg (i + 1) ( boundVars, arg ) of
                        Just pattern ->
                            pattern

                        Nothing ->
                            Branch.var ("arg" ++ String.fromInt (i + 1))
                )
                op.arity

        patternsArray =
            Array.fromList patterns

        argsArray =
            op.arity |> Array.fromList
    in
    if String.contains "cursor" op.name then
        Branch.variant1
            op.name
            (Branch.var "_")
            (\_ ->
                Elm.val "path"
            )

    else
        case List.length op.arity of
            0 ->
                Branch.variant0 op.name <| Elm.list []

            1 ->
                Branch.variant1
                    op.name
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                    (\arg ->
                        let
                            argSort =
                                Maybe.withDefault ( [], "ERROR" ) (Array.get 0 argsArray) |> Tuple.second
                        in
                        Elm.apply
                            (Elm.val "getCursorPath")
                            [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 1 ])
                            , Elm.apply (Elm.val (firstCharToUpper argSort)) [ arg ]
                            ]
                    )

            2 ->
                Branch.variant2
                    op.name
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 1 patternsArray)
                    (\arg1 arg2 ->
                        let
                            argSort1 =
                                Maybe.withDefault ( [], "ERROR" ) (Array.get 0 argsArray) |> Tuple.second

                            argSort2 =
                                Maybe.withDefault ( [], "ERROR" ) (Array.get 1 argsArray) |> Tuple.second
                        in
                        Elm.Op.append
                            (Elm.apply
                                (Elm.val "getCursorPath")
                                [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 1 ])
                                , Elm.apply (Elm.val (firstCharToUpper argSort1)) [ arg1 ]
                                ]
                            )
                            (Elm.apply
                                (Elm.val "getCursorPath")
                                [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 2 ])
                                , Elm.apply (Elm.val (firstCharToUpper argSort2)) [ Elm.val "arg2" ]
                                ]
                            )
                     -- Elm.list []
                    )

            3 ->
                Branch.variant3 op.name
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 1 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 2 patternsArray)
                    (\arg1 arg2 arg3 ->
                        let
                            argSort1 =
                                Maybe.withDefault ( [], "ERROR" ) (Array.get 0 argsArray) |> Tuple.second

                            argSort2 =
                                Maybe.withDefault ( [], "ERROR" ) (Array.get 1 argsArray) |> Tuple.second

                            argSort3 =
                                Maybe.withDefault ( [], "ERROR" ) (Array.get 2 argsArray) |> Tuple.second
                        in
                        Elm.Op.append
                            (Elm.Op.append
                                (Elm.apply
                                    (Elm.val "getCursorPath")
                                    [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 1 ])
                                    , Elm.apply (Elm.val (firstCharToUpper argSort1)) [ arg1 ]
                                    ]
                                )
                                (Elm.apply
                                    (Elm.val "getCursorPath")
                                    [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 2 ])
                                    , Elm.apply (Elm.val (firstCharToUpper argSort2)) [ arg2 ]
                                    ]
                                )
                            )
                            (Elm.apply
                                (Elm.val "getCursorPath")
                                [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 3 ])
                                , Elm.apply (Elm.val (firstCharToUpper argSort3)) [ arg3 ]
                                ]
                            )
                    )

            4 ->
                Branch.variant4 op.name
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 1 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 2 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 3 patternsArray)
                    (\arg1 arg2 arg3 arg4 ->
                        let
                            argSort1 =
                                Maybe.withDefault ( [], "ERROR" ) (Array.get 0 argsArray) |> Tuple.second

                            argSort2 =
                                Maybe.withDefault ( [], "ERROR" ) (Array.get 1 argsArray) |> Tuple.second

                            argSort3 =
                                Maybe.withDefault ( [], "ERROR" ) (Array.get 2 argsArray) |> Tuple.second

                            argSort4 =
                                Maybe.withDefault ( [], "ERROR" ) (Array.get 3 argsArray) |> Tuple.second
                        in
                        Elm.Op.append
                            (Elm.Op.append
                                (Elm.Op.append
                                    (Elm.apply
                                        (Elm.val "getCursorPath")
                                        [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 1 ])
                                        , Elm.apply (Elm.val (firstCharToUpper argSort1)) [ arg1 ]
                                        ]
                                    )
                                    (Elm.apply
                                        (Elm.val "getCursorPath")
                                        [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 2 ])
                                        , Elm.apply (Elm.val (firstCharToUpper argSort2)) [ arg2 ]
                                        ]
                                    )
                                )
                                (Elm.apply
                                    (Elm.val "getCursorPath")
                                    [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 3 ])
                                    , Elm.apply (Elm.val (firstCharToUpper argSort3)) [ arg3 ]
                                    ]
                                )
                            )
                            (Elm.apply
                                (Elm.val "getCursorPath")
                                [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 4 ])
                                , Elm.apply (Elm.val (firstCharToUpper argSort4)) [ arg4 ]
                                ]
                            )
                    )

            5 ->
                Branch.variant5 op.name
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 1 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 2 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 3 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 4 patternsArray)
                    (\arg1 arg2 arg3 arg4 arg5 ->
                        let
                            argSort1 =
                                Maybe.withDefault ( [], "ERROR" ) (Array.get 0 argsArray) |> Tuple.second

                            argSort2 =
                                Maybe.withDefault ( [], "ERROR" ) (Array.get 1 argsArray) |> Tuple.second

                            argSort3 =
                                Maybe.withDefault ( [], "ERROR" ) (Array.get 2 argsArray) |> Tuple.second

                            argSort4 =
                                Maybe.withDefault ( [], "ERROR" ) (Array.get 3 argsArray) |> Tuple.second

                            argSort5 =
                                Maybe.withDefault ( [], "ERROR" ) (Array.get 4 argsArray) |> Tuple.second
                        in
                        Elm.Op.append
                            (Elm.Op.append
                                (Elm.Op.append
                                    (Elm.Op.append
                                        (Elm.apply
                                            (Elm.val "getCursorPath")
                                            [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 1 ])
                                            , Elm.apply (Elm.val (firstCharToUpper argSort1)) [ arg1 ]
                                            ]
                                        )
                                        (Elm.apply
                                            (Elm.val "getCursorPath")
                                            [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 2 ])
                                            , Elm.apply (Elm.val (firstCharToUpper argSort2)) [ arg2 ]
                                            ]
                                        )
                                    )
                                    (Elm.apply
                                        (Elm.val "getCursorPath")
                                        [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 3 ])
                                        , Elm.apply (Elm.val (firstCharToUpper argSort3)) [ arg3 ]
                                        ]
                                    )
                                )
                                (Elm.apply
                                    (Elm.val "getCursorPath")
                                    [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 4 ])
                                    , Elm.apply (Elm.val (firstCharToUpper argSort4)) [ arg4 ]
                                    ]
                                )
                            )
                            (Elm.apply
                                (Elm.val "getCursorPath")
                                [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 5 ])
                                , Elm.apply (Elm.val (firstCharToUpper argSort5)) [ arg5 ]
                                ]
                            )
                    )

            _ ->
                Debug.todo "More than 5 arguments not supported"



-- case op.arity of
--     [] ->
--         Branch.variant0 op.name <| Elm.list []
--     [ ( boundVars, arg ) ] ->
--         case getPatternFromArg (List.head op.arity) of
--             Just pattern ->
--                 Branch.variant1
--                     op.name
--                     pattern
--                     (\exp -> Elm.int 420)
--             Nothing ->
--                 Branch.variant1
--                     op.name
--                     (Branch.var "arg")
--                     (\exp -> Elm.int 420)
--     [ ( boundVars1, arg1 ), ( boundVars2, arg2 ) ] ->
--         Branch.variant0 op.name <| Elm.list []
--     _ ->
--         Branch.variant0 op.name <| Elm.list []
-- getBranchFromOp : Operator -> Branch.Branch
-- getBranchFromOp op =
--     branchWith op.name
--         (List.length op.arity)
--         (\exps ->
--             Elm.Let.letIn
--                 (\_ ->
--                     Elm.int 420
--                 )
--                 |> Elm.Let.tuple "boundVars" "arg" (Elm.val "arg")
--                 |> Elm.Let.toExpression
--         )


isBinder : ( List String, String ) -> Bool
isBinder =
    Tuple.first >> List.isEmpty >> not



-- case op.arity of
--     [] ->
--         Branch.variant0 op.name <| Elm.list []
--     [(boundVars1, arg1)] ->
--         case getPatternFromArg (List.head op.arity) of
--             Just pattern ->
--                 Branch.variant1
--                     op.name
--                     pattern
--                     (\exp -> Elm.int 420)
--             Nothing ->
--                 Branch.variant1
--                     op.name
--                     (Branch.var "arg")
--                     (\exp -> Elm.int 420)
--     [(boundVars1, arg1), (boundVars2, arg2)] ->
--         Branch.variant0 op.name <| Elm.list []
--      [(boundVars1, arg1), (boundVars2, arg2), (boundVars3, arg3)] ->
--     _ ->
--         Branch.variant0 op.name <| Elm.list []


getPatternFromArg : Int -> ( List String, String ) -> Maybe (Branch.Pattern Elm.Expression)
getPatternFromArg i arg =
    case arg of
        ( [], arg_ ) ->
            Nothing

        ( boundVars, arg_ ) ->
            Just <| Branch.var <| "(boundVars" ++ String.fromInt i ++ ", arg" ++ String.fromInt i ++ ")"



-- Just <| Branch.tuple (Branch.var ("boundVars" ++ String.fromInt i)) (Branch.var ("arg" ++ String.fromInt i))
-- getBinderTuple : Arity -> Maybe (Branch.Pattern Elm.Expression)
-- getBinderTuple arity =
--     if List.
-- Branch.variant2 op.name (Branch.var op.name) <| \_ -> Elm.int 420
