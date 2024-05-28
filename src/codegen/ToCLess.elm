module ToCLess exposing (..)

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


createToCLessFuns : Syntax -> List Elm.Declaration
createToCLessFuns syntax =
    List.map createToCLessFun syntax.synCatOps
        ++ [ Elm.declaration "toCLess" <|
                Elm.withType (Type.function [ Type.named [] "Base" ] (Type.named [] "CursorLess")) <|
                    Elm.fn
                        ( "base", Nothing )
                        (\base ->
                            Elm.Case.custom base
                                (Type.named [] "Base")
                                (List.map
                                    (\synCatOp ->
                                        Elm.Case.branchWith
                                            synCatOp.synCat
                                            1
                                            (\exps ->
                                                Elm.apply (Elm.val <| firstCharToUpper <| synCatOp.synCat ++ "_CLess")
                                                    [ Elm.apply (Elm.val <| "toCLess_" ++ synCatOp.synCat) exps ]
                                            )
                                    )
                                    syntax.synCatOps
                                )
                        )
           ]


createToCLessFun : SynCatOps -> Elm.Declaration
createToCLessFun synCatOp =
    Elm.declaration ("toCLess_" ++ synCatOp.synCat) <|
        Elm.withType (Type.function [ Type.named [] synCatOp.synCat ] (Type.named [] (synCatOp.synCat ++ "_CLess"))) <|
            Elm.fn
                ( synCatOp.synCat, Nothing )
                (\base ->
                    Elm.Case.custom base
                        (Type.named [] synCatOp.synCat)
                        (getBranchListSynCatOp
                            synCatOp
                        )
                )


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
        Branch.variant1 op.name
            (Branch.var "cursor")
            (\cursor ->
                Elm.apply
                    (Elm.value
                        { importFrom = [ "Debug" ]
                        , name = "todo"
                        , annotation = Nothing
                        }
                    )
                    [ Elm.string "Not wellformed" ]
            )

    else
        case List.length op.arity of
            0 ->
                case op.literal of
                    Nothing ->
                        Branch.variant0 op.name (Elm.val <| firstCharToUpper <| op.name ++ "_CLess")

                    Just _ ->
                        Branch.variant1 op.name (Branch.var "lit") <|
                            \lit -> Elm.apply (Elm.val <| firstCharToUpper <| op.name ++ "_CLess") [ lit ]

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
                            (Elm.val <| firstCharToUpper <| op.name ++ "_CLess")
                            [ Elm.apply (Elm.val <| "toCLess" ++ "_" ++ argSort) [ arg ]
                            ]
                    )

            2 ->
                Branch.variant2
                    op.name
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 1 patternsArray)
                    (\arg1Exp arg2Exp ->
                        let
                            arg1 =
                                Maybe.withDefault ( [], "ERROR" ) (Array.get 0 argsArray)

                            arg2 =
                                Maybe.withDefault ( [], "ERROR" ) (Array.get 1 argsArray)
                        in
                        Elm.apply
                            (Elm.val <| firstCharToUpper <| op.name ++ "_CLess")
                            [ argToCLessTransformation 1 arg1
                            , argToCLessTransformation 2 arg2
                            ]
                    )

            3 ->
                Branch.variant3
                    op.name
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 1 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 2 patternsArray)
                    (\arg1Exp arg2Exp arg3Exp ->
                        let
                            arg1 =
                                Maybe.withDefault ( [], "ERROR" ) (Array.get 0 argsArray)

                            arg2 =
                                Maybe.withDefault ( [], "ERROR" ) (Array.get 1 argsArray)

                            arg3 =
                                Maybe.withDefault ( [], "ERROR" ) (Array.get 2 argsArray)
                        in
                        Elm.apply
                            (Elm.val <| firstCharToUpper <| op.name ++ "_CLess")
                            [ argToCLessTransformation 1 arg1
                            , argToCLessTransformation 2 arg2
                            , argToCLessTransformation 3 arg3
                            ]
                    )

            4 ->
                Branch.variant4
                    op.name
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 1 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 2 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 3 patternsArray)
                    (\arg1Exp arg2Exp arg3Exp arg4Exp ->
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
                        Elm.apply
                            (Elm.val <| firstCharToUpper <| op.name ++ "_CLess")
                            [ argToCLessTransformation 1 arg1
                            , argToCLessTransformation 2 arg2
                            , argToCLessTransformation 3 arg3
                            , argToCLessTransformation 4 arg4
                            ]
                    )

            5 ->
                Branch.variant5
                    op.name
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 0 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 1 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 2 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 3 patternsArray)
                    (Maybe.withDefault (Branch.var "ERROR") <| Array.get 4 patternsArray)
                    (\arg1Exp arg2Exp arg3Exp arg4Exp arg5Exp ->
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
                        Elm.apply
                            (Elm.val <| firstCharToUpper <| op.name ++ "_CLess")
                            [ argToCLessTransformation 1 arg1
                            , argToCLessTransformation 2 arg2
                            , argToCLessTransformation 3 arg3
                            , argToCLessTransformation 4 arg4
                            , argToCLessTransformation 5 arg5
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
