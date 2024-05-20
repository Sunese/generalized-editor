module GetCursorPath exposing (..)

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
        Elm.withType
            (Type.function
                [ Type.list Type.int
                , Type.named [] "Base"
                ]
                (Type.list Type.int)
            )
            (Elm.fn2
                ( "path", Nothing )
                ( "base", Nothing )
                (\_ base ->
                    Elm.Case.custom base
                        (Type.named [] "Base")
                        (getBranchList syntax)
                )
            )


getBranchList : Syntax -> List Branch.Branch
getBranchList syntax =
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
                                , Elm.apply (Elm.val (firstCharToUpper argSort1)) [ Elm.val "arg1" ]
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
                                    , Elm.apply (Elm.val (firstCharToUpper argSort1)) [ Elm.val "arg1" ]
                                    ]
                                )
                                (Elm.apply
                                    (Elm.val "getCursorPath")
                                    [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 2 ])
                                    , Elm.apply (Elm.val (firstCharToUpper argSort2)) [ Elm.val "arg2" ]
                                    ]
                                )
                            )
                            (Elm.apply
                                (Elm.val "getCursorPath")
                                [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 3 ])
                                , Elm.apply (Elm.val (firstCharToUpper argSort3)) [ Elm.val "arg3" ]
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
                                        , Elm.apply (Elm.val (firstCharToUpper argSort1)) [ Elm.val "arg1" ]
                                        ]
                                    )
                                    (Elm.apply
                                        (Elm.val "getCursorPath")
                                        [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 2 ])
                                        , Elm.apply (Elm.val (firstCharToUpper argSort2)) [ Elm.val "arg2" ]
                                        ]
                                    )
                                )
                                (Elm.apply
                                    (Elm.val "getCursorPath")
                                    [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 3 ])
                                    , Elm.apply (Elm.val (firstCharToUpper argSort3)) [ Elm.val "arg3" ]
                                    ]
                                )
                            )
                            (Elm.apply
                                (Elm.val "getCursorPath")
                                [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 4 ])
                                , Elm.apply (Elm.val (firstCharToUpper argSort4)) [ Elm.val "arg4" ]
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
                                            , Elm.apply (Elm.val (firstCharToUpper argSort1)) [ Elm.val "arg1" ]
                                            ]
                                        )
                                        (Elm.apply
                                            (Elm.val "getCursorPath")
                                            [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 2 ])
                                            , Elm.apply (Elm.val (firstCharToUpper argSort2)) [ Elm.val "arg2" ]
                                            ]
                                        )
                                    )
                                    (Elm.apply
                                        (Elm.val "getCursorPath")
                                        [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 3 ])
                                        , Elm.apply (Elm.val (firstCharToUpper argSort3)) [ Elm.val "arg3" ]
                                        ]
                                    )
                                )
                                (Elm.apply
                                    (Elm.val "getCursorPath")
                                    [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 4 ])
                                    , Elm.apply (Elm.val (firstCharToUpper argSort4)) [ Elm.val "arg4" ]
                                    ]
                                )
                            )
                            (Elm.apply
                                (Elm.val "getCursorPath")
                                [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 5 ])
                                , Elm.apply (Elm.val (firstCharToUpper argSort5)) [ Elm.val "arg5" ]
                                ]
                            )
                    )

            _ ->
                Debug.todo "More than 5 arguments not supported"


getPatternFromArg : Int -> ( List String, String ) -> Maybe (Branch.Pattern Elm.Expression)
getPatternFromArg i arg =
    case arg of
        ( [], arg_ ) ->
            Nothing

        ( boundVars, arg_ ) ->
            Just <| Branch.var <| "(boundVars" ++ String.fromInt i ++ ", arg" ++ String.fromInt i ++ ")"
