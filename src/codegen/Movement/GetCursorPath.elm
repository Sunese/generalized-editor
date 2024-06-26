module Movement.GetCursorPath exposing (..)

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
import Syntax.RawSyntaxP exposing (..)
import Syntax.Syntax exposing (..)



{-
   Given a Base syntax, get the path to the cursor in the tree
-}


createGetCursorPath : Syntax -> Elm.Declaration
createGetCursorPath syntax =
    Elm.declaration "getCursorPath" <|
        Elm.withType
            (Type.function
                [ Type.list Type.int
                , Type.named [ "Syntax", "Base" ] "Base"
                ]
                (Type.list Type.int)
            )
            (Elm.fn2
                ( "path", Nothing )
                ( "base", Nothing )
                (\_ base ->
                    Elm.Case.custom base
                        (Type.named [ "Syntax", "Base" ] "Base")
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
                        (Type.named [ "Syntax", "Base" ] synCatOp.synCat)
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
                case op.literal of
                    Just _ ->
                        Branch.variant1 op.name (Branch.var "lit") <|
                            \_ -> Elm.list []

                    Nothing ->
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
                            , Elm.apply
                                (Elm.value
                                    { importFrom = [ "Syntax", "Base" ]
                                    , name = firstCharToUpper argSort
                                    , annotation = Nothing
                                    }
                                )
                                [ arg ]
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
                                , Elm.apply
                                    (Elm.value
                                        { importFrom = [ "Syntax", "Base" ]
                                        , name = firstCharToUpper argSort1
                                        , annotation = Nothing
                                        }
                                    )
                                    [ Elm.val "arg1" ]
                                ]
                            )
                            (Elm.apply
                                (Elm.val "getCursorPath")
                                [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 2 ])
                                , Elm.apply
                                    (Elm.value
                                        { importFrom = [ "Syntax", "Base" ]
                                        , name = firstCharToUpper argSort2
                                        , annotation = Nothing
                                        }
                                    )
                                    [ Elm.val "arg2" ]
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
                                    , Elm.apply
                                        (Elm.value
                                            { importFrom = [ "Syntax", "Base" ]
                                            , name = firstCharToUpper argSort1
                                            , annotation = Nothing
                                            }
                                        )
                                        [ Elm.val "arg1" ]
                                    ]
                                )
                                (Elm.apply
                                    (Elm.val "getCursorPath")
                                    [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 2 ])
                                    , Elm.apply
                                        (Elm.value
                                            { importFrom = [ "Syntax", "Base" ]
                                            , name = firstCharToUpper argSort2
                                            , annotation = Nothing
                                            }
                                        )
                                        [ Elm.val "arg2" ]
                                    ]
                                )
                            )
                            (Elm.apply
                                (Elm.val "getCursorPath")
                                [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 3 ])
                                , Elm.apply
                                    (Elm.value
                                        { importFrom = [ "Syntax", "Base" ]
                                        , name = firstCharToUpper argSort3
                                        , annotation = Nothing
                                        }
                                    )
                                    [ Elm.val "arg3" ]
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
                                        , Elm.apply
                                            (Elm.value
                                                { importFrom = [ "Syntax", "Base" ]
                                                , name = firstCharToUpper argSort1
                                                , annotation = Nothing
                                                }
                                            )
                                            [ Elm.val "arg1" ]
                                        ]
                                    )
                                    (Elm.apply
                                        (Elm.val "getCursorPath")
                                        [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 2 ])
                                        , Elm.apply
                                            (Elm.value
                                                { importFrom = [ "Syntax", "Base" ]
                                                , name = firstCharToUpper argSort2
                                                , annotation = Nothing
                                                }
                                            )
                                            [ Elm.val "arg2" ]
                                        ]
                                    )
                                )
                                (Elm.apply
                                    (Elm.val "getCursorPath")
                                    [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 3 ])
                                    , Elm.apply
                                        (Elm.value
                                            { importFrom = [ "Syntax", "Base" ]
                                            , name = firstCharToUpper argSort3
                                            , annotation = Nothing
                                            }
                                        )
                                        [ Elm.val "arg3" ]
                                    ]
                                )
                            )
                            (Elm.apply
                                (Elm.val "getCursorPath")
                                [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 4 ])
                                , Elm.apply
                                    (Elm.value
                                        { importFrom = [ "Syntax", "Base" ]
                                        , name = firstCharToUpper argSort4
                                        , annotation = Nothing
                                        }
                                    )
                                    [ Elm.val "arg4" ]
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
                                            , Elm.apply
                                                (Elm.value
                                                    { importFrom = [ "Syntax", "Base" ]
                                                    , name = firstCharToUpper argSort1
                                                    , annotation = Nothing
                                                    }
                                                )
                                                [ Elm.val "arg1" ]
                                            ]
                                        )
                                        (Elm.apply
                                            (Elm.val "getCursorPath")
                                            [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 2 ])
                                            , Elm.apply
                                                (Elm.value
                                                    { importFrom = [ "Syntax", "Base" ]
                                                    , name = firstCharToUpper argSort2
                                                    , annotation = Nothing
                                                    }
                                                )
                                                [ Elm.val "arg2" ]
                                            ]
                                        )
                                    )
                                    (Elm.apply
                                        (Elm.val "getCursorPath")
                                        [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 3 ])
                                        , Elm.apply
                                            (Elm.value
                                                { importFrom = [ "Syntax", "Base" ]
                                                , name = firstCharToUpper argSort3
                                                , annotation = Nothing
                                                }
                                            )
                                            [ Elm.val "arg3" ]
                                        ]
                                    )
                                )
                                (Elm.apply
                                    (Elm.val "getCursorPath")
                                    [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 4 ])
                                    , Elm.apply
                                        (Elm.value
                                            { importFrom = [ "Syntax", "Base" ]
                                            , name = firstCharToUpper argSort4
                                            , annotation = Nothing
                                            }
                                        )
                                        [ Elm.val "arg4" ]
                                    ]
                                )
                            )
                            (Elm.apply
                                (Elm.val "getCursorPath")
                                [ Elm.Op.append (Elm.val "path") (Elm.list [ Elm.int 5 ])
                                , Elm.apply
                                    (Elm.value
                                        { importFrom = [ "Syntax", "Base" ]
                                        , name = firstCharToUpper argSort5
                                        , annotation = Nothing
                                        }
                                    )
                                    [ Elm.val "arg5" ]
                                ]
                            )
                    )

            _ ->
                Debug.todo <| "More than 5 arguments not supported: " ++ op.name


getPatternFromArg : Int -> ( List String, String ) -> Maybe (Branch.Pattern Elm.Expression)
getPatternFromArg i arg =
    case arg of
        ( [], arg_ ) ->
            Nothing

        ( boundVars, arg_ ) ->
            Just <| Branch.var <| "(boundVars" ++ String.fromInt i ++ ", arg" ++ String.fromInt i ++ ")"
