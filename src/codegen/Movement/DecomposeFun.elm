module Movement.DecomposeFun exposing (..)

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


createDecomposeFuns : Syntax -> List Elm.Declaration
createDecomposeFuns syntax =
    [ createToWellFormedFun syntax
    , createConsumeCursorFun syntax
    , Elm.declaration "decompose" <|
        Elm.withType
            (Type.function
                [ Type.named [ "Syntax", "Base" ] "Base" ]
                (Type.tuple
                    (Type.named [ "Syntax", "CCtx" ] "Cctx")
                    (Type.named [ "Syntax", "Wellformed" ] "Wellformed")
                )
            )
        <|
            Elm.fn
                ( "base", Nothing )
                (\base ->
                    Elm.Let.letIn
                        (\( cctx, rest ) ->
                            Elm.tuple
                                cctx
                                (Elm.apply
                                    (Elm.val "toWellformed")
                                    [ rest ]
                                )
                        )
                        |> Elm.Let.tuple
                            "cctx"
                            "rest"
                            (Elm.apply
                                (Elm.val "toCCtx")
                                [ Elm.val "base"
                                , Elm.apply
                                    (Elm.val "getCursorPath")
                                    [ Elm.list []
                                    , Elm.val "base"
                                    ]
                                ]
                            )
                        |> Elm.Let.toExpression
                )
    ]


createToWellFormedFun : Syntax -> Elm.Declaration
createToWellFormedFun syntax =
    Elm.declaration "toWellformed" <|
        Elm.withType (Type.function [ Type.named [ "Syntax", "Base" ] "Base" ] (Type.named [ "Syntax", "Wellformed" ] "Wellformed")) <|
            Elm.fn
                ( "base", Nothing )
                (\base ->
                    Elm.Case.custom
                        (Elm.apply (Elm.val "consumeCursor") [ base ])
                        (Type.named [ "Syntax", "Base" ] "Base")
                        (List.map
                            (\synCatOp ->
                                branchWith synCatOp.synCat
                                    1
                                    (\exps ->
                                        Elm.apply
                                            (Elm.value
                                                { importFrom = [ "Syntax", "Wellformed" ]
                                                , name = "Root_" ++ synCatOp.synCat ++ "_CLess"
                                                , annotation = Nothing
                                                }
                                            )
                                            [ Elm.apply (Elm.val <| "toCLess_" ++ synCatOp.synCat) exps ]
                                    )
                            )
                            syntax.synCatOps
                        )
                )


createConsumeCursorFun : Syntax -> Elm.Declaration
createConsumeCursorFun syntax =
    Elm.declaration "consumeCursor" <|
        Elm.withType (Type.function [ Type.named [ "Syntax", "Base" ] "Base" ] (Type.named [ "Syntax", "Base" ] "Base")) <|
            Elm.fn
                ( "base", Nothing )
                (\base ->
                    Elm.Case.custom base
                        (Type.named [ "Syntax", "Base" ] "Base")
                        (List.map
                            (\synCatOp ->
                                branchWith synCatOp.synCat
                                    1
                                    (\exps ->
                                        Elm.Case.custom
                                            (Elm.val "arg1")
                                            (Type.named [] (firstCharToUpper synCatOp.synCat))
                                            [ Branch.variant1
                                                ("Syntax.Base.Cursor_" ++ synCatOp.synCat)
                                                (Branch.var "underCursor")
                                                (\exp ->
                                                    Elm.apply
                                                        (Elm.value
                                                            { importFrom = [ "Syntax", "Base" ]
                                                            , name = firstCharToUpper synCatOp.synCat
                                                            , annotation = Nothing
                                                            }
                                                        )
                                                        [ exp ]
                                                )
                                            , Branch.ignore
                                                (Elm.apply
                                                    (Elm.value
                                                        { importFrom = [ "Syntax", "Base" ]
                                                        , name = firstCharToUpper synCatOp.synCat
                                                        , annotation = Nothing
                                                        }
                                                    )
                                                    exps
                                                )
                                            ]
                                    )
                            )
                            syntax.synCatOps
                        )
                )
