module Gen.Decomposable exposing (annotation_, make_, moduleName_)

{-| 
@docs moduleName_, annotation_, make_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Decomposable" ]


annotation_ :
    { decomposable :
        Type.Annotation -> Type.Annotation -> Type.Annotation -> Type.Annotation
    }
annotation_ =
    { decomposable =
        \decomposableArg0 decomposableArg1 decomposableArg2 ->
            Type.alias
                moduleName_
                "Decomposable"
                [ decomposableArg0, decomposableArg1, decomposableArg2 ]
                (Type.record
                    [ ( "decompose"
                      , Type.function
                            [ Type.var "baseSyntax", Type.list Type.int ]
                            (Type.maybe
                                (Type.tuple
                                    (Type.var "cctx")
                                    (Type.var "wellformed")
                                )
                            )
                      )
                    , ( "getCursorPath"
                      , Type.function
                            [ Type.var "baseSyntax" ]
                            (Type.list Type.int)
                      )
                    ]
                )
    }


make_ :
    { decomposable :
        { decompose : Elm.Expression, getCursorPath : Elm.Expression }
        -> Elm.Expression
    }
make_ =
    { decomposable =
        \decomposable_args ->
            Elm.withType
                (Type.alias
                    [ "Decomposable" ]
                    "Decomposable"
                    [ Type.var "baseSyntax"
                    , Type.var "cctx"
                    , Type.var "wellformed"
                    ]
                    (Type.record
                        [ ( "decompose"
                          , Type.function
                                [ Type.var "baseSyntax", Type.list Type.int ]
                                (Type.maybe
                                    (Type.tuple
                                        (Type.var "cctx")
                                        (Type.var "wellformed")
                                    )
                                )
                          )
                        , ( "getCursorPath"
                          , Type.function
                                [ Type.var "baseSyntax" ]
                                (Type.list Type.int)
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "decompose" decomposable_args.decompose
                    , Tuple.pair "getCursorPath" decomposable_args.getCursorPath
                    ]
                )
    }