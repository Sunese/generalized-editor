module Gen.Decomposable exposing (moduleName_, annotation_, make_)

{-|

@docs moduleName_, annotation_, make_

-}

import Elm
import Elm.Annotation as Type


{-| The name of this module.
-}
moduleName_ : List String
moduleName_ =
    [ "Decomposable" ]


annotation_ :
    { decomposable :
        Type.Annotation
        -> Type.Annotation
        -> Type.Annotation
        -> Type.Annotation
        -> Type.Annotation
    }
annotation_ =
    { decomposable =
        \decomposableArg0 decomposableArg1 decomposableArg2 decomposableArg3 ->
            Type.alias
                moduleName_
                "Decomposable"
                [ decomposableArg0
                , decomposableArg1
                , decomposableArg2
                , decomposableArg3
                ]
                (Type.record
                    [ ( "decompose"
                      , Type.function
                            [ Type.var "cursorLess", Type.list Type.int ]
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
                    , ( "toCLess"
                      , Type.function
                            [ Type.var "baseSyntax" ]
                            (Type.var "cursorLess")
                      )
                    ]
                )
    }


make_ :
    { decomposable :
        { decompose : Elm.Expression
        , getCursorPath : Elm.Expression
        , toCLess : Elm.Expression
        }
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
                    , Type.var "cursorLess"
                    , Type.var "cctx"
                    , Type.var "wellformed"
                    ]
                    (Type.record
                        [ ( "decompose"
                          , Type.function
                                [ Type.var "cursorLess", Type.list Type.int ]
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
                        , ( "toCLess"
                          , Type.function
                                [ Type.var "baseSyntax" ]
                                (Type.var "cursorLess")
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "decompose" decomposable_args.decompose
                    , Tuple.pair "getCursorPath" decomposable_args.getCursorPath
                    , Tuple.pair "toCLess" decomposable_args.toCLess
                    ]
                )
    }
