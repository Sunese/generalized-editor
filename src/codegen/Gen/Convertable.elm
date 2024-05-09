module Gen.Convertable exposing (annotation_, make_, moduleName_)

{-| 
@docs moduleName_, annotation_, make_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Convertable" ]


annotation_ :
    { convertable : Type.Annotation -> Type.Annotation -> Type.Annotation }
annotation_ =
    { convertable =
        \convertableArg0 convertableArg1 ->
            Type.alias
                moduleName_
                "Convertable"
                [ convertableArg0, convertableArg1 ]
                (Type.record
                    [ ( "toCLess"
                      , Type.function
                            [ Type.var "basesort" ]
                            (Type.maybe (Type.var "cursorless"))
                      )
                    ]
                )
    }


make_ : { convertable : { toCLess : Elm.Expression } -> Elm.Expression }
make_ =
    { convertable =
        \convertable_args ->
            Elm.withType
                (Type.alias
                    [ "Convertable" ]
                    "Convertable"
                    [ Type.var "basesort", Type.var "cursorless" ]
                    (Type.record
                        [ ( "toCLess"
                          , Type.function
                                [ Type.var "basesort" ]
                                (Type.maybe (Type.var "cursorless"))
                          )
                        ]
                    )
                )
                (Elm.record [ Tuple.pair "toCLess" convertable_args.toCLess ])
    }