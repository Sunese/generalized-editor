module Gen.Substitutable exposing (annotation_, make_, moduleName_)

{-| 
@docs moduleName_, annotation_, make_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Substitutable" ]


annotation_ : { substitutable : Type.Annotation -> Type.Annotation }
annotation_ =
    { substitutable =
        \substitutableArg0 ->
            Type.alias
                moduleName_
                "Substitutable"
                [ substitutableArg0 ]
                (Type.record
                    [ ( "substitute"
                      , Type.function
                            [ Type.var "a", Type.var "a" ]
                            (Type.var "a")
                      )
                    ]
                )
    }


make_ : { substitutable : { substitute : Elm.Expression } -> Elm.Expression }
make_ =
    { substitutable =
        \substitutable_args ->
            Elm.withType
                (Type.alias
                    [ "Substitutable" ]
                    "Substitutable"
                    [ Type.var "a" ]
                    (Type.record
                        [ ( "substitute"
                          , Type.function
                                [ Type.var "a", Type.var "a" ]
                                (Type.var "a")
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "substitute" substitutable_args.substitute ]
                )
    }