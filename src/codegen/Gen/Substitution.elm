module Gen.Substitution exposing (annotation_, call_, make_, moduleName_, substitute, substituteAny, values_)

{-| 
@docs moduleName_, substitute, substituteAny, annotation_, make_, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Substitution" ]


{-| {-| Polymorphic function that can be used with any type that has an instance of
the `Substitutable` typeclass.
-}

substitute: Substitution.Substitutable a -> a -> a -> a
-}
substitute :
    Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
substitute substituteArg substituteArg0 substituteArg1 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Substitution" ]
             , name = "substitute"
             , annotation =
                 Just
                     (Type.function
                          [ Type.namedWith
                              [ "Substitution" ]
                              "Substitutable"
                              [ Type.var "a" ]
                          , Type.var "a"
                          , Type.var "a"
                          ]
                          (Type.var "a")
                     )
             }
        )
        [ substituteArg, substituteArg0, substituteArg1 ]


{-| {-| (Generic) Instance of a typeclass, we don't need any specific implementation
for each type/sort, we just want to assure that the expression and replacement
are of the same type. This is constrained by the `substitute` function signature
in the typeclass.
-}

substituteAny: Substitution.Substitutable a
-}
substituteAny : Elm.Expression
substituteAny =
    Elm.value
        { importFrom = [ "Substitution" ]
        , name = "substituteAny"
        , annotation =
            Just
                (Type.namedWith
                     [ "Substitution" ]
                     "Substitutable"
                     [ Type.var "a" ]
                )
        }


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
                     [ "Substitution" ]
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


call_ :
    { substitute :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { substitute =
        \substituteArg substituteArg0 substituteArg1 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Substitution" ]
                     , name = "substitute"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.namedWith
                                      [ "Substitution" ]
                                      "Substitutable"
                                      [ Type.var "a" ]
                                  , Type.var "a"
                                  , Type.var "a"
                                  ]
                                  (Type.var "a")
                             )
                     }
                )
                [ substituteArg, substituteArg0, substituteArg1 ]
    }


values_ : { substitute : Elm.Expression, substituteAny : Elm.Expression }
values_ =
    { substitute =
        Elm.value
            { importFrom = [ "Substitution" ]
            , name = "substitute"
            , annotation =
                Just
                    (Type.function
                         [ Type.namedWith
                             [ "Substitution" ]
                             "Substitutable"
                             [ Type.var "a" ]
                         , Type.var "a"
                         , Type.var "a"
                         ]
                         (Type.var "a")
                    )
            }
    , substituteAny =
        Elm.value
            { importFrom = [ "Substitution" ]
            , name = "substituteAny"
            , annotation =
                Just
                    (Type.namedWith
                         [ "Substitution" ]
                         "Substitutable"
                         [ Type.var "a" ]
                    )
            }
    }