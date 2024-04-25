module Substitution exposing (..)

{-| Simulate a type class
-}


type alias Substitutable a =
    { substitute : a -> a -> a }


{-| (Generic) Instance of a typeclass, we don't need any specific implementation
for each type/sort, we just want to assure that the expression and replacement
are of the same type. This is constrained by the `substitute` function signature
in the typeclass.
-}
substituteAny : Substitutable a
substituteAny =
    { substitute = \_ replacement -> replacement }


{-| Polymorphic function that can be used with any type that has an instance of
the `Substitutable` typeclass.
-}
substitute : Substitutable a -> a -> a -> a
substitute substitutable expression replacement =
    substitutable.substitute expression replacement
