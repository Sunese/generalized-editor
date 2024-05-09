module Convertable exposing (..)

{-| Simulate a type class
-}


type alias Convertable basesort cursorless =
    { toCLess : basesort -> Maybe cursorless
    }
