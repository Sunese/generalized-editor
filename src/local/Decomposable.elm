module Decomposable exposing (..)

{-| Simulate a type class
-}


type alias Decomposable baseSyntax cctx wellformed =
    { decompose : baseSyntax -> List Int -> Maybe ( cctx, wellformed )
    , getCursorPath : baseSyntax -> List Int
    }
