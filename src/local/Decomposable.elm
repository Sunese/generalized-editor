module Decomposable exposing (..)

{-| Simulate a type class
-}


type alias Decomposable baseSyntax cursorLess cctx wellformed =
    { decompose : cursorLess -> List Int -> Maybe ( cctx, wellformed )
    , getCursorPath : baseSyntax -> List Int
    , toCLess : baseSyntax -> cursorLess
    }
