module Ops_CLess exposing (..)


type S
    = Let E (Bind E S)
    | Exp E
    | Hole_s


type E
    = Plus E E
    | Num
    | Var
    | Hole_e


type alias Bind a b =
    ( List a, b )