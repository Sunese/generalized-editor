module Ops_Wellformed exposing (..)


type S
    = Let E (Bind E S)
    | Exp E
    | Hole_s


type E
    = Plus E E
    | Num
    | Var
    | Hole_e


type Wellformed
    = RootCursor_s S
    | RootCursor_e E
    | Let_cursor_arg_0 E (Bind E S)
    | Let_cursor_arg_1 E (Bind E S)
    | Exp_cursor_arg_0 E
    | Plus_cursor_arg_0 E E
    | Plus_cursor_arg_1 E E


type alias Bind a b =
    ( List a, b )