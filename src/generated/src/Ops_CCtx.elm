module Ops_CCtx exposing (..)


type S
    = Let E (Bind E S)
    | Exp E
    | Hole_s


type E
    = Plus E E
    | Num
    | Var
    | Hole_e


type Cctx
    = Let_cctx0 Cctx (Bind E S)
    | Let_cctx1 E (Bind E Cctx)
    | Exp_cctx0 Cctx
    | Plus_cctx0 Cctx E
    | Plus_cctx1 E Cctx
    | CctxHole


type alias Bind a b =
    ( List a, b )