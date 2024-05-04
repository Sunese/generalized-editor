module Main exposing (..)


type S
    = Let E (Bind E S)
    | Exp E


type E
    = Plus E E
    | Num
    | Var


type BaseSyntax
    = S S
    | E E


type S_CLess
    = Let_CLess E_CLess (Bind E_CLess S_CLess)
    | Exp_CLess E_CLess
    | Hole_s_CLess


type E_CLess
    = Plus_CLess E_CLess E_CLess
    | Num_CLess
    | Var_CLess
    | Hole_e_CLess


type CursorlessSyntax
    = S_CLess S_CLess
    | E_CLess E_CLess


type Cctx
    = Hole
    | Let_CLess1 Cctx (Bind E_CLess S_CLess)
    | Let_CLess2 E_CLess (Bind E_CLess Cctx)
    | Exp_CLess1 Cctx
    | Plus_CLess1 Cctx E_CLess
    | Plus_CLess2 E_CLess Cctx


type CctxSyntax
    = S_CLess_CCtx S_CLess
    | E_CLess_CCtx E_CLess
    | Cctx_CCtx Cctx


type alias Bind a b =
    ( List a, b )
