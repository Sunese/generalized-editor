module Main exposing (..)


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


type alias Bind a b =
    ( List a, b )


type Cctx_CCtx
    = CctxHole_CCtx
    | Let_cctx1_CCtx Cctx_CCtx (Bind E_CCtx S_CCtx)
    | Let_cctx2_CCtx E_CCtx (Bind E_CCtx Cctx_CCtx)
    | Exp_cctx1_CCtx Cctx_CCtx
    | Plus_cctx1_CCtx Cctx_CCtx E_CCtx
    | Plus_cctx2_CCtx E_CCtx Cctx_CCtx


type CctxSyntax
    = S_CCtx S_CCtx
    | E_CCtx E_CCtx
    | Cctx_CCtx Cctx_CCtx
    | Cctx_CCtx Cctx_CCtx
