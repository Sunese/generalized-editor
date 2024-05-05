module Main exposing (..)


type S
    = Let E (Bind E S)
    | Exp E
    | Hole_s
    | Cursor_s S


type E
    = Plus E E
    | Num
    | Var
    | Hole_e
    | Cursor_e E


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
    | Let_CLess_cctx1 Cctx (Bind E_CLess S_CLess)
    | Let_CLess_cctx2 E_CLess (Bind E_CLess Cctx)
    | Exp_CLess_cctx1 Cctx
    | Plus_CLess_cctx1 Cctx E_CLess
    | Plus_CLess_cctx2 E_CLess Cctx


type CctxSyntax
    = S_CLess_CCtx S_CLess
    | E_CLess_CCtx E_CLess
    | Cctx_CCtx Cctx


type Wellformed
    = Root_s_CLess S_CLess
    | Root_e_CLess E_CLess
    | Let_CLess_cursor1 E_CLess (Bind E_CLess S_CLess)
    | Let_CLess_cursor2 E_CLess (Bind E_CLess S_CLess)
    | Exp_CLess_cursor1 E_CLess
    | Plus_CLess_cursor1 E_CLess E_CLess
    | Plus_CLess_cursor2 E_CLess E_CLess


type WellFormedSyntax
    = S_CLess_WellFormed S_CLess
    | E_CLess_WellFormed E_CLess
    | Wellformed_WellFormed Wellformed


type alias Bind a b =
    ( List a, b )


toCLess baseSyntax =
    case baseSyntax of
        S s ->
            case s of
                Let arg1 arg2 ->
                    Let_CLess arg1 arg2 

                Exp arg1 ->
                    Exp_CLess arg1 

                otherwise ->
                    Debugtodo

        E e ->
            case e of
                Plus arg1 arg2 ->
                    Plus_CLess arg1 arg2 

                Num ->
                    Num_CLess 

                Var ->
                    Var_CLess 

                otherwise ->
                    Debugtodo