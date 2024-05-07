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
                tODO ->
                    tODO

                Exp arg1 ->
                    S_CLess <| Exp_CLess ( toCLess (E arg1 ) )

                Hole_s ->
                    S_CLess <| Hole_s_CLess 

                Cursor_s arg1 ->
                    Debug.todo "Cursor operator cannot be mapped to cursorless operator"

        E e ->
            case e of
                tODO ->
                    tODO

                Num ->
                    E_CLess <| Num_CLess 

                Var ->
                    E_CLess <| Var_CLess 

                Hole_e ->
                    E_CLess <| Hole_e_CLess 

                Cursor_e arg1 ->
                    Debug.todo "Cursor operator cannot be mapped to cursorless operator"