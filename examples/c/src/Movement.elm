module Movement exposing (..)

import Syntax.Base
import Syntax.CCtx
import Syntax.Cursorless
import Syntax.Wellformed


toCLess_p : Syntax.Base.P -> Syntax.Cursorless.P_CLess
toCLess_p p =
    case p of
        Syntax.Base.Program arg1 ->
            Syntax.Cursorless.Program_CLess (toCLess_fd arg1)

        Syntax.Base.Hole_p ->
            Syntax.Cursorless.Hole_p_CLess

        Syntax.Base.Cursor_p cursor ->
            Debug.todo "Not wellformed"


toCLess_b : Syntax.Base.B -> Syntax.Cursorless.B_CLess
toCLess_b b =
    case b of
        Syntax.Base.Block arg1 ->
            Syntax.Cursorless.Block_CLess (toCLess_bi arg1)

        Syntax.Base.Hole_b ->
            Syntax.Cursorless.Hole_b_CLess

        Syntax.Base.Cursor_b cursor ->
            Debug.todo "Not wellformed"


toCLess_bi : Syntax.Base.Bi -> Syntax.Cursorless.Bi_CLess
toCLess_bi bi =
    case bi of
        Syntax.Base.Blockdecls arg1 ->
            Syntax.Cursorless.Blockdecls_CLess (toCLess_vd arg1)

        Syntax.Base.Blockstmts arg1 ->
            Syntax.Cursorless.Blockstmts_CLess (toCLess_s arg1)

        Syntax.Base.Blockdone ->
            Syntax.Cursorless.Blockdone_CLess

        Syntax.Base.Hole_bi ->
            Syntax.Cursorless.Hole_bi_CLess

        Syntax.Base.Cursor_bi cursor ->
            Debug.todo "Not wellformed"


toCLess_vd : Syntax.Base.Vd -> Syntax.Cursorless.Vd_CLess
toCLess_vd vd =
    case vd of
        Syntax.Base.Vardecl arg1 arg2 (boundVars3, arg3) ->
            Syntax.Cursorless.Vardecl_CLess
                (toCLess_t arg1)
                (toCLess_e arg2)
                ( List.map toCLess_id boundVars3, toCLess_bi arg3 )

        Syntax.Base.Hole_vd ->
            Syntax.Cursorless.Hole_vd_CLess

        Syntax.Base.Cursor_vd cursor ->
            Debug.todo "Not wellformed"


toCLess_fd : Syntax.Base.Fd -> Syntax.Cursorless.Fd_CLess
toCLess_fd fd =
    case fd of
        Syntax.Base.Fundecl1 arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
            Syntax.Cursorless.Fundecl1_CLess
                (toCLess_t arg1)
                ( List.map toCLess_id boundVars2, toCLess_fd arg2 )
                (toCLess_t arg3)
                ( List.map toCLess_id boundVars4, toCLess_b arg4 )

        Syntax.Base.Fundecl2 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
            Syntax.Cursorless.Fundecl2_CLess
                (toCLess_t arg1)
                ( List.map toCLess_id boundVars2, toCLess_fd arg2 )
                (toCLess_t arg3)
                (toCLess_t arg4)
                ( List.map toCLess_id boundVars5, toCLess_b arg5 )

        Syntax.Base.Fundecldone ->
            Syntax.Cursorless.Fundecldone_CLess

        Syntax.Base.Hole_fd ->
            Syntax.Cursorless.Hole_fd_CLess

        Syntax.Base.Cursor_fd cursor ->
            Debug.todo "Not wellformed"


toCLess_s : Syntax.Base.S -> Syntax.Cursorless.S_CLess
toCLess_s s =
    case s of
        Syntax.Base.Assignment arg1 arg2 ->
            Syntax.Cursorless.Assignment_CLess
                (toCLess_id arg1)
                (toCLess_e arg2)

        Syntax.Base.Stmtfuncall arg1 arg2 ->
            Syntax.Cursorless.Stmtfuncall_CLess
                (toCLess_id arg1)
                (toCLess_fa arg2)

        Syntax.Base.Return arg1 ->
            Syntax.Cursorless.Return_CLess (toCLess_e arg1)

        Syntax.Base.Conditional arg1 ->
            Syntax.Cursorless.Conditional_CLess (toCLess_cond arg1)

        Syntax.Base.Compstmt arg1 arg2 ->
            Syntax.Cursorless.Compstmt_CLess (toCLess_s arg1) (toCLess_s arg2)

        Syntax.Base.Hole_s ->
            Syntax.Cursorless.Hole_s_CLess

        Syntax.Base.Cursor_s cursor ->
            Debug.todo "Not wellformed"


toCLess_fa : Syntax.Base.Fa -> Syntax.Cursorless.Fa_CLess
toCLess_fa fa =
    case fa of
        Syntax.Base.Funarg arg1 arg2 ->
            Syntax.Cursorless.Funarg_CLess (toCLess_t arg1) (toCLess_id arg2)

        Syntax.Base.Funargs arg1 arg2 arg3 ->
            Syntax.Cursorless.Funargs_CLess
                (toCLess_t arg1)
                (toCLess_id arg2)
                (toCLess_fa arg3)

        Syntax.Base.Hole_fa ->
            Syntax.Cursorless.Hole_fa_CLess

        Syntax.Base.Cursor_fa cursor ->
            Debug.todo "Not wellformed"


toCLess_cond : Syntax.Base.Cond -> Syntax.Cursorless.Cond_CLess
toCLess_cond cond =
    case cond of
        Syntax.Base.Ifelse arg1 arg2 arg3 ->
            Syntax.Cursorless.Ifelse_CLess
                (toCLess_e arg1)
                (toCLess_b arg2)
                (toCLess_b arg3)

        Syntax.Base.Hole_cond ->
            Syntax.Cursorless.Hole_cond_CLess

        Syntax.Base.Cursor_cond cursor ->
            Debug.todo "Not wellformed"


toCLess_t : Syntax.Base.T -> Syntax.Cursorless.T_CLess
toCLess_t t =
    case t of
        Syntax.Base.Tint ->
            Syntax.Cursorless.Tint_CLess

        Syntax.Base.Tchar ->
            Syntax.Cursorless.Tchar_CLess

        Syntax.Base.Tbool ->
            Syntax.Cursorless.Tbool_CLess

        Syntax.Base.Hole_t ->
            Syntax.Cursorless.Hole_t_CLess

        Syntax.Base.Cursor_t cursor ->
            Debug.todo "Not wellformed"


toCLess_e : Syntax.Base.E -> Syntax.Cursorless.E_CLess
toCLess_e e =
    case e of
        Syntax.Base.Int lit ->
            Syntax.Cursorless.Int_CLess lit

        Syntax.Base.Char lit ->
            Syntax.Cursorless.Char_CLess lit

        Syntax.Base.Bool lit ->
            Syntax.Cursorless.Bool_CLess lit

        Syntax.Base.Plus arg1 arg2 ->
            Syntax.Cursorless.Plus_CLess (toCLess_e arg1) (toCLess_e arg2)

        Syntax.Base.Equals arg1 arg2 ->
            Syntax.Cursorless.Equals_CLess (toCLess_e arg1) (toCLess_e arg2)

        Syntax.Base.Expfuncall arg1 arg2 ->
            Syntax.Cursorless.Expfuncall_CLess
                (toCLess_id arg1)
                (toCLess_fa arg2)

        Syntax.Base.Expident arg1 ->
            Syntax.Cursorless.Expident_CLess (toCLess_id arg1)

        Syntax.Base.Hole_e ->
            Syntax.Cursorless.Hole_e_CLess

        Syntax.Base.Cursor_e cursor ->
            Debug.todo "Not wellformed"


toCLess_id : Syntax.Base.Id -> Syntax.Cursorless.Id_CLess
toCLess_id id =
    case id of
        Syntax.Base.Ident lit ->
            Syntax.Cursorless.Ident_CLess lit

        Syntax.Base.Hole_id ->
            Syntax.Cursorless.Hole_id_CLess

        Syntax.Base.Cursor_id cursor ->
            Debug.todo "Not wellformed"


toCLess : Syntax.Base.Base -> Syntax.Cursorless.CursorLess
toCLess base =
    case base of
        Syntax.Base.P arg1 ->
            Syntax.Cursorless.P_CLess (toCLess_p arg1)

        Syntax.Base.B arg1 ->
            Syntax.Cursorless.B_CLess (toCLess_b arg1)

        Syntax.Base.Bi arg1 ->
            Syntax.Cursorless.Bi_CLess (toCLess_bi arg1)

        Syntax.Base.Vd arg1 ->
            Syntax.Cursorless.Vd_CLess (toCLess_vd arg1)

        Syntax.Base.Fd arg1 ->
            Syntax.Cursorless.Fd_CLess (toCLess_fd arg1)

        Syntax.Base.S arg1 ->
            Syntax.Cursorless.S_CLess (toCLess_s arg1)

        Syntax.Base.Fa arg1 ->
            Syntax.Cursorless.Fa_CLess (toCLess_fa arg1)

        Syntax.Base.Cond arg1 ->
            Syntax.Cursorless.Cond_CLess (toCLess_cond arg1)

        Syntax.Base.T arg1 ->
            Syntax.Cursorless.T_CLess (toCLess_t arg1)

        Syntax.Base.E arg1 ->
            Syntax.Cursorless.E_CLess (toCLess_e arg1)

        Syntax.Base.Id arg1 ->
            Syntax.Cursorless.Id_CLess (toCLess_id arg1)


toCCtx_p : Syntax.Base.P -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx_p p path =
    case path of
        [] ->
            ( Syntax.CCtx.Cctx_hole, Syntax.Base.P p )

        i :: rest ->
            case p of
                Syntax.Base.Program arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_fd arg1 rest
                            in
                            ( Syntax.CCtx.Program_CLess_cctx1 cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Hole_p ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Cursor_p _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_b : Syntax.Base.B -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx_b b path =
    case path of
        [] ->
            ( Syntax.CCtx.Cctx_hole, Syntax.Base.B b )

        i :: rest ->
            case b of
                Syntax.Base.Block arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_bi arg1 rest
                            in
                            ( Syntax.CCtx.Block_CLess_cctx1 cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Hole_b ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Cursor_b _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_bi : Syntax.Base.Bi -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx_bi bi path =
    case path of
        [] ->
            ( Syntax.CCtx.Cctx_hole, Syntax.Base.Bi bi )

        i :: rest ->
            case bi of
                Syntax.Base.Blockdecls arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_vd arg1 rest
                            in
                            ( Syntax.CCtx.Blockdecls_CLess_cctx1 cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Blockstmts arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_s arg1 rest
                            in
                            ( Syntax.CCtx.Blockstmts_CLess_cctx1 cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Blockdone ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Hole_bi ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Cursor_bi _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_vd : Syntax.Base.Vd -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx_vd vd path =
    case path of
        [] ->
            ( Syntax.CCtx.Cctx_hole, Syntax.Base.Vd vd )

        i :: rest ->
            case vd of
                Syntax.Base.Vardecl arg1 arg2 (boundVars3, arg3) ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_t arg1 rest
                            in
                            ( Syntax.CCtx.Vardecl_CLess_cctx1
                                cctxChild
                                (toCLess_e arg2)
                                ( List.map toCLess_id boundVars3
                                , toCLess_bi arg3
                                )
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_e arg2 rest
                            in
                            ( Syntax.CCtx.Vardecl_CLess_cctx2
                                (toCLess_t arg1)
                                cctxChild
                                ( List.map toCLess_id boundVars3
                                , toCLess_bi arg3
                                )
                            , restTree
                            )

                        3 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_bi arg3 rest
                            in
                            ( Syntax.CCtx.Vardecl_CLess_cctx3
                                (toCLess_t arg1)
                                (toCLess_e arg2)
                                ( List.map toCLess_id boundVars3, cctxChild )
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Hole_vd ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Cursor_vd _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_fd : Syntax.Base.Fd -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx_fd fd path =
    case path of
        [] ->
            ( Syntax.CCtx.Cctx_hole, Syntax.Base.Fd fd )

        i :: rest ->
            case fd of
                Syntax.Base.Fundecl1 arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_t arg1 rest
                            in
                            ( Syntax.CCtx.Fundecl1_CLess_cctx1
                                cctxChild
                                ( List.map toCLess_id boundVars2
                                , toCLess_fd arg2
                                )
                                (toCLess_t arg3)
                                ( List.map toCLess_id boundVars4
                                , toCLess_b arg4
                                )
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_fd arg2 rest
                            in
                            ( Syntax.CCtx.Fundecl1_CLess_cctx2
                                (toCLess_t arg1)
                                ( List.map toCLess_id boundVars2, cctxChild )
                                (toCLess_t arg3)
                                ( List.map toCLess_id boundVars4
                                , toCLess_b arg4
                                )
                            , restTree
                            )

                        3 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_t arg3 rest
                            in
                            ( Syntax.CCtx.Fundecl1_CLess_cctx3
                                (toCLess_t arg1)
                                ( List.map toCLess_id boundVars2
                                , toCLess_fd arg2
                                )
                                cctxChild
                                ( List.map toCLess_id boundVars4
                                , toCLess_b arg4
                                )
                            , restTree
                            )

                        4 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_b arg4 rest
                            in
                            ( Syntax.CCtx.Fundecl1_CLess_cctx4
                                (toCLess_t arg1)
                                ( List.map toCLess_id boundVars2
                                , toCLess_fd arg2
                                )
                                (toCLess_t arg3)
                                ( List.map toCLess_id boundVars4, cctxChild )
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Fundecl2 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_t arg1 rest
                            in
                            ( Syntax.CCtx.Fundecl2_CLess_cctx1
                                cctxChild
                                ( List.map toCLess_id boundVars2
                                , toCLess_fd arg2
                                )
                                (toCLess_t arg3)
                                (toCLess_t arg4)
                                ( List.map toCLess_id boundVars5
                                , toCLess_b arg5
                                )
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_fd arg2 rest
                            in
                            ( Syntax.CCtx.Fundecl2_CLess_cctx2
                                (toCLess_t arg1)
                                ( List.map toCLess_id boundVars2, cctxChild )
                                (toCLess_t arg3)
                                (toCLess_t arg4)
                                ( List.map toCLess_id boundVars5
                                , toCLess_b arg5
                                )
                            , restTree
                            )

                        3 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_t arg3 rest
                            in
                            ( Syntax.CCtx.Fundecl2_CLess_cctx3
                                (toCLess_t arg1)
                                ( List.map toCLess_id boundVars2
                                , toCLess_fd arg2
                                )
                                cctxChild
                                (toCLess_t arg4)
                                ( List.map toCLess_id boundVars5
                                , toCLess_b arg5
                                )
                            , restTree
                            )

                        4 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_t arg4 rest
                            in
                            ( Syntax.CCtx.Fundecl2_CLess_cctx4
                                (toCLess_t arg1)
                                ( List.map toCLess_id boundVars2
                                , toCLess_fd arg2
                                )
                                (toCLess_t arg3)
                                cctxChild
                                ( List.map toCLess_id boundVars5
                                , toCLess_b arg5
                                )
                            , restTree
                            )

                        5 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_b arg5 rest
                            in
                            ( Syntax.CCtx.Fundecl2_CLess_cctx5
                                (toCLess_t arg1)
                                ( List.map toCLess_id boundVars2
                                , toCLess_fd arg2
                                )
                                (toCLess_t arg3)
                                (toCLess_t arg4)
                                ( List.map toCLess_id boundVars5, cctxChild )
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Fundecldone ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Hole_fd ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Cursor_fd _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_s : Syntax.Base.S -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx_s s path =
    case path of
        [] ->
            ( Syntax.CCtx.Cctx_hole, Syntax.Base.S s )

        i :: rest ->
            case s of
                Syntax.Base.Assignment arg1 arg2 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg1 rest
                            in
                            ( Syntax.CCtx.Assignment_CLess_cctx1
                                cctxChild
                                (toCLess_e arg2)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_e arg2 rest
                            in
                            ( Syntax.CCtx.Assignment_CLess_cctx2
                                (toCLess_id arg1)
                                cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Stmtfuncall arg1 arg2 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg1 rest
                            in
                            ( Syntax.CCtx.Stmtfuncall_CLess_cctx1
                                cctxChild
                                (toCLess_fa arg2)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_fa arg2 rest
                            in
                            ( Syntax.CCtx.Stmtfuncall_CLess_cctx2
                                (toCLess_id arg1)
                                cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Return arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_e arg1 rest
                            in
                            ( Syntax.CCtx.Return_CLess_cctx1 cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Conditional arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_cond arg1 rest
                            in
                            ( Syntax.CCtx.Conditional_CLess_cctx1 cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Compstmt arg1 arg2 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_s arg1 rest
                            in
                            ( Syntax.CCtx.Compstmt_CLess_cctx1
                                cctxChild
                                (toCLess_s arg2)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_s arg2 rest
                            in
                            ( Syntax.CCtx.Compstmt_CLess_cctx2
                                (toCLess_s arg1)
                                cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Hole_s ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Cursor_s _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_fa : Syntax.Base.Fa -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx_fa fa path =
    case path of
        [] ->
            ( Syntax.CCtx.Cctx_hole, Syntax.Base.Fa fa )

        i :: rest ->
            case fa of
                Syntax.Base.Funarg arg1 arg2 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_t arg1 rest
                            in
                            ( Syntax.CCtx.Funarg_CLess_cctx1
                                cctxChild
                                (toCLess_id arg2)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg2 rest
                            in
                            ( Syntax.CCtx.Funarg_CLess_cctx2
                                (toCLess_t arg1)
                                cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Funargs arg1 arg2 arg3 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_t arg1 rest
                            in
                            ( Syntax.CCtx.Funargs_CLess_cctx1
                                cctxChild
                                (toCLess_id arg2)
                                (toCLess_fa arg3)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg2 rest
                            in
                            ( Syntax.CCtx.Funargs_CLess_cctx2
                                (toCLess_t arg1)
                                cctxChild
                                (toCLess_fa arg3)
                            , restTree
                            )

                        3 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_fa arg3 rest
                            in
                            ( Syntax.CCtx.Funargs_CLess_cctx3
                                (toCLess_t arg1)
                                (toCLess_id arg2)
                                cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Hole_fa ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Cursor_fa _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_cond :
    Syntax.Base.Cond -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx_cond cond path =
    case path of
        [] ->
            ( Syntax.CCtx.Cctx_hole, Syntax.Base.Cond cond )

        i :: rest ->
            case cond of
                Syntax.Base.Ifelse arg1 arg2 arg3 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_e arg1 rest
                            in
                            ( Syntax.CCtx.Ifelse_CLess_cctx1
                                cctxChild
                                (toCLess_b arg2)
                                (toCLess_b arg3)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_b arg2 rest
                            in
                            ( Syntax.CCtx.Ifelse_CLess_cctx2
                                (toCLess_e arg1)
                                cctxChild
                                (toCLess_b arg3)
                            , restTree
                            )

                        3 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_b arg3 rest
                            in
                            ( Syntax.CCtx.Ifelse_CLess_cctx3
                                (toCLess_e arg1)
                                (toCLess_b arg2)
                                cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Hole_cond ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Cursor_cond _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_t : Syntax.Base.T -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx_t t path =
    case path of
        [] ->
            ( Syntax.CCtx.Cctx_hole, Syntax.Base.T t )

        i :: rest ->
            case t of
                Syntax.Base.Tint ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Tchar ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Tbool ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Hole_t ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Cursor_t _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_e : Syntax.Base.E -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx_e e path =
    case path of
        [] ->
            ( Syntax.CCtx.Cctx_hole, Syntax.Base.E e )

        i :: rest ->
            case e of
                Syntax.Base.Int lit ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Char lit ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Bool lit ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Plus arg1 arg2 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_e arg1 rest
                            in
                            ( Syntax.CCtx.Plus_CLess_cctx1
                                cctxChild
                                (toCLess_e arg2)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_e arg2 rest
                            in
                            ( Syntax.CCtx.Plus_CLess_cctx2
                                (toCLess_e arg1)
                                cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Equals arg1 arg2 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_e arg1 rest
                            in
                            ( Syntax.CCtx.Equals_CLess_cctx1
                                cctxChild
                                (toCLess_e arg2)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_e arg2 rest
                            in
                            ( Syntax.CCtx.Equals_CLess_cctx2
                                (toCLess_e arg1)
                                cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Expfuncall arg1 arg2 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg1 rest
                            in
                            ( Syntax.CCtx.Expfuncall_CLess_cctx1
                                cctxChild
                                (toCLess_fa arg2)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_fa arg2 rest
                            in
                            ( Syntax.CCtx.Expfuncall_CLess_cctx2
                                (toCLess_id arg1)
                                cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Expident arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg1 rest
                            in
                            ( Syntax.CCtx.Expident_CLess_cctx1 cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Hole_e ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Cursor_e _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_id : Syntax.Base.Id -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx_id id path =
    case path of
        [] ->
            ( Syntax.CCtx.Cctx_hole, Syntax.Base.Id id )

        i :: rest ->
            case id of
                Syntax.Base.Ident lit ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Hole_id ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Cursor_id _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx : Syntax.Base.Base -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx base path =
    case base of
        Syntax.Base.P arg1 ->
            toCCtx_p arg1 path

        Syntax.Base.B arg1 ->
            toCCtx_b arg1 path

        Syntax.Base.Bi arg1 ->
            toCCtx_bi arg1 path

        Syntax.Base.Vd arg1 ->
            toCCtx_vd arg1 path

        Syntax.Base.Fd arg1 ->
            toCCtx_fd arg1 path

        Syntax.Base.S arg1 ->
            toCCtx_s arg1 path

        Syntax.Base.Fa arg1 ->
            toCCtx_fa arg1 path

        Syntax.Base.Cond arg1 ->
            toCCtx_cond arg1 path

        Syntax.Base.T arg1 ->
            toCCtx_t arg1 path

        Syntax.Base.E arg1 ->
            toCCtx_e arg1 path

        Syntax.Base.Id arg1 ->
            toCCtx_id arg1 path


toWellformed : Syntax.Base.Base -> Syntax.Wellformed.Wellformed
toWellformed base =
    case consumeCursor base of
        Syntax.Base.P arg1 ->
            Syntax.Wellformed.Root_p_CLess (toCLess_p arg1)

        Syntax.Base.B arg1 ->
            Syntax.Wellformed.Root_b_CLess (toCLess_b arg1)

        Syntax.Base.Bi arg1 ->
            Syntax.Wellformed.Root_bi_CLess (toCLess_bi arg1)

        Syntax.Base.Vd arg1 ->
            Syntax.Wellformed.Root_vd_CLess (toCLess_vd arg1)

        Syntax.Base.Fd arg1 ->
            Syntax.Wellformed.Root_fd_CLess (toCLess_fd arg1)

        Syntax.Base.S arg1 ->
            Syntax.Wellformed.Root_s_CLess (toCLess_s arg1)

        Syntax.Base.Fa arg1 ->
            Syntax.Wellformed.Root_fa_CLess (toCLess_fa arg1)

        Syntax.Base.Cond arg1 ->
            Syntax.Wellformed.Root_cond_CLess (toCLess_cond arg1)

        Syntax.Base.T arg1 ->
            Syntax.Wellformed.Root_t_CLess (toCLess_t arg1)

        Syntax.Base.E arg1 ->
            Syntax.Wellformed.Root_e_CLess (toCLess_e arg1)

        Syntax.Base.Id arg1 ->
            Syntax.Wellformed.Root_id_CLess (toCLess_id arg1)


consumeCursor : Syntax.Base.Base -> Syntax.Base.Base
consumeCursor base =
    case base of
        Syntax.Base.P arg1 ->
            case arg1 of
                Syntax.Base.Cursor_p underCursor ->
                    Syntax.Base.P underCursor

                _ ->
                    Syntax.Base.P arg1

        Syntax.Base.B arg1 ->
            case arg1 of
                Syntax.Base.Cursor_b underCursor ->
                    Syntax.Base.B underCursor

                _ ->
                    Syntax.Base.B arg1

        Syntax.Base.Bi arg1 ->
            case arg1 of
                Syntax.Base.Cursor_bi underCursor ->
                    Syntax.Base.Bi underCursor

                _ ->
                    Syntax.Base.Bi arg1

        Syntax.Base.Vd arg1 ->
            case arg1 of
                Syntax.Base.Cursor_vd underCursor ->
                    Syntax.Base.Vd underCursor

                _ ->
                    Syntax.Base.Vd arg1

        Syntax.Base.Fd arg1 ->
            case arg1 of
                Syntax.Base.Cursor_fd underCursor ->
                    Syntax.Base.Fd underCursor

                _ ->
                    Syntax.Base.Fd arg1

        Syntax.Base.S arg1 ->
            case arg1 of
                Syntax.Base.Cursor_s underCursor ->
                    Syntax.Base.S underCursor

                _ ->
                    Syntax.Base.S arg1

        Syntax.Base.Fa arg1 ->
            case arg1 of
                Syntax.Base.Cursor_fa underCursor ->
                    Syntax.Base.Fa underCursor

                _ ->
                    Syntax.Base.Fa arg1

        Syntax.Base.Cond arg1 ->
            case arg1 of
                Syntax.Base.Cursor_cond underCursor ->
                    Syntax.Base.Cond underCursor

                _ ->
                    Syntax.Base.Cond arg1

        Syntax.Base.T arg1 ->
            case arg1 of
                Syntax.Base.Cursor_t underCursor ->
                    Syntax.Base.T underCursor

                _ ->
                    Syntax.Base.T arg1

        Syntax.Base.E arg1 ->
            case arg1 of
                Syntax.Base.Cursor_e underCursor ->
                    Syntax.Base.E underCursor

                _ ->
                    Syntax.Base.E arg1

        Syntax.Base.Id arg1 ->
            case arg1 of
                Syntax.Base.Cursor_id underCursor ->
                    Syntax.Base.Id underCursor

                _ ->
                    Syntax.Base.Id arg1


decompose :
    Syntax.Base.Base -> ( Syntax.CCtx.Cctx, Syntax.Wellformed.Wellformed )
decompose base =
    let
        ( cctx, rest ) =
            toCCtx base (getCursorPath [] base)
    in
    ( cctx, toWellformed rest )


replaceCctxHole :
    Int -> Syntax.CCtx.Cctx -> Syntax.Cursorless.CursorLess -> Syntax.CCtx.Cctx
replaceCctxHole i orig_cctx underCursor =
    case orig_cctx of
        Syntax.CCtx.Cctx_hole ->
            case underCursor of
                Syntax.Cursorless.P_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.Program_CLess arg1 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Program_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Hole_p_CLess ->
                            Debug.todo "Invalid replacement"

                Syntax.Cursorless.B_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.Block_CLess arg1 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Block_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Hole_b_CLess ->
                            Debug.todo "Invalid replacement"

                Syntax.Cursorless.Bi_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.Blockdecls_CLess arg1 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Blockdecls_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Blockstmts_CLess arg1 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Blockstmts_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Blockdone_CLess ->
                            Debug.todo "Invalid replacement"

                        Syntax.Cursorless.Hole_bi_CLess ->
                            Debug.todo "Invalid replacement"

                Syntax.Cursorless.Vd_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.Vardecl_CLess arg1 arg2 (boundVars3, arg3) ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Vardecl_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole
                                        arg2
                                        (boundVars3, arg3)

                                2 ->
                                    Syntax.CCtx.Vardecl_CLess_cctx2
                                        arg1
                                        Syntax.CCtx.Cctx_hole
                                        (boundVars3, arg3)

                                3 ->
                                    Syntax.CCtx.Vardecl_CLess_cctx3
                                        arg1
                                        arg2
                                        ( boundVars3, Syntax.CCtx.Cctx_hole )

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Hole_vd_CLess ->
                            Debug.todo "Invalid replacement"

                Syntax.Cursorless.Fd_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.Fundecl1_CLess arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Fundecl1_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole
                                        (boundVars2, arg2)
                                        arg3
                                        (boundVars4, arg4)

                                2 ->
                                    Syntax.CCtx.Fundecl1_CLess_cctx2
                                        arg1
                                        ( boundVars2, Syntax.CCtx.Cctx_hole )
                                        arg3
                                        (boundVars4, arg4)

                                3 ->
                                    Syntax.CCtx.Fundecl1_CLess_cctx3
                                        arg1
                                        (boundVars2, arg2)
                                        Syntax.CCtx.Cctx_hole
                                        (boundVars4, arg4)

                                4 ->
                                    Syntax.CCtx.Fundecl1_CLess_cctx4
                                        arg1
                                        (boundVars2, arg2)
                                        arg3
                                        ( boundVars4, Syntax.CCtx.Cctx_hole )

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Fundecl2_CLess arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Fundecl2_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole
                                        (boundVars2, arg2)
                                        arg3
                                        arg4
                                        (boundVars5, arg5)

                                2 ->
                                    Syntax.CCtx.Fundecl2_CLess_cctx2
                                        arg1
                                        ( boundVars2, Syntax.CCtx.Cctx_hole )
                                        arg3
                                        arg4
                                        (boundVars5, arg5)

                                3 ->
                                    Syntax.CCtx.Fundecl2_CLess_cctx3
                                        arg1
                                        (boundVars2, arg2)
                                        Syntax.CCtx.Cctx_hole
                                        arg4
                                        (boundVars5, arg5)

                                4 ->
                                    Syntax.CCtx.Fundecl2_CLess_cctx4
                                        arg1
                                        (boundVars2, arg2)
                                        arg3
                                        Syntax.CCtx.Cctx_hole
                                        (boundVars5, arg5)

                                5 ->
                                    Syntax.CCtx.Fundecl2_CLess_cctx5
                                        arg1
                                        (boundVars2, arg2)
                                        arg3
                                        arg4
                                        ( boundVars5, Syntax.CCtx.Cctx_hole )

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Fundecldone_CLess ->
                            Debug.todo "Invalid replacement"

                        Syntax.Cursorless.Hole_fd_CLess ->
                            Debug.todo "Invalid replacement"

                Syntax.Cursorless.S_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.Assignment_CLess arg1 arg2 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Assignment_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole
                                        arg2

                                2 ->
                                    Syntax.CCtx.Assignment_CLess_cctx2
                                        arg1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Stmtfuncall_CLess arg1 arg2 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Stmtfuncall_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole
                                        arg2

                                2 ->
                                    Syntax.CCtx.Stmtfuncall_CLess_cctx2
                                        arg1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Return_CLess arg1 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Return_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Conditional_CLess arg1 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Conditional_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Compstmt_CLess arg1 arg2 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Compstmt_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole
                                        arg2

                                2 ->
                                    Syntax.CCtx.Compstmt_CLess_cctx2
                                        arg1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Hole_s_CLess ->
                            Debug.todo "Invalid replacement"

                Syntax.Cursorless.Fa_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.Funarg_CLess arg1 arg2 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Funarg_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole
                                        arg2

                                2 ->
                                    Syntax.CCtx.Funarg_CLess_cctx2
                                        arg1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Funargs_CLess arg1 arg2 arg3 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Funargs_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole
                                        arg2
                                        arg3

                                2 ->
                                    Syntax.CCtx.Funargs_CLess_cctx2
                                        arg1
                                        Syntax.CCtx.Cctx_hole
                                        arg3

                                3 ->
                                    Syntax.CCtx.Funargs_CLess_cctx3
                                        arg1
                                        arg2
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Hole_fa_CLess ->
                            Debug.todo "Invalid replacement"

                Syntax.Cursorless.Cond_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.Ifelse_CLess arg1 arg2 arg3 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Ifelse_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole
                                        arg2
                                        arg3

                                2 ->
                                    Syntax.CCtx.Ifelse_CLess_cctx2
                                        arg1
                                        Syntax.CCtx.Cctx_hole
                                        arg3

                                3 ->
                                    Syntax.CCtx.Ifelse_CLess_cctx3
                                        arg1
                                        arg2
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Hole_cond_CLess ->
                            Debug.todo "Invalid replacement"

                Syntax.Cursorless.T_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.Tint_CLess ->
                            Debug.todo "Invalid replacement"

                        Syntax.Cursorless.Tchar_CLess ->
                            Debug.todo "Invalid replacement"

                        Syntax.Cursorless.Tbool_CLess ->
                            Debug.todo "Invalid replacement"

                        Syntax.Cursorless.Hole_t_CLess ->
                            Debug.todo "Invalid replacement"

                Syntax.Cursorless.E_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.Int_CLess lit ->
                            Debug.todo "Invalid replacement"

                        Syntax.Cursorless.Char_CLess lit ->
                            Debug.todo "Invalid replacement"

                        Syntax.Cursorless.Bool_CLess lit ->
                            Debug.todo "Invalid replacement"

                        Syntax.Cursorless.Plus_CLess arg1 arg2 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Plus_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole
                                        arg2

                                2 ->
                                    Syntax.CCtx.Plus_CLess_cctx2
                                        arg1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Equals_CLess arg1 arg2 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Equals_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole
                                        arg2

                                2 ->
                                    Syntax.CCtx.Equals_CLess_cctx2
                                        arg1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Expfuncall_CLess arg1 arg2 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Expfuncall_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole
                                        arg2

                                2 ->
                                    Syntax.CCtx.Expfuncall_CLess_cctx2
                                        arg1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Expident_CLess arg1 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Expident_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Hole_e_CLess ->
                            Debug.todo "Invalid replacement"

                Syntax.Cursorless.Id_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.Ident_CLess lit ->
                            Debug.todo "Invalid replacement"

                        Syntax.Cursorless.Hole_id_CLess ->
                            Debug.todo "Invalid replacement"

        Syntax.CCtx.Program_CLess_cctx1 cctx ->
            Syntax.CCtx.Program_CLess_cctx1 (replaceCctxHole i cctx underCursor)

        Syntax.CCtx.Block_CLess_cctx1 cctx ->
            Syntax.CCtx.Block_CLess_cctx1 (replaceCctxHole i cctx underCursor)

        Syntax.CCtx.Blockdecls_CLess_cctx1 cctx ->
            Syntax.CCtx.Blockdecls_CLess_cctx1
                (replaceCctxHole i cctx underCursor)

        Syntax.CCtx.Blockstmts_CLess_cctx1 cctx ->
            Syntax.CCtx.Blockstmts_CLess_cctx1
                (replaceCctxHole i cctx underCursor)

        Syntax.CCtx.Vardecl_CLess_cctx1 arg1 arg2 (boundVars3, arg3) ->
            Syntax.CCtx.Vardecl_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                arg2
                (boundVars3, arg3)

        Syntax.CCtx.Vardecl_CLess_cctx2 arg1 arg2 (boundVars3, arg3) ->
            Syntax.CCtx.Vardecl_CLess_cctx2
                arg1
                (replaceCctxHole i arg2 underCursor)
                (boundVars3, arg3)

        Syntax.CCtx.Vardecl_CLess_cctx3 arg1 arg2 (boundVars3, arg3) ->
            Syntax.CCtx.Vardecl_CLess_cctx3
                arg1
                arg2
                ( boundVars3, replaceCctxHole i arg3 underCursor )

        Syntax.CCtx.Fundecl1_CLess_cctx1 arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
            Syntax.CCtx.Fundecl1_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                (boundVars2, arg2)
                arg3
                (boundVars4, arg4)

        Syntax.CCtx.Fundecl1_CLess_cctx2 arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
            Syntax.CCtx.Fundecl1_CLess_cctx2
                arg1
                ( boundVars2, replaceCctxHole i arg2 underCursor )
                arg3
                (boundVars4, arg4)

        Syntax.CCtx.Fundecl1_CLess_cctx3 arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
            Syntax.CCtx.Fundecl1_CLess_cctx3
                arg1
                (boundVars2, arg2)
                (replaceCctxHole i arg3 underCursor)
                (boundVars4, arg4)

        Syntax.CCtx.Fundecl1_CLess_cctx4 arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
            Syntax.CCtx.Fundecl1_CLess_cctx4
                arg1
                (boundVars2, arg2)
                arg3
                ( boundVars4, replaceCctxHole i arg4 underCursor )

        Syntax.CCtx.Fundecl2_CLess_cctx1 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
            Syntax.CCtx.Fundecl2_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                (boundVars2, arg2)
                arg3
                arg4
                (boundVars5, arg5)

        Syntax.CCtx.Fundecl2_CLess_cctx2 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
            Syntax.CCtx.Fundecl2_CLess_cctx2
                arg1
                ( boundVars2, replaceCctxHole i arg2 underCursor )
                arg3
                arg4
                (boundVars5, arg5)

        Syntax.CCtx.Fundecl2_CLess_cctx3 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
            Syntax.CCtx.Fundecl2_CLess_cctx3
                arg1
                (boundVars2, arg2)
                (replaceCctxHole i arg3 underCursor)
                arg4
                (boundVars5, arg5)

        Syntax.CCtx.Fundecl2_CLess_cctx4 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
            Syntax.CCtx.Fundecl2_CLess_cctx4
                arg1
                (boundVars2, arg2)
                arg3
                (replaceCctxHole i arg4 underCursor)
                (boundVars5, arg5)

        Syntax.CCtx.Fundecl2_CLess_cctx5 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
            Syntax.CCtx.Fundecl2_CLess_cctx5
                arg1
                (boundVars2, arg2)
                arg3
                arg4
                ( boundVars5, replaceCctxHole i arg5 underCursor )

        Syntax.CCtx.Assignment_CLess_cctx1 arg1 arg2 ->
            Syntax.CCtx.Assignment_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                arg2

        Syntax.CCtx.Assignment_CLess_cctx2 arg1 arg2 ->
            Syntax.CCtx.Assignment_CLess_cctx2
                arg1
                (replaceCctxHole i arg2 underCursor)

        Syntax.CCtx.Stmtfuncall_CLess_cctx1 arg1 arg2 ->
            Syntax.CCtx.Stmtfuncall_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                arg2

        Syntax.CCtx.Stmtfuncall_CLess_cctx2 arg1 arg2 ->
            Syntax.CCtx.Stmtfuncall_CLess_cctx2
                arg1
                (replaceCctxHole i arg2 underCursor)

        Syntax.CCtx.Return_CLess_cctx1 cctx ->
            Syntax.CCtx.Return_CLess_cctx1 (replaceCctxHole i cctx underCursor)

        Syntax.CCtx.Conditional_CLess_cctx1 cctx ->
            Syntax.CCtx.Conditional_CLess_cctx1
                (replaceCctxHole i cctx underCursor)

        Syntax.CCtx.Compstmt_CLess_cctx1 arg1 arg2 ->
            Syntax.CCtx.Compstmt_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                arg2

        Syntax.CCtx.Compstmt_CLess_cctx2 arg1 arg2 ->
            Syntax.CCtx.Compstmt_CLess_cctx2
                arg1
                (replaceCctxHole i arg2 underCursor)

        Syntax.CCtx.Funarg_CLess_cctx1 arg1 arg2 ->
            Syntax.CCtx.Funarg_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                arg2

        Syntax.CCtx.Funarg_CLess_cctx2 arg1 arg2 ->
            Syntax.CCtx.Funarg_CLess_cctx2
                arg1
                (replaceCctxHole i arg2 underCursor)

        Syntax.CCtx.Funargs_CLess_cctx1 arg1 arg2 arg3 ->
            Syntax.CCtx.Funargs_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                arg2
                arg3

        Syntax.CCtx.Funargs_CLess_cctx2 arg1 arg2 arg3 ->
            Syntax.CCtx.Funargs_CLess_cctx2
                arg1
                (replaceCctxHole i arg2 underCursor)
                arg3

        Syntax.CCtx.Funargs_CLess_cctx3 arg1 arg2 arg3 ->
            Syntax.CCtx.Funargs_CLess_cctx3
                arg1
                arg2
                (replaceCctxHole i arg3 underCursor)

        Syntax.CCtx.Ifelse_CLess_cctx1 arg1 arg2 arg3 ->
            Syntax.CCtx.Ifelse_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                arg2
                arg3

        Syntax.CCtx.Ifelse_CLess_cctx2 arg1 arg2 arg3 ->
            Syntax.CCtx.Ifelse_CLess_cctx2
                arg1
                (replaceCctxHole i arg2 underCursor)
                arg3

        Syntax.CCtx.Ifelse_CLess_cctx3 arg1 arg2 arg3 ->
            Syntax.CCtx.Ifelse_CLess_cctx3
                arg1
                arg2
                (replaceCctxHole i arg3 underCursor)

        Syntax.CCtx.Plus_CLess_cctx1 arg1 arg2 ->
            Syntax.CCtx.Plus_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                arg2

        Syntax.CCtx.Plus_CLess_cctx2 arg1 arg2 ->
            Syntax.CCtx.Plus_CLess_cctx2
                arg1
                (replaceCctxHole i arg2 underCursor)

        Syntax.CCtx.Equals_CLess_cctx1 arg1 arg2 ->
            Syntax.CCtx.Equals_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                arg2

        Syntax.CCtx.Equals_CLess_cctx2 arg1 arg2 ->
            Syntax.CCtx.Equals_CLess_cctx2
                arg1
                (replaceCctxHole i arg2 underCursor)

        Syntax.CCtx.Expfuncall_CLess_cctx1 arg1 arg2 ->
            Syntax.CCtx.Expfuncall_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                arg2

        Syntax.CCtx.Expfuncall_CLess_cctx2 arg1 arg2 ->
            Syntax.CCtx.Expfuncall_CLess_cctx2
                arg1
                (replaceCctxHole i arg2 underCursor)

        Syntax.CCtx.Expident_CLess_cctx1 cctx ->
            Syntax.CCtx.Expident_CLess_cctx1
                (replaceCctxHole i cctx underCursor)


child :
    Int
    -> ( Syntax.CCtx.Cctx, Syntax.Wellformed.Wellformed )
    -> Maybe ( Syntax.CCtx.Cctx, Syntax.Wellformed.Wellformed )
child i decomposed =
    let
        ( cctx, wellformed ) =
            decomposed
    in
    case wellformed of
        Syntax.Wellformed.Root_p_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.Program_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.P_CLess underCursor)
                                , Syntax.Wellformed.Root_fd_CLess arg1
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Hole_p_CLess ->
                    Nothing

        Syntax.Wellformed.Root_s_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.Assignment_CLess arg1 arg2 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.S_CLess underCursor)
                                , Syntax.Wellformed.Root_id_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.S_CLess underCursor)
                                , Syntax.Wellformed.Root_e_CLess arg2
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Stmtfuncall_CLess arg1 arg2 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.S_CLess underCursor)
                                , Syntax.Wellformed.Root_id_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.S_CLess underCursor)
                                , Syntax.Wellformed.Root_fa_CLess arg2
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Return_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.S_CLess underCursor)
                                , Syntax.Wellformed.Root_e_CLess arg1
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Conditional_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.S_CLess underCursor)
                                , Syntax.Wellformed.Root_cond_CLess arg1
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Compstmt_CLess arg1 arg2 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.S_CLess underCursor)
                                , Syntax.Wellformed.Root_s_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.S_CLess underCursor)
                                , Syntax.Wellformed.Root_s_CLess arg2
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Hole_s_CLess ->
                    Nothing

        Syntax.Wellformed.Root_vd_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.Vardecl_CLess arg1 arg2 (boundVars3, arg3) ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Vd_CLess underCursor)
                                , Syntax.Wellformed.Root_t_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Vd_CLess underCursor)
                                , Syntax.Wellformed.Root_e_CLess arg2
                                )

                        3 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Vd_CLess underCursor)
                                , Syntax.Wellformed.Root_bi_CLess arg3
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Hole_vd_CLess ->
                    Nothing

        Syntax.Wellformed.Root_fd_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.Fundecl1_CLess arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Fd_CLess underCursor)
                                , Syntax.Wellformed.Root_t_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Fd_CLess underCursor)
                                , Syntax.Wellformed.Root_fd_CLess arg2
                                )

                        3 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Fd_CLess underCursor)
                                , Syntax.Wellformed.Root_t_CLess arg3
                                )

                        4 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Fd_CLess underCursor)
                                , Syntax.Wellformed.Root_b_CLess arg4
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Fundecl2_CLess arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Fd_CLess underCursor)
                                , Syntax.Wellformed.Root_t_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Fd_CLess underCursor)
                                , Syntax.Wellformed.Root_fd_CLess arg2
                                )

                        3 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Fd_CLess underCursor)
                                , Syntax.Wellformed.Root_t_CLess arg3
                                )

                        4 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Fd_CLess underCursor)
                                , Syntax.Wellformed.Root_t_CLess arg4
                                )

                        5 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Fd_CLess underCursor)
                                , Syntax.Wellformed.Root_b_CLess arg5
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Fundecldone_CLess ->
                    Nothing

                Syntax.Cursorless.Hole_fd_CLess ->
                    Nothing

        Syntax.Wellformed.Root_t_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.Tint_CLess ->
                    Nothing

                Syntax.Cursorless.Tchar_CLess ->
                    Nothing

                Syntax.Cursorless.Tbool_CLess ->
                    Nothing

                Syntax.Cursorless.Hole_t_CLess ->
                    Nothing

        Syntax.Wellformed.Root_id_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.Ident_CLess lit ->
                    Nothing

                Syntax.Cursorless.Hole_id_CLess ->
                    Nothing

        Syntax.Wellformed.Root_e_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.Int_CLess lit ->
                    Nothing

                Syntax.Cursorless.Char_CLess lit ->
                    Nothing

                Syntax.Cursorless.Bool_CLess lit ->
                    Nothing

                Syntax.Cursorless.Plus_CLess arg1 arg2 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.E_CLess underCursor)
                                , Syntax.Wellformed.Root_e_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.E_CLess underCursor)
                                , Syntax.Wellformed.Root_e_CLess arg2
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Equals_CLess arg1 arg2 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.E_CLess underCursor)
                                , Syntax.Wellformed.Root_e_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.E_CLess underCursor)
                                , Syntax.Wellformed.Root_e_CLess arg2
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Expfuncall_CLess arg1 arg2 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.E_CLess underCursor)
                                , Syntax.Wellformed.Root_id_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.E_CLess underCursor)
                                , Syntax.Wellformed.Root_fa_CLess arg2
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Expident_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.E_CLess underCursor)
                                , Syntax.Wellformed.Root_id_CLess arg1
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Hole_e_CLess ->
                    Nothing

        Syntax.Wellformed.Root_b_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.Block_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.B_CLess underCursor)
                                , Syntax.Wellformed.Root_bi_CLess arg1
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Hole_b_CLess ->
                    Nothing

        Syntax.Wellformed.Root_bi_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.Blockdecls_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Bi_CLess underCursor)
                                , Syntax.Wellformed.Root_vd_CLess arg1
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Blockstmts_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Bi_CLess underCursor)
                                , Syntax.Wellformed.Root_s_CLess arg1
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Blockdone_CLess ->
                    Nothing

                Syntax.Cursorless.Hole_bi_CLess ->
                    Nothing

        Syntax.Wellformed.Root_fa_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.Funarg_CLess arg1 arg2 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Fa_CLess underCursor)
                                , Syntax.Wellformed.Root_t_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Fa_CLess underCursor)
                                , Syntax.Wellformed.Root_id_CLess arg2
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Funargs_CLess arg1 arg2 arg3 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Fa_CLess underCursor)
                                , Syntax.Wellformed.Root_t_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Fa_CLess underCursor)
                                , Syntax.Wellformed.Root_id_CLess arg2
                                )

                        3 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Fa_CLess underCursor)
                                , Syntax.Wellformed.Root_fa_CLess arg3
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Hole_fa_CLess ->
                    Nothing

        Syntax.Wellformed.Root_cond_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.Ifelse_CLess arg1 arg2 arg3 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Cond_CLess underCursor)
                                , Syntax.Wellformed.Root_e_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Cond_CLess underCursor)
                                , Syntax.Wellformed.Root_b_CLess arg2
                                )

                        3 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Cond_CLess underCursor)
                                , Syntax.Wellformed.Root_b_CLess arg3
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Hole_cond_CLess ->
                    Nothing


substitute :
    ( Syntax.CCtx.Cctx, Syntax.Wellformed.Wellformed )
    -> Syntax.Cursorless.CursorLess
    -> Maybe ( Syntax.CCtx.Cctx, Syntax.Wellformed.Wellformed )
substitute decomposed sub =
    let
        ( cctx, wellformed ) =
            decomposed
    in
    case wellformed of
        Syntax.Wellformed.Root_p_CLess _ ->
            case sub of
                Syntax.Cursorless.P_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_p_CLess sub0 )

                _ ->
                    Nothing

        Syntax.Wellformed.Root_s_CLess _ ->
            case sub of
                Syntax.Cursorless.S_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_s_CLess sub0 )

                _ ->
                    Nothing

        Syntax.Wellformed.Root_vd_CLess _ ->
            case sub of
                Syntax.Cursorless.Vd_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_vd_CLess sub0 )

                _ ->
                    Nothing

        Syntax.Wellformed.Root_fd_CLess _ ->
            case sub of
                Syntax.Cursorless.Fd_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_fd_CLess sub0 )

                _ ->
                    Nothing

        Syntax.Wellformed.Root_t_CLess _ ->
            case sub of
                Syntax.Cursorless.T_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_t_CLess sub0 )

                _ ->
                    Nothing

        Syntax.Wellformed.Root_id_CLess _ ->
            case sub of
                Syntax.Cursorless.Id_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_id_CLess sub0 )

                _ ->
                    Nothing

        Syntax.Wellformed.Root_e_CLess _ ->
            case sub of
                Syntax.Cursorless.E_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_e_CLess sub0 )

                _ ->
                    Nothing

        Syntax.Wellformed.Root_b_CLess _ ->
            case sub of
                Syntax.Cursorless.B_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_b_CLess sub0 )

                _ ->
                    Nothing

        Syntax.Wellformed.Root_bi_CLess _ ->
            case sub of
                Syntax.Cursorless.Bi_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_bi_CLess sub0 )

                _ ->
                    Nothing

        Syntax.Wellformed.Root_fa_CLess _ ->
            case sub of
                Syntax.Cursorless.Fa_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_fa_CLess sub0 )

                _ ->
                    Nothing

        Syntax.Wellformed.Root_cond_CLess _ ->
            case sub of
                Syntax.Cursorless.Cond_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_cond_CLess sub0 )

                _ ->
                    Nothing


getCursorPath : List Int -> Syntax.Base.Base -> List Int
getCursorPath path base =
    case base of
        Syntax.Base.P p ->
            case p of
                Syntax.Base.Program arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Syntax.Base.Fd arg1)

                Syntax.Base.Hole_p ->
                    []

                Syntax.Base.Cursor_p _ ->
                    path

        Syntax.Base.B b ->
            case b of
                Syntax.Base.Block arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Syntax.Base.Bi arg1)

                Syntax.Base.Hole_b ->
                    []

                Syntax.Base.Cursor_b _ ->
                    path

        Syntax.Base.Bi bi ->
            case bi of
                Syntax.Base.Blockdecls arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Syntax.Base.Vd arg1)

                Syntax.Base.Blockstmts arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Syntax.Base.S arg1)

                Syntax.Base.Blockdone ->
                    []

                Syntax.Base.Hole_bi ->
                    []

                Syntax.Base.Cursor_bi _ ->
                    path

        Syntax.Base.Vd vd ->
            case vd of
                Syntax.Base.Vardecl arg1 arg2 (boundVars3, arg3) ->
                    (getCursorPath
                         (path ++ [ 1 ])
                         (Syntax.Base.T arg1) ++ getCursorPath
                                                         (path ++ [ 2 ])
                                                         (Syntax.Base.E arg2)
                    ) ++ getCursorPath (path ++ [ 3 ]) (Syntax.Base.Bi arg3)

                Syntax.Base.Hole_vd ->
                    []

                Syntax.Base.Cursor_vd _ ->
                    path

        Syntax.Base.Fd fd ->
            case fd of
                Syntax.Base.Fundecl1 arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
                    ((getCursorPath
                          (path ++ [ 1 ])
                          (Syntax.Base.T arg1) ++ getCursorPath
                                                          (path ++ [ 2 ])
                                                          (Syntax.Base.Fd arg2)
                     ) ++ getCursorPath (path ++ [ 3 ]) (Syntax.Base.T arg3)
                    ) ++ getCursorPath (path ++ [ 4 ]) (Syntax.Base.B arg4)

                Syntax.Base.Fundecl2 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
                    (((getCursorPath
                           (path ++ [ 1 ])
                           (Syntax.Base.T arg1) ++ getCursorPath
                                                           (path ++ [ 2 ])
                                                           (Syntax.Base.Fd arg2)
                      ) ++ getCursorPath (path ++ [ 3 ]) (Syntax.Base.T arg3)
                     ) ++ getCursorPath (path ++ [ 4 ]) (Syntax.Base.T arg4)
                    ) ++ getCursorPath (path ++ [ 5 ]) (Syntax.Base.B arg5)

                Syntax.Base.Fundecldone ->
                    []

                Syntax.Base.Hole_fd ->
                    []

                Syntax.Base.Cursor_fd _ ->
                    path

        Syntax.Base.S s ->
            case s of
                Syntax.Base.Assignment arg1 arg2 ->
                    getCursorPath
                        (path ++ [ 1 ])
                        (Syntax.Base.Id arg1) ++ getCursorPath
                                                         (path ++ [ 2 ])
                                                         (Syntax.Base.E arg2)

                Syntax.Base.Stmtfuncall arg1 arg2 ->
                    getCursorPath
                        (path ++ [ 1 ])
                        (Syntax.Base.Id arg1) ++ getCursorPath
                                                         (path ++ [ 2 ])
                                                         (Syntax.Base.Fa arg2)

                Syntax.Base.Return arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Syntax.Base.E arg1)

                Syntax.Base.Conditional arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Syntax.Base.Cond arg1)

                Syntax.Base.Compstmt arg1 arg2 ->
                    getCursorPath
                        (path ++ [ 1 ])
                        (Syntax.Base.S arg1) ++ getCursorPath
                                                        (path ++ [ 2 ])
                                                        (Syntax.Base.S arg2)

                Syntax.Base.Hole_s ->
                    []

                Syntax.Base.Cursor_s _ ->
                    path

        Syntax.Base.Fa fa ->
            case fa of
                Syntax.Base.Funarg arg1 arg2 ->
                    getCursorPath
                        (path ++ [ 1 ])
                        (Syntax.Base.T arg1) ++ getCursorPath
                                                        (path ++ [ 2 ])
                                                        (Syntax.Base.Id arg2)

                Syntax.Base.Funargs arg1 arg2 arg3 ->
                    (getCursorPath
                         (path ++ [ 1 ])
                         (Syntax.Base.T arg1) ++ getCursorPath
                                                         (path ++ [ 2 ])
                                                         (Syntax.Base.Id arg2)
                    ) ++ getCursorPath (path ++ [ 3 ]) (Syntax.Base.Fa arg3)

                Syntax.Base.Hole_fa ->
                    []

                Syntax.Base.Cursor_fa _ ->
                    path

        Syntax.Base.Cond cond ->
            case cond of
                Syntax.Base.Ifelse arg1 arg2 arg3 ->
                    (getCursorPath
                         (path ++ [ 1 ])
                         (Syntax.Base.E arg1) ++ getCursorPath
                                                         (path ++ [ 2 ])
                                                         (Syntax.Base.B arg2)
                    ) ++ getCursorPath (path ++ [ 3 ]) (Syntax.Base.B arg3)

                Syntax.Base.Hole_cond ->
                    []

                Syntax.Base.Cursor_cond _ ->
                    path

        Syntax.Base.T t ->
            case t of
                Syntax.Base.Tint ->
                    []

                Syntax.Base.Tchar ->
                    []

                Syntax.Base.Tbool ->
                    []

                Syntax.Base.Hole_t ->
                    []

                Syntax.Base.Cursor_t _ ->
                    path

        Syntax.Base.E e ->
            case e of
                Syntax.Base.Int lit ->
                    []

                Syntax.Base.Char lit ->
                    []

                Syntax.Base.Bool lit ->
                    []

                Syntax.Base.Plus arg1 arg2 ->
                    getCursorPath
                        (path ++ [ 1 ])
                        (Syntax.Base.E arg1) ++ getCursorPath
                                                        (path ++ [ 2 ])
                                                        (Syntax.Base.E arg2)

                Syntax.Base.Equals arg1 arg2 ->
                    getCursorPath
                        (path ++ [ 1 ])
                        (Syntax.Base.E arg1) ++ getCursorPath
                                                        (path ++ [ 2 ])
                                                        (Syntax.Base.E arg2)

                Syntax.Base.Expfuncall arg1 arg2 ->
                    getCursorPath
                        (path ++ [ 1 ])
                        (Syntax.Base.Id arg1) ++ getCursorPath
                                                         (path ++ [ 2 ])
                                                         (Syntax.Base.Fa arg2)

                Syntax.Base.Expident arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Syntax.Base.Id arg1)

                Syntax.Base.Hole_e ->
                    []

                Syntax.Base.Cursor_e _ ->
                    path

        Syntax.Base.Id id ->
            case id of
                Syntax.Base.Ident lit ->
                    []

                Syntax.Base.Hole_id ->
                    []

                Syntax.Base.Cursor_id _ ->
                    path


getCctxPath : Syntax.CCtx.Cctx -> List Int -> List Int
getCctxPath cctx path =
    case cctx of
        Syntax.CCtx.Cctx_hole ->
            path

        Syntax.CCtx.Program_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Block_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Blockdecls_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Blockstmts_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Vardecl_CLess_cctx1 arg1 arg2 (boundVars3, arg3) ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Vardecl_CLess_cctx2 arg1 arg2 (boundVars3, arg3) ->
            getCctxPath arg2 (path ++ [ 2 ])

        Syntax.CCtx.Vardecl_CLess_cctx3 arg1 arg2 (boundVars3, arg3) ->
            getCctxPath arg3 (path ++ [ 3 ])

        Syntax.CCtx.Fundecl1_CLess_cctx1 arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Fundecl1_CLess_cctx2 arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
            getCctxPath arg2 (path ++ [ 2 ])

        Syntax.CCtx.Fundecl1_CLess_cctx3 arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
            getCctxPath arg3 (path ++ [ 3 ])

        Syntax.CCtx.Fundecl1_CLess_cctx4 arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
            getCctxPath arg4 (path ++ [ 4 ])

        Syntax.CCtx.Fundecl2_CLess_cctx1 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Fundecl2_CLess_cctx2 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
            getCctxPath arg2 (path ++ [ 2 ])

        Syntax.CCtx.Fundecl2_CLess_cctx3 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
            getCctxPath arg3 (path ++ [ 3 ])

        Syntax.CCtx.Fundecl2_CLess_cctx4 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
            getCctxPath arg4 (path ++ [ 4 ])

        Syntax.CCtx.Fundecl2_CLess_cctx5 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
            getCctxPath arg5 (path ++ [ 5 ])

        Syntax.CCtx.Assignment_CLess_cctx1 arg1 arg2 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Assignment_CLess_cctx2 arg1 arg2 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Syntax.CCtx.Stmtfuncall_CLess_cctx1 arg1 arg2 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Stmtfuncall_CLess_cctx2 arg1 arg2 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Syntax.CCtx.Return_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Conditional_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Compstmt_CLess_cctx1 arg1 arg2 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Compstmt_CLess_cctx2 arg1 arg2 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Syntax.CCtx.Funarg_CLess_cctx1 arg1 arg2 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Funarg_CLess_cctx2 arg1 arg2 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Syntax.CCtx.Funargs_CLess_cctx1 arg1 arg2 arg3 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Funargs_CLess_cctx2 arg1 arg2 arg3 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Syntax.CCtx.Funargs_CLess_cctx3 arg1 arg2 arg3 ->
            getCctxPath arg3 (path ++ [ 3 ])

        Syntax.CCtx.Ifelse_CLess_cctx1 arg1 arg2 arg3 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Ifelse_CLess_cctx2 arg1 arg2 arg3 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Syntax.CCtx.Ifelse_CLess_cctx3 arg1 arg2 arg3 ->
            getCctxPath arg3 (path ++ [ 3 ])

        Syntax.CCtx.Plus_CLess_cctx1 arg1 arg2 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Plus_CLess_cctx2 arg1 arg2 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Syntax.CCtx.Equals_CLess_cctx1 arg1 arg2 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Equals_CLess_cctx2 arg1 arg2 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Syntax.CCtx.Expfuncall_CLess_cctx1 arg1 arg2 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Expfuncall_CLess_cctx2 arg1 arg2 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Syntax.CCtx.Expident_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])


moveCCtxHoleUp :
    Syntax.CCtx.Cctx -> List Int -> Maybe ( Syntax.CCtx.Cctx, Syntax.CCtx.Cctx )
moveCCtxHoleUp cctx path =
    case path of
        [_,_] ->
            case cctx of
                Syntax.CCtx.Cctx_hole ->
                    Nothing

                Syntax.CCtx.Program_CLess_cctx1 arg1 ->
                    Just
                        ( Syntax.CCtx.Program_CLess_cctx1 Syntax.CCtx.Cctx_hole
                        , arg1
                        )

                Syntax.CCtx.Block_CLess_cctx1 arg1 ->
                    Just
                        ( Syntax.CCtx.Block_CLess_cctx1 Syntax.CCtx.Cctx_hole
                        , arg1
                        )

                Syntax.CCtx.Blockdecls_CLess_cctx1 arg1 ->
                    Just
                        ( Syntax.CCtx.Blockdecls_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                        , arg1
                        )

                Syntax.CCtx.Blockstmts_CLess_cctx1 arg1 ->
                    Just
                        ( Syntax.CCtx.Blockstmts_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                        , arg1
                        )

                Syntax.CCtx.Vardecl_CLess_cctx1 arg1 arg2 (boundVars3, arg3) ->
                    Just
                        ( Syntax.CCtx.Vardecl_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                            arg2
                            (boundVars3, arg3)
                        , arg1
                        )

                Syntax.CCtx.Vardecl_CLess_cctx2 arg1 arg2 (boundVars3, arg3) ->
                    Just
                        ( Syntax.CCtx.Vardecl_CLess_cctx2
                            arg1
                            Syntax.CCtx.Cctx_hole
                            (boundVars3, arg3)
                        , arg2
                        )

                Syntax.CCtx.Vardecl_CLess_cctx3 arg1 arg2 (boundVars3, arg3) ->
                    Just
                        ( Syntax.CCtx.Vardecl_CLess_cctx3
                            arg1
                            arg2
                            ( boundVars3, Syntax.CCtx.Cctx_hole )
                        , arg3
                        )

                Syntax.CCtx.Fundecl1_CLess_cctx1 arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
                    Just
                        ( Syntax.CCtx.Fundecl1_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                            (boundVars2, arg2)
                            arg3
                            (boundVars4, arg4)
                        , arg1
                        )

                Syntax.CCtx.Fundecl1_CLess_cctx2 arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
                    Just
                        ( Syntax.CCtx.Fundecl1_CLess_cctx2
                            arg1
                            ( boundVars2, Syntax.CCtx.Cctx_hole )
                            arg3
                            (boundVars4, arg4)
                        , arg2
                        )

                Syntax.CCtx.Fundecl1_CLess_cctx3 arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
                    Just
                        ( Syntax.CCtx.Fundecl1_CLess_cctx3
                            arg1
                            (boundVars2, arg2)
                            Syntax.CCtx.Cctx_hole
                            (boundVars4, arg4)
                        , arg3
                        )

                Syntax.CCtx.Fundecl1_CLess_cctx4 arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
                    Just
                        ( Syntax.CCtx.Fundecl1_CLess_cctx4
                            arg1
                            (boundVars2, arg2)
                            arg3
                            ( boundVars4, Syntax.CCtx.Cctx_hole )
                        , arg4
                        )

                Syntax.CCtx.Fundecl2_CLess_cctx1 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
                    Just
                        ( Syntax.CCtx.Fundecl2_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                            (boundVars2, arg2)
                            arg3
                            arg4
                            (boundVars5, arg5)
                        , arg1
                        )

                Syntax.CCtx.Fundecl2_CLess_cctx2 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
                    Just
                        ( Syntax.CCtx.Fundecl2_CLess_cctx2
                            arg1
                            ( boundVars2, Syntax.CCtx.Cctx_hole )
                            arg3
                            arg4
                            (boundVars5, arg5)
                        , arg2
                        )

                Syntax.CCtx.Fundecl2_CLess_cctx3 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
                    Just
                        ( Syntax.CCtx.Fundecl2_CLess_cctx3
                            arg1
                            (boundVars2, arg2)
                            Syntax.CCtx.Cctx_hole
                            arg4
                            (boundVars5, arg5)
                        , arg3
                        )

                Syntax.CCtx.Fundecl2_CLess_cctx4 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
                    Just
                        ( Syntax.CCtx.Fundecl2_CLess_cctx4
                            arg1
                            (boundVars2, arg2)
                            arg3
                            Syntax.CCtx.Cctx_hole
                            (boundVars5, arg5)
                        , arg4
                        )

                Syntax.CCtx.Fundecl2_CLess_cctx5 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
                    Just
                        ( Syntax.CCtx.Fundecl2_CLess_cctx5
                            arg1
                            (boundVars2, arg2)
                            arg3
                            arg4
                            ( boundVars5, Syntax.CCtx.Cctx_hole )
                        , arg5
                        )

                Syntax.CCtx.Assignment_CLess_cctx1 arg1 arg2 ->
                    Just
                        ( Syntax.CCtx.Assignment_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                            arg2
                        , arg1
                        )

                Syntax.CCtx.Assignment_CLess_cctx2 arg1 arg2 ->
                    Just
                        ( Syntax.CCtx.Assignment_CLess_cctx2
                            arg1
                            Syntax.CCtx.Cctx_hole
                        , arg2
                        )

                Syntax.CCtx.Stmtfuncall_CLess_cctx1 arg1 arg2 ->
                    Just
                        ( Syntax.CCtx.Stmtfuncall_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                            arg2
                        , arg1
                        )

                Syntax.CCtx.Stmtfuncall_CLess_cctx2 arg1 arg2 ->
                    Just
                        ( Syntax.CCtx.Stmtfuncall_CLess_cctx2
                            arg1
                            Syntax.CCtx.Cctx_hole
                        , arg2
                        )

                Syntax.CCtx.Return_CLess_cctx1 arg1 ->
                    Just
                        ( Syntax.CCtx.Return_CLess_cctx1 Syntax.CCtx.Cctx_hole
                        , arg1
                        )

                Syntax.CCtx.Conditional_CLess_cctx1 arg1 ->
                    Just
                        ( Syntax.CCtx.Conditional_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                        , arg1
                        )

                Syntax.CCtx.Compstmt_CLess_cctx1 arg1 arg2 ->
                    Just
                        ( Syntax.CCtx.Compstmt_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                            arg2
                        , arg1
                        )

                Syntax.CCtx.Compstmt_CLess_cctx2 arg1 arg2 ->
                    Just
                        ( Syntax.CCtx.Compstmt_CLess_cctx2
                            arg1
                            Syntax.CCtx.Cctx_hole
                        , arg2
                        )

                Syntax.CCtx.Funarg_CLess_cctx1 arg1 arg2 ->
                    Just
                        ( Syntax.CCtx.Funarg_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                            arg2
                        , arg1
                        )

                Syntax.CCtx.Funarg_CLess_cctx2 arg1 arg2 ->
                    Just
                        ( Syntax.CCtx.Funarg_CLess_cctx2
                            arg1
                            Syntax.CCtx.Cctx_hole
                        , arg2
                        )

                Syntax.CCtx.Funargs_CLess_cctx1 arg1 arg2 arg3 ->
                    Just
                        ( Syntax.CCtx.Funargs_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                            arg2
                            arg3
                        , arg1
                        )

                Syntax.CCtx.Funargs_CLess_cctx2 arg1 arg2 arg3 ->
                    Just
                        ( Syntax.CCtx.Funargs_CLess_cctx2
                            arg1
                            Syntax.CCtx.Cctx_hole
                            arg3
                        , arg2
                        )

                Syntax.CCtx.Funargs_CLess_cctx3 arg1 arg2 arg3 ->
                    Just
                        ( Syntax.CCtx.Funargs_CLess_cctx3
                            arg1
                            arg2
                            Syntax.CCtx.Cctx_hole
                        , arg3
                        )

                Syntax.CCtx.Ifelse_CLess_cctx1 arg1 arg2 arg3 ->
                    Just
                        ( Syntax.CCtx.Ifelse_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                            arg2
                            arg3
                        , arg1
                        )

                Syntax.CCtx.Ifelse_CLess_cctx2 arg1 arg2 arg3 ->
                    Just
                        ( Syntax.CCtx.Ifelse_CLess_cctx2
                            arg1
                            Syntax.CCtx.Cctx_hole
                            arg3
                        , arg2
                        )

                Syntax.CCtx.Ifelse_CLess_cctx3 arg1 arg2 arg3 ->
                    Just
                        ( Syntax.CCtx.Ifelse_CLess_cctx3
                            arg1
                            arg2
                            Syntax.CCtx.Cctx_hole
                        , arg3
                        )

                Syntax.CCtx.Plus_CLess_cctx1 arg1 arg2 ->
                    Just
                        ( Syntax.CCtx.Plus_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                            arg2
                        , arg1
                        )

                Syntax.CCtx.Plus_CLess_cctx2 arg1 arg2 ->
                    Just
                        ( Syntax.CCtx.Plus_CLess_cctx2
                            arg1
                            Syntax.CCtx.Cctx_hole
                        , arg2
                        )

                Syntax.CCtx.Equals_CLess_cctx1 arg1 arg2 ->
                    Just
                        ( Syntax.CCtx.Equals_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                            arg2
                        , arg1
                        )

                Syntax.CCtx.Equals_CLess_cctx2 arg1 arg2 ->
                    Just
                        ( Syntax.CCtx.Equals_CLess_cctx2
                            arg1
                            Syntax.CCtx.Cctx_hole
                        , arg2
                        )

                Syntax.CCtx.Expfuncall_CLess_cctx1 arg1 arg2 ->
                    Just
                        ( Syntax.CCtx.Expfuncall_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                            arg2
                        , arg1
                        )

                Syntax.CCtx.Expfuncall_CLess_cctx2 arg1 arg2 ->
                    Just
                        ( Syntax.CCtx.Expfuncall_CLess_cctx2
                            arg1
                            Syntax.CCtx.Cctx_hole
                        , arg2
                        )

                Syntax.CCtx.Expident_CLess_cctx1 arg1 ->
                    Just
                        ( Syntax.CCtx.Expident_CLess_cctx1 Syntax.CCtx.Cctx_hole
                        , arg1
                        )

        [_] ->
            Just ( Syntax.CCtx.Cctx_hole, cctx )

        _ :: rest ->
            case cctx of
                Syntax.CCtx.Cctx_hole ->
                    Nothing

                Syntax.CCtx.Program_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Program_CLess_cctx1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Block_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Block_CLess_cctx1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Blockdecls_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Blockdecls_CLess_cctx1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Blockstmts_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Blockstmts_CLess_cctx1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Vardecl_CLess_cctx1 arg1 arg2 (boundVars3, arg3) ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Vardecl_CLess_cctx1
                                                                     newCctx
                                                                     arg2
                                                                     (boundVars3, arg3)
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Vardecl_CLess_cctx2 arg1 arg2 (boundVars3, arg3) ->
                    moveCCtxHoleUp arg2 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Vardecl_CLess_cctx2
                                                                     arg1
                                                                     newCctx
                                                                     (boundVars3, arg3)
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Vardecl_CLess_cctx3 arg1 arg2 (boundVars3, arg3) ->
                    moveCCtxHoleUp arg3 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Vardecl_CLess_cctx3
                                                                     arg1
                                                                     arg2
                                                                     (boundVars3, newCctx )
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Fundecl1_CLess_cctx1 arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Fundecl1_CLess_cctx1
                                                                     newCctx
                                                                     (boundVars2, arg2)
                                                                     arg3
                                                                     (boundVars4, arg4)
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Fundecl1_CLess_cctx2 arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
                    moveCCtxHoleUp arg2 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Fundecl1_CLess_cctx2
                                                                     arg1
                                                                     (boundVars2, newCctx )
                                                                     arg3
                                                                     (boundVars4, arg4)
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Fundecl1_CLess_cctx3 arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
                    moveCCtxHoleUp arg3 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Fundecl1_CLess_cctx3
                                                                     arg1
                                                                     (boundVars2, arg2)
                                                                     newCctx
                                                                     (boundVars4, arg4)
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Fundecl1_CLess_cctx4 arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
                    moveCCtxHoleUp arg4 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Fundecl1_CLess_cctx4
                                                                     arg1
                                                                     (boundVars2, arg2)
                                                                     arg3
                                                                     (boundVars4, newCctx )
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Fundecl2_CLess_cctx1 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Fundecl2_CLess_cctx1
                                                                     newCctx
                                                                     (boundVars2, arg2)
                                                                     arg3
                                                                     arg4
                                                                     (boundVars5, arg5)
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Fundecl2_CLess_cctx2 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
                    moveCCtxHoleUp arg2 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Fundecl2_CLess_cctx2
                                                                     arg1
                                                                     (boundVars2, newCctx )
                                                                     arg3
                                                                     arg4
                                                                     (boundVars5, arg5)
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Fundecl2_CLess_cctx3 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
                    moveCCtxHoleUp arg3 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Fundecl2_CLess_cctx3
                                                                     arg1
                                                                     (boundVars2, arg2)
                                                                     newCctx
                                                                     arg4
                                                                     (boundVars5, arg5)
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Fundecl2_CLess_cctx4 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
                    moveCCtxHoleUp arg4 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Fundecl2_CLess_cctx4
                                                                     arg1
                                                                     (boundVars2, arg2)
                                                                     arg3
                                                                     newCctx
                                                                     (boundVars5, arg5)
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Fundecl2_CLess_cctx5 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
                    moveCCtxHoleUp arg5 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Fundecl2_CLess_cctx5
                                                                     arg1
                                                                     (boundVars2, arg2)
                                                                     arg3
                                                                     arg4
                                                                     (boundVars5, newCctx )
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Assignment_CLess_cctx1 arg1 arg2 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Assignment_CLess_cctx1
                                                                     newCctx
                                                                     arg2
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Assignment_CLess_cctx2 arg1 arg2 ->
                    moveCCtxHoleUp arg2 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Assignment_CLess_cctx2
                                                                     arg1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Stmtfuncall_CLess_cctx1 arg1 arg2 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Stmtfuncall_CLess_cctx1
                                                                     newCctx
                                                                     arg2
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Stmtfuncall_CLess_cctx2 arg1 arg2 ->
                    moveCCtxHoleUp arg2 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Stmtfuncall_CLess_cctx2
                                                                     arg1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Return_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Return_CLess_cctx1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Conditional_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Conditional_CLess_cctx1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Compstmt_CLess_cctx1 arg1 arg2 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Compstmt_CLess_cctx1
                                                                     newCctx
                                                                     arg2
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Compstmt_CLess_cctx2 arg1 arg2 ->
                    moveCCtxHoleUp arg2 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Compstmt_CLess_cctx2
                                                                     arg1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Funarg_CLess_cctx1 arg1 arg2 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Funarg_CLess_cctx1
                                                                     newCctx
                                                                     arg2
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Funarg_CLess_cctx2 arg1 arg2 ->
                    moveCCtxHoleUp arg2 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Funarg_CLess_cctx2
                                                                     arg1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Funargs_CLess_cctx1 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Funargs_CLess_cctx1
                                                                     newCctx
                                                                     arg2
                                                                     arg3
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Funargs_CLess_cctx2 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg2 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Funargs_CLess_cctx2
                                                                     arg1
                                                                     newCctx
                                                                     arg3
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Funargs_CLess_cctx3 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg3 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Funargs_CLess_cctx3
                                                                     arg1
                                                                     arg2
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Ifelse_CLess_cctx1 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Ifelse_CLess_cctx1
                                                                     newCctx
                                                                     arg2
                                                                     arg3
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Ifelse_CLess_cctx2 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg2 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Ifelse_CLess_cctx2
                                                                     arg1
                                                                     newCctx
                                                                     arg3
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Ifelse_CLess_cctx3 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg3 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Ifelse_CLess_cctx3
                                                                     arg1
                                                                     arg2
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Plus_CLess_cctx1 arg1 arg2 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Plus_CLess_cctx1
                                                                     newCctx
                                                                     arg2
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Plus_CLess_cctx2 arg1 arg2 ->
                    moveCCtxHoleUp arg2 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Plus_CLess_cctx2
                                                                     arg1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Equals_CLess_cctx1 arg1 arg2 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Equals_CLess_cctx1
                                                                     newCctx
                                                                     arg2
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Equals_CLess_cctx2 arg1 arg2 ->
                    moveCCtxHoleUp arg2 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Equals_CLess_cctx2
                                                                     arg1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Expfuncall_CLess_cctx1 arg1 arg2 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Expfuncall_CLess_cctx1
                                                                     newCctx
                                                                     arg2
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Expfuncall_CLess_cctx2 arg1 arg2 ->
                    moveCCtxHoleUp arg2 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Expfuncall_CLess_cctx2
                                                                     arg1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Expident_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Expident_CLess_cctx1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

        [] ->
            Nothing


addParent :
    Syntax.CCtx.Cctx
    -> Syntax.Wellformed.Wellformed
    -> Maybe Syntax.Wellformed.Wellformed
addParent cctx wellformed =
    case cctx of
        Syntax.CCtx.Cctx_hole ->
            Nothing

        Syntax.CCtx.Program_CLess_cctx1 arg1 ->
            case wellformed of
                Syntax.Wellformed.Root_fd_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_p_CLess
                             (Syntax.Cursorless.Program_CLess underCursor)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Block_CLess_cctx1 arg1 ->
            case wellformed of
                Syntax.Wellformed.Root_bi_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_b_CLess
                             (Syntax.Cursorless.Block_CLess underCursor)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Blockdecls_CLess_cctx1 arg1 ->
            case wellformed of
                Syntax.Wellformed.Root_vd_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_bi_CLess
                             (Syntax.Cursorless.Blockdecls_CLess underCursor)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Blockstmts_CLess_cctx1 arg1 ->
            case wellformed of
                Syntax.Wellformed.Root_s_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_bi_CLess
                             (Syntax.Cursorless.Blockstmts_CLess underCursor)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Vardecl_CLess_cctx1 arg1 arg2 (boundVars3, arg3) ->
            case wellformed of
                Syntax.Wellformed.Root_t_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_vd_CLess
                             (Syntax.Cursorless.Vardecl_CLess
                                  underCursor
                                  arg2
                                  (boundVars3, arg3)
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Vardecl_CLess_cctx2 arg1 arg2 (boundVars3, arg3) ->
            case wellformed of
                Syntax.Wellformed.Root_e_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_vd_CLess
                             (Syntax.Cursorless.Vardecl_CLess
                                  arg1
                                  underCursor
                                  (boundVars3, arg3)
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Vardecl_CLess_cctx3 arg1 arg2 (boundVars3, arg3) ->
            case wellformed of
                Syntax.Wellformed.Root_bi_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_vd_CLess
                             (Syntax.Cursorless.Vardecl_CLess
                                  arg1
                                  arg2
                                  (boundVars3, underCursor )
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Fundecl1_CLess_cctx1 arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
            case wellformed of
                Syntax.Wellformed.Root_t_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_fd_CLess
                             (Syntax.Cursorless.Fundecl1_CLess
                                  underCursor
                                  (boundVars2, arg2)
                                  arg3
                                  (boundVars4, arg4)
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Fundecl1_CLess_cctx2 arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
            case wellformed of
                Syntax.Wellformed.Root_fd_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_fd_CLess
                             (Syntax.Cursorless.Fundecl1_CLess
                                  arg1
                                  (boundVars2, underCursor )
                                  arg3
                                  (boundVars4, arg4)
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Fundecl1_CLess_cctx3 arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
            case wellformed of
                Syntax.Wellformed.Root_t_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_fd_CLess
                             (Syntax.Cursorless.Fundecl1_CLess
                                  arg1
                                  (boundVars2, arg2)
                                  underCursor
                                  (boundVars4, arg4)
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Fundecl1_CLess_cctx4 arg1 (boundVars2, arg2) arg3 (boundVars4, arg4) ->
            case wellformed of
                Syntax.Wellformed.Root_b_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_fd_CLess
                             (Syntax.Cursorless.Fundecl1_CLess
                                  arg1
                                  (boundVars2, arg2)
                                  arg3
                                  (boundVars4, underCursor )
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Fundecl2_CLess_cctx1 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
            case wellformed of
                Syntax.Wellformed.Root_t_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_fd_CLess
                             (Syntax.Cursorless.Fundecl2_CLess
                                  underCursor
                                  (boundVars2, arg2)
                                  arg3
                                  arg4
                                  (boundVars5, arg5)
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Fundecl2_CLess_cctx2 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
            case wellformed of
                Syntax.Wellformed.Root_fd_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_fd_CLess
                             (Syntax.Cursorless.Fundecl2_CLess
                                  arg1
                                  (boundVars2, underCursor )
                                  arg3
                                  arg4
                                  (boundVars5, arg5)
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Fundecl2_CLess_cctx3 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
            case wellformed of
                Syntax.Wellformed.Root_t_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_fd_CLess
                             (Syntax.Cursorless.Fundecl2_CLess
                                  arg1
                                  (boundVars2, arg2)
                                  underCursor
                                  arg4
                                  (boundVars5, arg5)
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Fundecl2_CLess_cctx4 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
            case wellformed of
                Syntax.Wellformed.Root_t_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_fd_CLess
                             (Syntax.Cursorless.Fundecl2_CLess
                                  arg1
                                  (boundVars2, arg2)
                                  arg3
                                  underCursor
                                  (boundVars5, arg5)
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Fundecl2_CLess_cctx5 arg1 (boundVars2, arg2) arg3 arg4 (boundVars5, arg5) ->
            case wellformed of
                Syntax.Wellformed.Root_b_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_fd_CLess
                             (Syntax.Cursorless.Fundecl2_CLess
                                  arg1
                                  (boundVars2, arg2)
                                  arg3
                                  arg4
                                  (boundVars5, underCursor )
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Assignment_CLess_cctx1 arg1 arg2 ->
            case wellformed of
                Syntax.Wellformed.Root_id_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_s_CLess
                             (Syntax.Cursorless.Assignment_CLess
                                  underCursor
                                  arg2
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Assignment_CLess_cctx2 arg1 arg2 ->
            case wellformed of
                Syntax.Wellformed.Root_e_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_s_CLess
                             (Syntax.Cursorless.Assignment_CLess
                                  arg1
                                  underCursor
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Stmtfuncall_CLess_cctx1 arg1 arg2 ->
            case wellformed of
                Syntax.Wellformed.Root_id_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_s_CLess
                             (Syntax.Cursorless.Stmtfuncall_CLess
                                  underCursor
                                  arg2
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Stmtfuncall_CLess_cctx2 arg1 arg2 ->
            case wellformed of
                Syntax.Wellformed.Root_fa_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_s_CLess
                             (Syntax.Cursorless.Stmtfuncall_CLess
                                  arg1
                                  underCursor
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Return_CLess_cctx1 arg1 ->
            case wellformed of
                Syntax.Wellformed.Root_e_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_s_CLess
                             (Syntax.Cursorless.Return_CLess underCursor)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Conditional_CLess_cctx1 arg1 ->
            case wellformed of
                Syntax.Wellformed.Root_cond_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_s_CLess
                             (Syntax.Cursorless.Conditional_CLess underCursor)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Compstmt_CLess_cctx1 arg1 arg2 ->
            case wellformed of
                Syntax.Wellformed.Root_s_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_s_CLess
                             (Syntax.Cursorless.Compstmt_CLess underCursor arg2)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Compstmt_CLess_cctx2 arg1 arg2 ->
            case wellformed of
                Syntax.Wellformed.Root_s_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_s_CLess
                             (Syntax.Cursorless.Compstmt_CLess arg1 underCursor)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Funarg_CLess_cctx1 arg1 arg2 ->
            case wellformed of
                Syntax.Wellformed.Root_t_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_fa_CLess
                             (Syntax.Cursorless.Funarg_CLess underCursor arg2)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Funarg_CLess_cctx2 arg1 arg2 ->
            case wellformed of
                Syntax.Wellformed.Root_id_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_fa_CLess
                             (Syntax.Cursorless.Funarg_CLess arg1 underCursor)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Funargs_CLess_cctx1 arg1 arg2 arg3 ->
            case wellformed of
                Syntax.Wellformed.Root_t_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_fa_CLess
                             (Syntax.Cursorless.Funargs_CLess
                                  underCursor
                                  arg2
                                  arg3
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Funargs_CLess_cctx2 arg1 arg2 arg3 ->
            case wellformed of
                Syntax.Wellformed.Root_id_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_fa_CLess
                             (Syntax.Cursorless.Funargs_CLess
                                  arg1
                                  underCursor
                                  arg3
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Funargs_CLess_cctx3 arg1 arg2 arg3 ->
            case wellformed of
                Syntax.Wellformed.Root_fa_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_fa_CLess
                             (Syntax.Cursorless.Funargs_CLess
                                  arg1
                                  arg2
                                  underCursor
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Ifelse_CLess_cctx1 arg1 arg2 arg3 ->
            case wellformed of
                Syntax.Wellformed.Root_e_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_cond_CLess
                             (Syntax.Cursorless.Ifelse_CLess
                                  underCursor
                                  arg2
                                  arg3
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Ifelse_CLess_cctx2 arg1 arg2 arg3 ->
            case wellformed of
                Syntax.Wellformed.Root_b_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_cond_CLess
                             (Syntax.Cursorless.Ifelse_CLess
                                  arg1
                                  underCursor
                                  arg3
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Ifelse_CLess_cctx3 arg1 arg2 arg3 ->
            case wellformed of
                Syntax.Wellformed.Root_b_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_cond_CLess
                             (Syntax.Cursorless.Ifelse_CLess
                                  arg1
                                  arg2
                                  underCursor
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Plus_CLess_cctx1 arg1 arg2 ->
            case wellformed of
                Syntax.Wellformed.Root_e_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_e_CLess
                             (Syntax.Cursorless.Plus_CLess underCursor arg2)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Plus_CLess_cctx2 arg1 arg2 ->
            case wellformed of
                Syntax.Wellformed.Root_e_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_e_CLess
                             (Syntax.Cursorless.Plus_CLess arg1 underCursor)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Equals_CLess_cctx1 arg1 arg2 ->
            case wellformed of
                Syntax.Wellformed.Root_e_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_e_CLess
                             (Syntax.Cursorless.Equals_CLess underCursor arg2)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Equals_CLess_cctx2 arg1 arg2 ->
            case wellformed of
                Syntax.Wellformed.Root_e_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_e_CLess
                             (Syntax.Cursorless.Equals_CLess arg1 underCursor)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Expfuncall_CLess_cctx1 arg1 arg2 ->
            case wellformed of
                Syntax.Wellformed.Root_id_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_e_CLess
                             (Syntax.Cursorless.Expfuncall_CLess
                                  underCursor
                                  arg2
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Expfuncall_CLess_cctx2 arg1 arg2 ->
            case wellformed of
                Syntax.Wellformed.Root_fa_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_e_CLess
                             (Syntax.Cursorless.Expfuncall_CLess
                                  arg1
                                  underCursor
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Expident_CLess_cctx1 arg1 ->
            case wellformed of
                Syntax.Wellformed.Root_id_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_e_CLess
                             (Syntax.Cursorless.Expident_CLess underCursor)
                        )

                _ ->
                    Nothing


parent :
    ( Syntax.CCtx.Cctx, Syntax.Wellformed.Wellformed )
    -> Maybe ( Syntax.CCtx.Cctx, Syntax.Wellformed.Wellformed )
parent decomposed =
    let
        ( cctx, wellformed ) =
            decomposed
    in
    case moveCCtxHoleUp cctx (getCctxPath cctx []) of
        Nothing ->
            Nothing

        Just ( newCctx, removedCctx ) ->
            case addParent removedCctx wellformed of
                Nothing ->
                    Nothing

                Just newWellformed ->
                    Just ( newCctx, newWellformed )