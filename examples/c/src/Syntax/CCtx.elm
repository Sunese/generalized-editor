module Syntax.CCtx exposing (..)

import Syntax.Bind
import Syntax.Cursorless


type Cctx
    = Cctx_hole
    | Program_CLess_cctx1 Cctx
    | Block_CLess_cctx1 Cctx
    | Blockdecls_CLess_cctx1 Cctx
    | Blockstmts_CLess_cctx1 Cctx
    | Vardecl_CLess_cctx1
        Cctx
        Syntax.Cursorless.E_CLess
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Syntax.Cursorless.Bi_CLess)
    | Vardecl_CLess_cctx2
        Syntax.Cursorless.T_CLess
        Cctx
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Syntax.Cursorless.Bi_CLess)
    | Vardecl_CLess_cctx3
        Syntax.Cursorless.T_CLess
        Syntax.Cursorless.E_CLess
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Cctx)
    | Fundecl1_CLess_cctx1
        Cctx
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Syntax.Cursorless.Fd_CLess)
        Syntax.Cursorless.T_CLess
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Syntax.Cursorless.B_CLess)
    | Fundecl1_CLess_cctx2
        Syntax.Cursorless.T_CLess
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Cctx)
        Syntax.Cursorless.T_CLess
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Syntax.Cursorless.B_CLess)
    | Fundecl1_CLess_cctx3
        Syntax.Cursorless.T_CLess
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Syntax.Cursorless.Fd_CLess)
        Cctx
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Syntax.Cursorless.B_CLess)
    | Fundecl1_CLess_cctx4
        Syntax.Cursorless.T_CLess
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Syntax.Cursorless.Fd_CLess)
        Syntax.Cursorless.T_CLess
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Cctx)
    | Fundecl2_CLess_cctx1
        Cctx
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Syntax.Cursorless.Fd_CLess)
        Syntax.Cursorless.T_CLess
        Syntax.Cursorless.T_CLess
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Syntax.Cursorless.B_CLess)
    | Fundecl2_CLess_cctx2
        Syntax.Cursorless.T_CLess
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Cctx)
        Syntax.Cursorless.T_CLess
        Syntax.Cursorless.T_CLess
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Syntax.Cursorless.B_CLess)
    | Fundecl2_CLess_cctx3
        Syntax.Cursorless.T_CLess
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Syntax.Cursorless.Fd_CLess)
        Cctx
        Syntax.Cursorless.T_CLess
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Syntax.Cursorless.B_CLess)
    | Fundecl2_CLess_cctx4
        Syntax.Cursorless.T_CLess
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Syntax.Cursorless.Fd_CLess)
        Syntax.Cursorless.T_CLess
        Cctx
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Syntax.Cursorless.B_CLess)
    | Fundecl2_CLess_cctx5
        Syntax.Cursorless.T_CLess
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Syntax.Cursorless.Fd_CLess)
        Syntax.Cursorless.T_CLess
        Syntax.Cursorless.T_CLess
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Cctx)
    | Assignment_CLess_cctx1 Cctx Syntax.Cursorless.E_CLess
    | Assignment_CLess_cctx2 Syntax.Cursorless.Id_CLess Cctx
    | Stmtfuncall_CLess_cctx1 Cctx Syntax.Cursorless.Fa_CLess
    | Stmtfuncall_CLess_cctx2 Syntax.Cursorless.Id_CLess Cctx
    | Return_CLess_cctx1 Cctx
    | Conditional_CLess_cctx1 Cctx
    | Compstmt_CLess_cctx1 Cctx Syntax.Cursorless.S_CLess
    | Compstmt_CLess_cctx2 Syntax.Cursorless.S_CLess Cctx
    | Funarg_CLess_cctx1 Cctx Syntax.Cursorless.Id_CLess
    | Funarg_CLess_cctx2 Syntax.Cursorless.T_CLess Cctx
    | Funargs_CLess_cctx1
        Cctx
        Syntax.Cursorless.Id_CLess
        Syntax.Cursorless.Fa_CLess
    | Funargs_CLess_cctx2
        Syntax.Cursorless.T_CLess
        Cctx
        Syntax.Cursorless.Fa_CLess
    | Funargs_CLess_cctx3
        Syntax.Cursorless.T_CLess
        Syntax.Cursorless.Id_CLess
        Cctx
    | Ifelse_CLess_cctx1
        Cctx
        Syntax.Cursorless.B_CLess
        Syntax.Cursorless.B_CLess
    | Ifelse_CLess_cctx2
        Syntax.Cursorless.E_CLess
        Cctx
        Syntax.Cursorless.B_CLess
    | Ifelse_CLess_cctx3
        Syntax.Cursorless.E_CLess
        Syntax.Cursorless.B_CLess
        Cctx
    | Plus_CLess_cctx1 Cctx Syntax.Cursorless.E_CLess
    | Plus_CLess_cctx2 Syntax.Cursorless.E_CLess Cctx
    | Equals_CLess_cctx1 Cctx Syntax.Cursorless.E_CLess
    | Equals_CLess_cctx2 Syntax.Cursorless.E_CLess Cctx
    | Expfuncall_CLess_cctx1 Cctx Syntax.Cursorless.Fa_CLess
    | Expfuncall_CLess_cctx2 Syntax.Cursorless.Id_CLess Cctx
    | Expident_CLess_cctx1 Cctx