module Syntax.Cursorless exposing (..)

import Syntax.Bind


type P_CLess
    = Program_CLess Fd_CLess
    | Hole_p_CLess


type S_CLess
    = Assignment_CLess Id_CLess E_CLess
    | Stmtfuncall_CLess Id_CLess Fa_CLess
    | Return_CLess E_CLess
    | Conditional_CLess Cond_CLess
    | Compstmt_CLess S_CLess S_CLess
    | Hole_s_CLess


type Vd_CLess
    = Vardecl_CLess T_CLess E_CLess (Syntax.Bind.Bind Id_CLess Bi_CLess)
    | Hole_vd_CLess


type Fd_CLess
    = Fundecl1_CLess
        T_CLess
        (Syntax.Bind.Bind Id_CLess Fd_CLess)
        T_CLess
        (Syntax.Bind.Bind Id_CLess B_CLess)
    | Fundecl2_CLess
        T_CLess
        (Syntax.Bind.Bind Id_CLess Fd_CLess)
        T_CLess
        T_CLess
        (Syntax.Bind.Bind Id_CLess B_CLess)
    | Fundecldone_CLess
    | Hole_fd_CLess


type T_CLess
    = Tint_CLess
    | Tchar_CLess
    | Tbool_CLess
    | Hole_t_CLess


type Id_CLess
    = Ident_CLess String
    | Hole_id_CLess


type E_CLess
    = Int_CLess Int
    | Char_CLess Char
    | Bool_CLess Bool
    | Plus_CLess E_CLess E_CLess
    | Equals_CLess E_CLess E_CLess
    | Expfuncall_CLess Id_CLess Fa_CLess
    | Expident_CLess Id_CLess
    | Hole_e_CLess


type B_CLess
    = Block_CLess Bi_CLess
    | Hole_b_CLess


type Bi_CLess
    = Blockdecls_CLess Vd_CLess
    | Blockstmts_CLess S_CLess
    | Blockdone_CLess
    | Hole_bi_CLess


type Fa_CLess
    = Funarg_CLess T_CLess Id_CLess
    | Funargs_CLess T_CLess Id_CLess Fa_CLess
    | Hole_fa_CLess


type Cond_CLess
    = Ifelse_CLess E_CLess B_CLess B_CLess
    | Hole_cond_CLess


type CursorLess
    = P_CLess P_CLess
    | S_CLess S_CLess
    | Vd_CLess Vd_CLess
    | Fd_CLess Fd_CLess
    | T_CLess T_CLess
    | Id_CLess Id_CLess
    | E_CLess E_CLess
    | B_CLess B_CLess
    | Bi_CLess Bi_CLess
    | Fa_CLess Fa_CLess
    | Cond_CLess Cond_CLess