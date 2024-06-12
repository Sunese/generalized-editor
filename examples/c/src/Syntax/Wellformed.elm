module Syntax.Wellformed exposing (..)

import Syntax.Cursorless


type Wellformed
    = Root_p_CLess Syntax.Cursorless.P_CLess
    | Root_s_CLess Syntax.Cursorless.S_CLess
    | Root_vd_CLess Syntax.Cursorless.Vd_CLess
    | Root_fd_CLess Syntax.Cursorless.Fd_CLess
    | Root_t_CLess Syntax.Cursorless.T_CLess
    | Root_id_CLess Syntax.Cursorless.Id_CLess
    | Root_e_CLess Syntax.Cursorless.E_CLess
    | Root_b_CLess Syntax.Cursorless.B_CLess
    | Root_bi_CLess Syntax.Cursorless.Bi_CLess
    | Root_fa_CLess Syntax.Cursorless.Fa_CLess
    | Root_cond_CLess Syntax.Cursorless.Cond_CLess