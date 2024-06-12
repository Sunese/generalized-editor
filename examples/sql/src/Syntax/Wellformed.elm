module Syntax.Wellformed exposing (..)

import Syntax.Cursorless


type Wellformed
    = Root_q_CLess Syntax.Cursorless.Q_CLess
    | Root_cmd_CLess Syntax.Cursorless.Cmd_CLess
    | Root_id_CLess Syntax.Cursorless.Id_CLess
    | Root_const_CLess Syntax.Cursorless.Const_CLess
    | Root_clause_CLess Syntax.Cursorless.Clause_CLess
    | Root_cond_CLess Syntax.Cursorless.Cond_CLess
    | Root_exp_CLess Syntax.Cursorless.Exp_CLess