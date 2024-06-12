module Syntax.Wellformed exposing (..)

import Syntax.Cursorless


type Wellformed
    = Root_d_CLess Syntax.Cursorless.D_CLess
    | Root_e_CLess Syntax.Cursorless.E_CLess
    | Root_cmd_CLess Syntax.Cursorless.Cmd_CLess
    | Root_a_CLess Syntax.Cursorless.A_CLess
    | Root_id_CLess Syntax.Cursorless.Id_CLess
    | Root_c_CLess Syntax.Cursorless.C_CLess