module Syntax.Cursorless exposing (..)

import Syntax.Bind


type Q_CLess
    = Select_CLess Id_CLess Id_CLess Clause_CLess
    | Hole_q_CLess


type Cmd_CLess
    = Insert_CLess Id_CLess (Syntax.Bind.Bind Id_CLess Q_CLess)
    | Hole_cmd_CLess


type Id_CLess
    = Ident_CLess String
    | Hole_id_CLess


type Const_CLess
    = Num_CLess Int
    | Str_CLess String
    | Hole_const_CLess


type Clause_CLess
    = Where_CLess Cond_CLess
    | Having_CLess Cond_CLess
    | Hole_clause_CLess


type Cond_CLess
    = Greater_CLess Exp_CLess Exp_CLess
    | Equals_CLess Exp_CLess Exp_CLess
    | Hole_cond_CLess


type Exp_CLess
    = Econst_CLess Const_CLess
    | Eident_CLess Id_CLess
    | Hole_exp_CLess


type CursorLess
    = Q_CLess Q_CLess
    | Cmd_CLess Cmd_CLess
    | Id_CLess Id_CLess
    | Const_CLess Const_CLess
    | Clause_CLess Clause_CLess
    | Cond_CLess Cond_CLess
    | Exp_CLess Exp_CLess