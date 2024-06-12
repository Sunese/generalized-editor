module Syntax.Cursorless exposing (..)

import Syntax.Bind


type D_CLess
    = Latexdoc_CLess
        Id_CLess
        E_CLess
        A_CLess
        A_CLess
        (Syntax.Bind.Bind Id_CLess C_CLess)
    | Hole_d_CLess


type E_CLess
    = Environment_CLess Id_CLess C_CLess Id_CLess
    | Hole_e_CLess


type Cmd_CLess
    = Command_CLess Id_CLess A_CLess
    | Hole_cmd_CLess


type A_CLess
    = Argument_CLess C_CLess
    | Hole_a_CLess


type Id_CLess
    = Ident_CLess String
    | Hole_id_CLess


type C_CLess
    = TextContent_CLess String
    | CmdContent_CLess Cmd_CLess
    | EnvContent_CLess E_CLess
    | SeqContent_CLess C_CLess C_CLess
    | Hole_c_CLess


type CursorLess
    = D_CLess D_CLess
    | E_CLess E_CLess
    | Cmd_CLess Cmd_CLess
    | A_CLess A_CLess
    | Id_CLess Id_CLess
    | C_CLess C_CLess