module Syntax.CCtx exposing (..)

import Syntax.Bind
import Syntax.Cursorless


type Cctx
    = Cctx_hole
    | Latexdoc_CLess_cctx1
        Cctx
        Syntax.Cursorless.E_CLess
        Syntax.Cursorless.A_CLess
        Syntax.Cursorless.A_CLess
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Syntax.Cursorless.C_CLess)
    | Latexdoc_CLess_cctx2
        Syntax.Cursorless.Id_CLess
        Cctx
        Syntax.Cursorless.A_CLess
        Syntax.Cursorless.A_CLess
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Syntax.Cursorless.C_CLess)
    | Latexdoc_CLess_cctx3
        Syntax.Cursorless.Id_CLess
        Syntax.Cursorless.E_CLess
        Cctx
        Syntax.Cursorless.A_CLess
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Syntax.Cursorless.C_CLess)
    | Latexdoc_CLess_cctx4
        Syntax.Cursorless.Id_CLess
        Syntax.Cursorless.E_CLess
        Syntax.Cursorless.A_CLess
        Cctx
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Syntax.Cursorless.C_CLess)
    | Latexdoc_CLess_cctx5
        Syntax.Cursorless.Id_CLess
        Syntax.Cursorless.E_CLess
        Syntax.Cursorless.A_CLess
        Syntax.Cursorless.A_CLess
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Cctx)
    | Environment_CLess_cctx1
        Cctx
        Syntax.Cursorless.C_CLess
        Syntax.Cursorless.Id_CLess
    | Environment_CLess_cctx2
        Syntax.Cursorless.Id_CLess
        Cctx
        Syntax.Cursorless.Id_CLess
    | Environment_CLess_cctx3
        Syntax.Cursorless.Id_CLess
        Syntax.Cursorless.C_CLess
        Cctx
    | Command_CLess_cctx1 Cctx Syntax.Cursorless.A_CLess
    | Command_CLess_cctx2 Syntax.Cursorless.Id_CLess Cctx
    | Argument_CLess_cctx1 Cctx
    | CmdContent_CLess_cctx1 Cctx
    | EnvContent_CLess_cctx1 Cctx
    | SeqContent_CLess_cctx1 Cctx Syntax.Cursorless.C_CLess
    | SeqContent_CLess_cctx2 Syntax.Cursorless.C_CLess Cctx