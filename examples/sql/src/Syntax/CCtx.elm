module Syntax.CCtx exposing (..)

import Syntax.Bind
import Syntax.Cursorless


type Cctx
    = Cctx_hole
    | Select_CLess_cctx1
        Cctx
        Syntax.Cursorless.Id_CLess
        Syntax.Cursorless.Clause_CLess
    | Select_CLess_cctx2
        Syntax.Cursorless.Id_CLess
        Cctx
        Syntax.Cursorless.Clause_CLess
    | Select_CLess_cctx3
        Syntax.Cursorless.Id_CLess
        Syntax.Cursorless.Id_CLess
        Cctx
    | Insert_CLess_cctx1
        Cctx
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Syntax.Cursorless.Q_CLess)
    | Insert_CLess_cctx2
        Syntax.Cursorless.Id_CLess
        (Syntax.Bind.Bind Syntax.Cursorless.Id_CLess Cctx)
    | Where_CLess_cctx1 Cctx
    | Having_CLess_cctx1 Cctx
    | Greater_CLess_cctx1 Cctx Syntax.Cursorless.Exp_CLess
    | Greater_CLess_cctx2 Syntax.Cursorless.Exp_CLess Cctx
    | Equals_CLess_cctx1 Cctx Syntax.Cursorless.Exp_CLess
    | Equals_CLess_cctx2 Syntax.Cursorless.Exp_CLess Cctx
    | Econst_CLess_cctx1 Cctx
    | Eident_CLess_cctx1 Cctx