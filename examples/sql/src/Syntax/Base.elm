module Syntax.Base exposing (..)

import Syntax.Bind


type Q
    = Select Id Id Clause
    | Hole_q
    | Cursor_q Q


type Cmd
    = Insert Id (Syntax.Bind.Bind Id Q)
    | Hole_cmd
    | Cursor_cmd Cmd


type Id
    = Ident String
    | Hole_id
    | Cursor_id Id


type Const
    = Num Int
    | Str String
    | Hole_const
    | Cursor_const Const


type Clause
    = Where Cond
    | Having Cond
    | Hole_clause
    | Cursor_clause Clause


type Cond
    = Greater Exp Exp
    | Equals Exp Exp
    | Hole_cond
    | Cursor_cond Cond


type Exp
    = Econst Const
    | Eident Id
    | Hole_exp
    | Cursor_exp Exp


type Base
    = Q Q
    | Cmd Cmd
    | Id Id
    | Const Const
    | Clause Clause
    | Cond Cond
    | Exp Exp