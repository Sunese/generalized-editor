module Syntax.Base exposing (..)

import Syntax.Bind


type P
    = Program Fd
    | Hole_p
    | Cursor_p P


type S
    = Assignment Id E
    | Stmtfuncall Id Fa
    | Return E
    | Conditional Cond
    | Compstmt S S
    | Hole_s
    | Cursor_s S


type Vd
    = Vardecl T E (Syntax.Bind.Bind Id Bi)
    | Hole_vd
    | Cursor_vd Vd


type Fd
    = Fundecl1 T (Syntax.Bind.Bind Id Fd) T (Syntax.Bind.Bind Id B)
    | Fundecl2 T (Syntax.Bind.Bind Id Fd) T T (Syntax.Bind.Bind Id B)
    | Fundecldone
    | Hole_fd
    | Cursor_fd Fd


type T
    = Tint
    | Tchar
    | Tbool
    | Hole_t
    | Cursor_t T


type Id
    = Ident String
    | Hole_id
    | Cursor_id Id


type E
    = Int Int
    | Char Char
    | Bool Bool
    | Plus E E
    | Equals E E
    | Expfuncall Id Fa
    | Expident Id
    | Hole_e
    | Cursor_e E


type B
    = Block Bi
    | Hole_b
    | Cursor_b B


type Bi
    = Blockdecls Vd
    | Blockstmts S
    | Blockdone
    | Hole_bi
    | Cursor_bi Bi


type Fa
    = Funarg T Id
    | Funargs T Id Fa
    | Hole_fa
    | Cursor_fa Fa


type Cond
    = Ifelse E B B
    | Hole_cond
    | Cursor_cond Cond


type Base
    = P P
    | S S
    | Vd Vd
    | Fd Fd
    | T T
    | Id Id
    | E E
    | B B
    | Bi Bi
    | Fa Fa
    | Cond Cond