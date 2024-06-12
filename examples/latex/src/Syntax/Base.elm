module Syntax.Base exposing (..)

import Syntax.Bind


type D
    = Latexdoc Id E A A (Syntax.Bind.Bind Id C)
    | Hole_d
    | Cursor_d D


type E
    = Environment Id C Id
    | Hole_e
    | Cursor_e E


type Cmd
    = Command Id A
    | Hole_cmd
    | Cursor_cmd Cmd


type A
    = Argument C
    | Hole_a
    | Cursor_a A


type Id
    = Ident String
    | Hole_id
    | Cursor_id Id


type C
    = TextContent String
    | CmdContent Cmd
    | EnvContent E
    | SeqContent C C
    | Hole_c
    | Cursor_c C


type Base
    = D D
    | E E
    | Cmd Cmd
    | A A
    | Id Id
    | C C