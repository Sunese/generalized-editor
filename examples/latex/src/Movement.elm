module Movement exposing (..)

import Syntax.Base
import Syntax.CCtx
import Syntax.Cursorless
import Syntax.Wellformed


toCLess_d : Syntax.Base.D -> Syntax.Cursorless.D_CLess
toCLess_d d =
    case d of
        Syntax.Base.Latexdoc arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
            Syntax.Cursorless.Latexdoc_CLess
                (toCLess_id arg1)
                (toCLess_e arg2)
                (toCLess_a arg3)
                (toCLess_a arg4)
                ( List.map toCLess_id boundVars5, toCLess_c arg5 )

        Syntax.Base.Hole_d ->
            Syntax.Cursorless.Hole_d_CLess

        Syntax.Base.Cursor_d cursor ->
            Debug.todo "Not wellformed"


toCLess_id : Syntax.Base.Id -> Syntax.Cursorless.Id_CLess
toCLess_id id =
    case id of
        Syntax.Base.Ident lit ->
            Syntax.Cursorless.Ident_CLess lit

        Syntax.Base.Hole_id ->
            Syntax.Cursorless.Hole_id_CLess

        Syntax.Base.Cursor_id cursor ->
            Debug.todo "Not wellformed"


toCLess_e : Syntax.Base.E -> Syntax.Cursorless.E_CLess
toCLess_e e =
    case e of
        Syntax.Base.Environment arg1 arg2 arg3 ->
            Syntax.Cursorless.Environment_CLess
                (toCLess_id arg1)
                (toCLess_c arg2)
                (toCLess_id arg3)

        Syntax.Base.Hole_e ->
            Syntax.Cursorless.Hole_e_CLess

        Syntax.Base.Cursor_e cursor ->
            Debug.todo "Not wellformed"


toCLess_cmd : Syntax.Base.Cmd -> Syntax.Cursorless.Cmd_CLess
toCLess_cmd cmd =
    case cmd of
        Syntax.Base.Command arg1 arg2 ->
            Syntax.Cursorless.Command_CLess (toCLess_id arg1) (toCLess_a arg2)

        Syntax.Base.Hole_cmd ->
            Syntax.Cursorless.Hole_cmd_CLess

        Syntax.Base.Cursor_cmd cursor ->
            Debug.todo "Not wellformed"


toCLess_a : Syntax.Base.A -> Syntax.Cursorless.A_CLess
toCLess_a a =
    case a of
        Syntax.Base.Argument arg1 ->
            Syntax.Cursorless.Argument_CLess (toCLess_c arg1)

        Syntax.Base.Hole_a ->
            Syntax.Cursorless.Hole_a_CLess

        Syntax.Base.Cursor_a cursor ->
            Debug.todo "Not wellformed"


toCLess_c : Syntax.Base.C -> Syntax.Cursorless.C_CLess
toCLess_c c =
    case c of
        Syntax.Base.TextContent lit ->
            Syntax.Cursorless.TextContent_CLess lit

        Syntax.Base.CmdContent arg1 ->
            Syntax.Cursorless.CmdContent_CLess (toCLess_cmd arg1)

        Syntax.Base.EnvContent arg1 ->
            Syntax.Cursorless.EnvContent_CLess (toCLess_e arg1)

        Syntax.Base.SeqContent arg1 arg2 ->
            Syntax.Cursorless.SeqContent_CLess (toCLess_c arg1) (toCLess_c arg2)

        Syntax.Base.Hole_c ->
            Syntax.Cursorless.Hole_c_CLess

        Syntax.Base.Cursor_c cursor ->
            Debug.todo "Not wellformed"


toCLess : Syntax.Base.Base -> Syntax.Cursorless.CursorLess
toCLess base =
    case base of
        Syntax.Base.D arg1 ->
            Syntax.Cursorless.D_CLess (toCLess_d arg1)

        Syntax.Base.Id arg1 ->
            Syntax.Cursorless.Id_CLess (toCLess_id arg1)

        Syntax.Base.E arg1 ->
            Syntax.Cursorless.E_CLess (toCLess_e arg1)

        Syntax.Base.Cmd arg1 ->
            Syntax.Cursorless.Cmd_CLess (toCLess_cmd arg1)

        Syntax.Base.A arg1 ->
            Syntax.Cursorless.A_CLess (toCLess_a arg1)

        Syntax.Base.C arg1 ->
            Syntax.Cursorless.C_CLess (toCLess_c arg1)


toCCtx_d : Syntax.Base.D -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx_d d path =
    case path of
        [] ->
            ( Syntax.CCtx.Cctx_hole, Syntax.Base.D d )

        i :: rest ->
            case d of
                Syntax.Base.Latexdoc arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg1 rest
                            in
                            ( Syntax.CCtx.Latexdoc_CLess_cctx1
                                cctxChild
                                (toCLess_e arg2)
                                (toCLess_a arg3)
                                (toCLess_a arg4)
                                ( List.map toCLess_id boundVars5
                                , toCLess_c arg5
                                )
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_e arg2 rest
                            in
                            ( Syntax.CCtx.Latexdoc_CLess_cctx2
                                (toCLess_id arg1)
                                cctxChild
                                (toCLess_a arg3)
                                (toCLess_a arg4)
                                ( List.map toCLess_id boundVars5
                                , toCLess_c arg5
                                )
                            , restTree
                            )

                        3 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_a arg3 rest
                            in
                            ( Syntax.CCtx.Latexdoc_CLess_cctx3
                                (toCLess_id arg1)
                                (toCLess_e arg2)
                                cctxChild
                                (toCLess_a arg4)
                                ( List.map toCLess_id boundVars5
                                , toCLess_c arg5
                                )
                            , restTree
                            )

                        4 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_a arg4 rest
                            in
                            ( Syntax.CCtx.Latexdoc_CLess_cctx4
                                (toCLess_id arg1)
                                (toCLess_e arg2)
                                (toCLess_a arg3)
                                cctxChild
                                ( List.map toCLess_id boundVars5
                                , toCLess_c arg5
                                )
                            , restTree
                            )

                        5 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_c arg5 rest
                            in
                            ( Syntax.CCtx.Latexdoc_CLess_cctx5
                                (toCLess_id arg1)
                                (toCLess_e arg2)
                                (toCLess_a arg3)
                                (toCLess_a arg4)
                                ( List.map toCLess_id boundVars5, cctxChild )
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Hole_d ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Cursor_d _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_id : Syntax.Base.Id -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx_id id path =
    case path of
        [] ->
            ( Syntax.CCtx.Cctx_hole, Syntax.Base.Id id )

        i :: rest ->
            case id of
                Syntax.Base.Ident lit ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Hole_id ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Cursor_id _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_e : Syntax.Base.E -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx_e e path =
    case path of
        [] ->
            ( Syntax.CCtx.Cctx_hole, Syntax.Base.E e )

        i :: rest ->
            case e of
                Syntax.Base.Environment arg1 arg2 arg3 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg1 rest
                            in
                            ( Syntax.CCtx.Environment_CLess_cctx1
                                cctxChild
                                (toCLess_c arg2)
                                (toCLess_id arg3)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_c arg2 rest
                            in
                            ( Syntax.CCtx.Environment_CLess_cctx2
                                (toCLess_id arg1)
                                cctxChild
                                (toCLess_id arg3)
                            , restTree
                            )

                        3 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg3 rest
                            in
                            ( Syntax.CCtx.Environment_CLess_cctx3
                                (toCLess_id arg1)
                                (toCLess_c arg2)
                                cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Hole_e ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Cursor_e _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_cmd :
    Syntax.Base.Cmd -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx_cmd cmd path =
    case path of
        [] ->
            ( Syntax.CCtx.Cctx_hole, Syntax.Base.Cmd cmd )

        i :: rest ->
            case cmd of
                Syntax.Base.Command arg1 arg2 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg1 rest
                            in
                            ( Syntax.CCtx.Command_CLess_cctx1
                                cctxChild
                                (toCLess_a arg2)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_a arg2 rest
                            in
                            ( Syntax.CCtx.Command_CLess_cctx2
                                (toCLess_id arg1)
                                cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Hole_cmd ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Cursor_cmd _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_a : Syntax.Base.A -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx_a a path =
    case path of
        [] ->
            ( Syntax.CCtx.Cctx_hole, Syntax.Base.A a )

        i :: rest ->
            case a of
                Syntax.Base.Argument arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_c arg1 rest
                            in
                            ( Syntax.CCtx.Argument_CLess_cctx1 cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Hole_a ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Cursor_a _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_c : Syntax.Base.C -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx_c c path =
    case path of
        [] ->
            ( Syntax.CCtx.Cctx_hole, Syntax.Base.C c )

        i :: rest ->
            case c of
                Syntax.Base.TextContent lit ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.CmdContent arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_cmd arg1 rest
                            in
                            ( Syntax.CCtx.CmdContent_CLess_cctx1 cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.EnvContent arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_e arg1 rest
                            in
                            ( Syntax.CCtx.EnvContent_CLess_cctx1 cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.SeqContent arg1 arg2 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_c arg1 rest
                            in
                            ( Syntax.CCtx.SeqContent_CLess_cctx1
                                cctxChild
                                (toCLess_c arg2)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_c arg2 rest
                            in
                            ( Syntax.CCtx.SeqContent_CLess_cctx2
                                (toCLess_c arg1)
                                cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Hole_c ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Cursor_c _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx : Syntax.Base.Base -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx base path =
    case base of
        Syntax.Base.D arg1 ->
            toCCtx_d arg1 path

        Syntax.Base.Id arg1 ->
            toCCtx_id arg1 path

        Syntax.Base.E arg1 ->
            toCCtx_e arg1 path

        Syntax.Base.Cmd arg1 ->
            toCCtx_cmd arg1 path

        Syntax.Base.A arg1 ->
            toCCtx_a arg1 path

        Syntax.Base.C arg1 ->
            toCCtx_c arg1 path


toWellformed : Syntax.Base.Base -> Syntax.Wellformed.Wellformed
toWellformed base =
    case consumeCursor base of
        Syntax.Base.D arg1 ->
            Syntax.Wellformed.Root_d_CLess (toCLess_d arg1)

        Syntax.Base.Id arg1 ->
            Syntax.Wellformed.Root_id_CLess (toCLess_id arg1)

        Syntax.Base.E arg1 ->
            Syntax.Wellformed.Root_e_CLess (toCLess_e arg1)

        Syntax.Base.Cmd arg1 ->
            Syntax.Wellformed.Root_cmd_CLess (toCLess_cmd arg1)

        Syntax.Base.A arg1 ->
            Syntax.Wellformed.Root_a_CLess (toCLess_a arg1)

        Syntax.Base.C arg1 ->
            Syntax.Wellformed.Root_c_CLess (toCLess_c arg1)


consumeCursor : Syntax.Base.Base -> Syntax.Base.Base
consumeCursor base =
    case base of
        Syntax.Base.D arg1 ->
            case arg1 of
                Syntax.Base.Cursor_d underCursor ->
                    Syntax.Base.D underCursor

                _ ->
                    Syntax.Base.D arg1

        Syntax.Base.Id arg1 ->
            case arg1 of
                Syntax.Base.Cursor_id underCursor ->
                    Syntax.Base.Id underCursor

                _ ->
                    Syntax.Base.Id arg1

        Syntax.Base.E arg1 ->
            case arg1 of
                Syntax.Base.Cursor_e underCursor ->
                    Syntax.Base.E underCursor

                _ ->
                    Syntax.Base.E arg1

        Syntax.Base.Cmd arg1 ->
            case arg1 of
                Syntax.Base.Cursor_cmd underCursor ->
                    Syntax.Base.Cmd underCursor

                _ ->
                    Syntax.Base.Cmd arg1

        Syntax.Base.A arg1 ->
            case arg1 of
                Syntax.Base.Cursor_a underCursor ->
                    Syntax.Base.A underCursor

                _ ->
                    Syntax.Base.A arg1

        Syntax.Base.C arg1 ->
            case arg1 of
                Syntax.Base.Cursor_c underCursor ->
                    Syntax.Base.C underCursor

                _ ->
                    Syntax.Base.C arg1


decompose :
    Syntax.Base.Base -> ( Syntax.CCtx.Cctx, Syntax.Wellformed.Wellformed )
decompose base =
    let
        ( cctx, rest ) =
            toCCtx base (getCursorPath [] base)
    in
    ( cctx, toWellformed rest )


replaceCctxHole :
    Int -> Syntax.CCtx.Cctx -> Syntax.Cursorless.CursorLess -> Syntax.CCtx.Cctx
replaceCctxHole i orig_cctx underCursor =
    case orig_cctx of
        Syntax.CCtx.Cctx_hole ->
            case underCursor of
                Syntax.Cursorless.D_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.Latexdoc_CLess arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Latexdoc_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole
                                        arg2
                                        arg3
                                        arg4
                                        (boundVars5, arg5)

                                2 ->
                                    Syntax.CCtx.Latexdoc_CLess_cctx2
                                        arg1
                                        Syntax.CCtx.Cctx_hole
                                        arg3
                                        arg4
                                        (boundVars5, arg5)

                                3 ->
                                    Syntax.CCtx.Latexdoc_CLess_cctx3
                                        arg1
                                        arg2
                                        Syntax.CCtx.Cctx_hole
                                        arg4
                                        (boundVars5, arg5)

                                4 ->
                                    Syntax.CCtx.Latexdoc_CLess_cctx4
                                        arg1
                                        arg2
                                        arg3
                                        Syntax.CCtx.Cctx_hole
                                        (boundVars5, arg5)

                                5 ->
                                    Syntax.CCtx.Latexdoc_CLess_cctx5
                                        arg1
                                        arg2
                                        arg3
                                        arg4
                                        ( boundVars5, Syntax.CCtx.Cctx_hole )

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Hole_d_CLess ->
                            Debug.todo "Invalid replacement"

                Syntax.Cursorless.Id_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.Ident_CLess lit ->
                            Debug.todo "Invalid replacement"

                        Syntax.Cursorless.Hole_id_CLess ->
                            Debug.todo "Invalid replacement"

                Syntax.Cursorless.E_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.Environment_CLess arg1 arg2 arg3 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Environment_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole
                                        arg2
                                        arg3

                                2 ->
                                    Syntax.CCtx.Environment_CLess_cctx2
                                        arg1
                                        Syntax.CCtx.Cctx_hole
                                        arg3

                                3 ->
                                    Syntax.CCtx.Environment_CLess_cctx3
                                        arg1
                                        arg2
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Hole_e_CLess ->
                            Debug.todo "Invalid replacement"

                Syntax.Cursorless.Cmd_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.Command_CLess arg1 arg2 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Command_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole
                                        arg2

                                2 ->
                                    Syntax.CCtx.Command_CLess_cctx2
                                        arg1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Hole_cmd_CLess ->
                            Debug.todo "Invalid replacement"

                Syntax.Cursorless.A_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.Argument_CLess arg1 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Argument_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Hole_a_CLess ->
                            Debug.todo "Invalid replacement"

                Syntax.Cursorless.C_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.TextContent_CLess lit ->
                            Debug.todo "Invalid replacement"

                        Syntax.Cursorless.CmdContent_CLess arg1 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.CmdContent_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.EnvContent_CLess arg1 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.EnvContent_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.SeqContent_CLess arg1 arg2 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.SeqContent_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole
                                        arg2

                                2 ->
                                    Syntax.CCtx.SeqContent_CLess_cctx2
                                        arg1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Hole_c_CLess ->
                            Debug.todo "Invalid replacement"

        Syntax.CCtx.Latexdoc_CLess_cctx1 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
            Syntax.CCtx.Latexdoc_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                arg2
                arg3
                arg4
                (boundVars5, arg5)

        Syntax.CCtx.Latexdoc_CLess_cctx2 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
            Syntax.CCtx.Latexdoc_CLess_cctx2
                arg1
                (replaceCctxHole i arg2 underCursor)
                arg3
                arg4
                (boundVars5, arg5)

        Syntax.CCtx.Latexdoc_CLess_cctx3 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
            Syntax.CCtx.Latexdoc_CLess_cctx3
                arg1
                arg2
                (replaceCctxHole i arg3 underCursor)
                arg4
                (boundVars5, arg5)

        Syntax.CCtx.Latexdoc_CLess_cctx4 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
            Syntax.CCtx.Latexdoc_CLess_cctx4
                arg1
                arg2
                arg3
                (replaceCctxHole i arg4 underCursor)
                (boundVars5, arg5)

        Syntax.CCtx.Latexdoc_CLess_cctx5 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
            Syntax.CCtx.Latexdoc_CLess_cctx5
                arg1
                arg2
                arg3
                arg4
                ( boundVars5, replaceCctxHole i arg5 underCursor )

        Syntax.CCtx.Environment_CLess_cctx1 arg1 arg2 arg3 ->
            Syntax.CCtx.Environment_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                arg2
                arg3

        Syntax.CCtx.Environment_CLess_cctx2 arg1 arg2 arg3 ->
            Syntax.CCtx.Environment_CLess_cctx2
                arg1
                (replaceCctxHole i arg2 underCursor)
                arg3

        Syntax.CCtx.Environment_CLess_cctx3 arg1 arg2 arg3 ->
            Syntax.CCtx.Environment_CLess_cctx3
                arg1
                arg2
                (replaceCctxHole i arg3 underCursor)

        Syntax.CCtx.Command_CLess_cctx1 arg1 arg2 ->
            Syntax.CCtx.Command_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                arg2

        Syntax.CCtx.Command_CLess_cctx2 arg1 arg2 ->
            Syntax.CCtx.Command_CLess_cctx2
                arg1
                (replaceCctxHole i arg2 underCursor)

        Syntax.CCtx.Argument_CLess_cctx1 cctx ->
            Syntax.CCtx.Argument_CLess_cctx1
                (replaceCctxHole i cctx underCursor)

        Syntax.CCtx.CmdContent_CLess_cctx1 cctx ->
            Syntax.CCtx.CmdContent_CLess_cctx1
                (replaceCctxHole i cctx underCursor)

        Syntax.CCtx.EnvContent_CLess_cctx1 cctx ->
            Syntax.CCtx.EnvContent_CLess_cctx1
                (replaceCctxHole i cctx underCursor)

        Syntax.CCtx.SeqContent_CLess_cctx1 arg1 arg2 ->
            Syntax.CCtx.SeqContent_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                arg2

        Syntax.CCtx.SeqContent_CLess_cctx2 arg1 arg2 ->
            Syntax.CCtx.SeqContent_CLess_cctx2
                arg1
                (replaceCctxHole i arg2 underCursor)


child :
    Int
    -> ( Syntax.CCtx.Cctx, Syntax.Wellformed.Wellformed )
    -> Maybe ( Syntax.CCtx.Cctx, Syntax.Wellformed.Wellformed )
child i decomposed =
    let
        ( cctx, wellformed ) =
            decomposed
    in
    case wellformed of
        Syntax.Wellformed.Root_d_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.Latexdoc_CLess arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.D_CLess underCursor)
                                , Syntax.Wellformed.Root_id_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.D_CLess underCursor)
                                , Syntax.Wellformed.Root_e_CLess arg2
                                )

                        3 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.D_CLess underCursor)
                                , Syntax.Wellformed.Root_a_CLess arg3
                                )

                        4 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.D_CLess underCursor)
                                , Syntax.Wellformed.Root_a_CLess arg4
                                )

                        5 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.D_CLess underCursor)
                                , Syntax.Wellformed.Root_c_CLess arg5
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Hole_d_CLess ->
                    Nothing

        Syntax.Wellformed.Root_e_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.Environment_CLess arg1 arg2 arg3 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.E_CLess underCursor)
                                , Syntax.Wellformed.Root_id_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.E_CLess underCursor)
                                , Syntax.Wellformed.Root_c_CLess arg2
                                )

                        3 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.E_CLess underCursor)
                                , Syntax.Wellformed.Root_id_CLess arg3
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Hole_e_CLess ->
                    Nothing

        Syntax.Wellformed.Root_cmd_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.Command_CLess arg1 arg2 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Cmd_CLess underCursor)
                                , Syntax.Wellformed.Root_id_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Cmd_CLess underCursor)
                                , Syntax.Wellformed.Root_a_CLess arg2
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Hole_cmd_CLess ->
                    Nothing

        Syntax.Wellformed.Root_a_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.Argument_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.A_CLess underCursor)
                                , Syntax.Wellformed.Root_c_CLess arg1
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Hole_a_CLess ->
                    Nothing

        Syntax.Wellformed.Root_id_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.Ident_CLess lit ->
                    Nothing

                Syntax.Cursorless.Hole_id_CLess ->
                    Nothing

        Syntax.Wellformed.Root_c_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.TextContent_CLess lit ->
                    Nothing

                Syntax.Cursorless.CmdContent_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.C_CLess underCursor)
                                , Syntax.Wellformed.Root_cmd_CLess arg1
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.EnvContent_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.C_CLess underCursor)
                                , Syntax.Wellformed.Root_e_CLess arg1
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.SeqContent_CLess arg1 arg2 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.C_CLess underCursor)
                                , Syntax.Wellformed.Root_c_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.C_CLess underCursor)
                                , Syntax.Wellformed.Root_c_CLess arg2
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Hole_c_CLess ->
                    Nothing


substitute :
    ( Syntax.CCtx.Cctx, Syntax.Wellformed.Wellformed )
    -> Syntax.Cursorless.CursorLess
    -> Maybe ( Syntax.CCtx.Cctx, Syntax.Wellformed.Wellformed )
substitute decomposed sub =
    let
        ( cctx, wellformed ) =
            decomposed
    in
    case wellformed of
        Syntax.Wellformed.Root_d_CLess _ ->
            case sub of
                Syntax.Cursorless.D_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_d_CLess sub0 )

                _ ->
                    Nothing

        Syntax.Wellformed.Root_e_CLess _ ->
            case sub of
                Syntax.Cursorless.E_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_e_CLess sub0 )

                _ ->
                    Nothing

        Syntax.Wellformed.Root_cmd_CLess _ ->
            case sub of
                Syntax.Cursorless.Cmd_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_cmd_CLess sub0 )

                _ ->
                    Nothing

        Syntax.Wellformed.Root_a_CLess _ ->
            case sub of
                Syntax.Cursorless.A_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_a_CLess sub0 )

                _ ->
                    Nothing

        Syntax.Wellformed.Root_id_CLess _ ->
            case sub of
                Syntax.Cursorless.Id_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_id_CLess sub0 )

                _ ->
                    Nothing

        Syntax.Wellformed.Root_c_CLess _ ->
            case sub of
                Syntax.Cursorless.C_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_c_CLess sub0 )

                _ ->
                    Nothing


getCursorPath : List Int -> Syntax.Base.Base -> List Int
getCursorPath path base =
    case base of
        Syntax.Base.D d ->
            case d of
                Syntax.Base.Latexdoc arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
                    (((getCursorPath
                           (path ++ [ 1 ])
                           (Syntax.Base.Id arg1) ++ getCursorPath
                                                            (path ++ [ 2 ])
                                                            (Syntax.Base.E arg2)
                      ) ++ getCursorPath (path ++ [ 3 ]) (Syntax.Base.A arg3)
                     ) ++ getCursorPath (path ++ [ 4 ]) (Syntax.Base.A arg4)
                    ) ++ getCursorPath (path ++ [ 5 ]) (Syntax.Base.C arg5)

                Syntax.Base.Hole_d ->
                    []

                Syntax.Base.Cursor_d _ ->
                    path

        Syntax.Base.Id id ->
            case id of
                Syntax.Base.Ident lit ->
                    []

                Syntax.Base.Hole_id ->
                    []

                Syntax.Base.Cursor_id _ ->
                    path

        Syntax.Base.E e ->
            case e of
                Syntax.Base.Environment arg1 arg2 arg3 ->
                    (getCursorPath
                         (path ++ [ 1 ])
                         (Syntax.Base.Id arg1) ++ getCursorPath
                                                          (path ++ [ 2 ])
                                                          (Syntax.Base.C arg2)
                    ) ++ getCursorPath (path ++ [ 3 ]) (Syntax.Base.Id arg3)

                Syntax.Base.Hole_e ->
                    []

                Syntax.Base.Cursor_e _ ->
                    path

        Syntax.Base.Cmd cmd ->
            case cmd of
                Syntax.Base.Command arg1 arg2 ->
                    getCursorPath
                        (path ++ [ 1 ])
                        (Syntax.Base.Id arg1) ++ getCursorPath
                                                         (path ++ [ 2 ])
                                                         (Syntax.Base.A arg2)

                Syntax.Base.Hole_cmd ->
                    []

                Syntax.Base.Cursor_cmd _ ->
                    path

        Syntax.Base.A a ->
            case a of
                Syntax.Base.Argument arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Syntax.Base.C arg1)

                Syntax.Base.Hole_a ->
                    []

                Syntax.Base.Cursor_a _ ->
                    path

        Syntax.Base.C c ->
            case c of
                Syntax.Base.TextContent lit ->
                    []

                Syntax.Base.CmdContent arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Syntax.Base.Cmd arg1)

                Syntax.Base.EnvContent arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Syntax.Base.E arg1)

                Syntax.Base.SeqContent arg1 arg2 ->
                    getCursorPath
                        (path ++ [ 1 ])
                        (Syntax.Base.C arg1) ++ getCursorPath
                                                        (path ++ [ 2 ])
                                                        (Syntax.Base.C arg2)

                Syntax.Base.Hole_c ->
                    []

                Syntax.Base.Cursor_c _ ->
                    path


getCctxPath : Syntax.CCtx.Cctx -> List Int -> List Int
getCctxPath cctx path =
    case cctx of
        Syntax.CCtx.Cctx_hole ->
            path

        Syntax.CCtx.Latexdoc_CLess_cctx1 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Latexdoc_CLess_cctx2 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
            getCctxPath arg2 (path ++ [ 2 ])

        Syntax.CCtx.Latexdoc_CLess_cctx3 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
            getCctxPath arg3 (path ++ [ 3 ])

        Syntax.CCtx.Latexdoc_CLess_cctx4 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
            getCctxPath arg4 (path ++ [ 4 ])

        Syntax.CCtx.Latexdoc_CLess_cctx5 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
            getCctxPath arg5 (path ++ [ 5 ])

        Syntax.CCtx.Environment_CLess_cctx1 arg1 arg2 arg3 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Environment_CLess_cctx2 arg1 arg2 arg3 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Syntax.CCtx.Environment_CLess_cctx3 arg1 arg2 arg3 ->
            getCctxPath arg3 (path ++ [ 3 ])

        Syntax.CCtx.Command_CLess_cctx1 arg1 arg2 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Command_CLess_cctx2 arg1 arg2 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Syntax.CCtx.Argument_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.CmdContent_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.EnvContent_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.SeqContent_CLess_cctx1 arg1 arg2 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.SeqContent_CLess_cctx2 arg1 arg2 ->
            getCctxPath arg2 (path ++ [ 2 ])


moveCCtxHoleUp :
    Syntax.CCtx.Cctx -> List Int -> Maybe ( Syntax.CCtx.Cctx, Syntax.CCtx.Cctx )
moveCCtxHoleUp cctx path =
    case path of
        [_,_] ->
            case cctx of
                Syntax.CCtx.Cctx_hole ->
                    Nothing

                Syntax.CCtx.Latexdoc_CLess_cctx1 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
                    Just
                        ( Syntax.CCtx.Latexdoc_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                            arg2
                            arg3
                            arg4
                            (boundVars5, arg5)
                        , arg1
                        )

                Syntax.CCtx.Latexdoc_CLess_cctx2 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
                    Just
                        ( Syntax.CCtx.Latexdoc_CLess_cctx2
                            arg1
                            Syntax.CCtx.Cctx_hole
                            arg3
                            arg4
                            (boundVars5, arg5)
                        , arg2
                        )

                Syntax.CCtx.Latexdoc_CLess_cctx3 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
                    Just
                        ( Syntax.CCtx.Latexdoc_CLess_cctx3
                            arg1
                            arg2
                            Syntax.CCtx.Cctx_hole
                            arg4
                            (boundVars5, arg5)
                        , arg3
                        )

                Syntax.CCtx.Latexdoc_CLess_cctx4 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
                    Just
                        ( Syntax.CCtx.Latexdoc_CLess_cctx4
                            arg1
                            arg2
                            arg3
                            Syntax.CCtx.Cctx_hole
                            (boundVars5, arg5)
                        , arg4
                        )

                Syntax.CCtx.Latexdoc_CLess_cctx5 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
                    Just
                        ( Syntax.CCtx.Latexdoc_CLess_cctx5
                            arg1
                            arg2
                            arg3
                            arg4
                            ( boundVars5, Syntax.CCtx.Cctx_hole )
                        , arg5
                        )

                Syntax.CCtx.Environment_CLess_cctx1 arg1 arg2 arg3 ->
                    Just
                        ( Syntax.CCtx.Environment_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                            arg2
                            arg3
                        , arg1
                        )

                Syntax.CCtx.Environment_CLess_cctx2 arg1 arg2 arg3 ->
                    Just
                        ( Syntax.CCtx.Environment_CLess_cctx2
                            arg1
                            Syntax.CCtx.Cctx_hole
                            arg3
                        , arg2
                        )

                Syntax.CCtx.Environment_CLess_cctx3 arg1 arg2 arg3 ->
                    Just
                        ( Syntax.CCtx.Environment_CLess_cctx3
                            arg1
                            arg2
                            Syntax.CCtx.Cctx_hole
                        , arg3
                        )

                Syntax.CCtx.Command_CLess_cctx1 arg1 arg2 ->
                    Just
                        ( Syntax.CCtx.Command_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                            arg2
                        , arg1
                        )

                Syntax.CCtx.Command_CLess_cctx2 arg1 arg2 ->
                    Just
                        ( Syntax.CCtx.Command_CLess_cctx2
                            arg1
                            Syntax.CCtx.Cctx_hole
                        , arg2
                        )

                Syntax.CCtx.Argument_CLess_cctx1 arg1 ->
                    Just
                        ( Syntax.CCtx.Argument_CLess_cctx1 Syntax.CCtx.Cctx_hole
                        , arg1
                        )

                Syntax.CCtx.CmdContent_CLess_cctx1 arg1 ->
                    Just
                        ( Syntax.CCtx.CmdContent_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                        , arg1
                        )

                Syntax.CCtx.EnvContent_CLess_cctx1 arg1 ->
                    Just
                        ( Syntax.CCtx.EnvContent_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                        , arg1
                        )

                Syntax.CCtx.SeqContent_CLess_cctx1 arg1 arg2 ->
                    Just
                        ( Syntax.CCtx.SeqContent_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                            arg2
                        , arg1
                        )

                Syntax.CCtx.SeqContent_CLess_cctx2 arg1 arg2 ->
                    Just
                        ( Syntax.CCtx.SeqContent_CLess_cctx2
                            arg1
                            Syntax.CCtx.Cctx_hole
                        , arg2
                        )

        [_] ->
            Just ( Syntax.CCtx.Cctx_hole, cctx )

        _ :: rest ->
            case cctx of
                Syntax.CCtx.Cctx_hole ->
                    Nothing

                Syntax.CCtx.Latexdoc_CLess_cctx1 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Latexdoc_CLess_cctx1
                                                                     newCctx
                                                                     arg2
                                                                     arg3
                                                                     arg4
                                                                     (boundVars5, arg5)
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Latexdoc_CLess_cctx2 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
                    moveCCtxHoleUp arg2 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Latexdoc_CLess_cctx2
                                                                     arg1
                                                                     newCctx
                                                                     arg3
                                                                     arg4
                                                                     (boundVars5, arg5)
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Latexdoc_CLess_cctx3 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
                    moveCCtxHoleUp arg3 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Latexdoc_CLess_cctx3
                                                                     arg1
                                                                     arg2
                                                                     newCctx
                                                                     arg4
                                                                     (boundVars5, arg5)
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Latexdoc_CLess_cctx4 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
                    moveCCtxHoleUp arg4 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Latexdoc_CLess_cctx4
                                                                     arg1
                                                                     arg2
                                                                     arg3
                                                                     newCctx
                                                                     (boundVars5, arg5)
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Latexdoc_CLess_cctx5 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
                    moveCCtxHoleUp arg5 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Latexdoc_CLess_cctx5
                                                                     arg1
                                                                     arg2
                                                                     arg3
                                                                     arg4
                                                                     (boundVars5, newCctx )
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Environment_CLess_cctx1 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Environment_CLess_cctx1
                                                                     newCctx
                                                                     arg2
                                                                     arg3
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Environment_CLess_cctx2 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg2 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Environment_CLess_cctx2
                                                                     arg1
                                                                     newCctx
                                                                     arg3
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Environment_CLess_cctx3 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg3 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Environment_CLess_cctx3
                                                                     arg1
                                                                     arg2
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Command_CLess_cctx1 arg1 arg2 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Command_CLess_cctx1
                                                                     newCctx
                                                                     arg2
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Command_CLess_cctx2 arg1 arg2 ->
                    moveCCtxHoleUp arg2 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Command_CLess_cctx2
                                                                     arg1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Argument_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Argument_CLess_cctx1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.CmdContent_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.CmdContent_CLess_cctx1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.EnvContent_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.EnvContent_CLess_cctx1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.SeqContent_CLess_cctx1 arg1 arg2 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.SeqContent_CLess_cctx1
                                                                     newCctx
                                                                     arg2
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.SeqContent_CLess_cctx2 arg1 arg2 ->
                    moveCCtxHoleUp arg2 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.SeqContent_CLess_cctx2
                                                                     arg1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

        [] ->
            Nothing


addParent :
    Syntax.CCtx.Cctx
    -> Syntax.Wellformed.Wellformed
    -> Maybe Syntax.Wellformed.Wellformed
addParent cctx wellformed =
    case cctx of
        Syntax.CCtx.Cctx_hole ->
            Nothing

        Syntax.CCtx.Latexdoc_CLess_cctx1 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
            case wellformed of
                Syntax.Wellformed.Root_id_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_d_CLess
                             (Syntax.Cursorless.Latexdoc_CLess
                                  underCursor
                                  arg2
                                  arg3
                                  arg4
                                  (boundVars5, arg5)
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Latexdoc_CLess_cctx2 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
            case wellformed of
                Syntax.Wellformed.Root_e_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_d_CLess
                             (Syntax.Cursorless.Latexdoc_CLess
                                  arg1
                                  underCursor
                                  arg3
                                  arg4
                                  (boundVars5, arg5)
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Latexdoc_CLess_cctx3 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
            case wellformed of
                Syntax.Wellformed.Root_a_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_d_CLess
                             (Syntax.Cursorless.Latexdoc_CLess
                                  arg1
                                  arg2
                                  underCursor
                                  arg4
                                  (boundVars5, arg5)
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Latexdoc_CLess_cctx4 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
            case wellformed of
                Syntax.Wellformed.Root_a_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_d_CLess
                             (Syntax.Cursorless.Latexdoc_CLess
                                  arg1
                                  arg2
                                  arg3
                                  underCursor
                                  (boundVars5, arg5)
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Latexdoc_CLess_cctx5 arg1 arg2 arg3 arg4 (boundVars5, arg5) ->
            case wellformed of
                Syntax.Wellformed.Root_c_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_d_CLess
                             (Syntax.Cursorless.Latexdoc_CLess
                                  arg1
                                  arg2
                                  arg3
                                  arg4
                                  (boundVars5, underCursor )
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Environment_CLess_cctx1 arg1 arg2 arg3 ->
            case wellformed of
                Syntax.Wellformed.Root_id_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_e_CLess
                             (Syntax.Cursorless.Environment_CLess
                                  underCursor
                                  arg2
                                  arg3
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Environment_CLess_cctx2 arg1 arg2 arg3 ->
            case wellformed of
                Syntax.Wellformed.Root_c_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_e_CLess
                             (Syntax.Cursorless.Environment_CLess
                                  arg1
                                  underCursor
                                  arg3
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Environment_CLess_cctx3 arg1 arg2 arg3 ->
            case wellformed of
                Syntax.Wellformed.Root_id_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_e_CLess
                             (Syntax.Cursorless.Environment_CLess
                                  arg1
                                  arg2
                                  underCursor
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Command_CLess_cctx1 arg1 arg2 ->
            case wellformed of
                Syntax.Wellformed.Root_id_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_cmd_CLess
                             (Syntax.Cursorless.Command_CLess underCursor arg2)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Command_CLess_cctx2 arg1 arg2 ->
            case wellformed of
                Syntax.Wellformed.Root_a_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_cmd_CLess
                             (Syntax.Cursorless.Command_CLess arg1 underCursor)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Argument_CLess_cctx1 arg1 ->
            case wellformed of
                Syntax.Wellformed.Root_c_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_a_CLess
                             (Syntax.Cursorless.Argument_CLess underCursor)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.CmdContent_CLess_cctx1 arg1 ->
            case wellformed of
                Syntax.Wellformed.Root_cmd_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_c_CLess
                             (Syntax.Cursorless.CmdContent_CLess underCursor)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.EnvContent_CLess_cctx1 arg1 ->
            case wellformed of
                Syntax.Wellformed.Root_e_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_c_CLess
                             (Syntax.Cursorless.EnvContent_CLess underCursor)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.SeqContent_CLess_cctx1 arg1 arg2 ->
            case wellformed of
                Syntax.Wellformed.Root_c_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_c_CLess
                             (Syntax.Cursorless.SeqContent_CLess
                                  underCursor
                                  arg2
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.SeqContent_CLess_cctx2 arg1 arg2 ->
            case wellformed of
                Syntax.Wellformed.Root_c_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_c_CLess
                             (Syntax.Cursorless.SeqContent_CLess
                                  arg1
                                  underCursor
                             )
                        )

                _ ->
                    Nothing


parent :
    ( Syntax.CCtx.Cctx, Syntax.Wellformed.Wellformed )
    -> Maybe ( Syntax.CCtx.Cctx, Syntax.Wellformed.Wellformed )
parent decomposed =
    let
        ( cctx, wellformed ) =
            decomposed
    in
    case moveCCtxHoleUp cctx (getCctxPath cctx []) of
        Nothing ->
            Nothing

        Just ( newCctx, removedCctx ) ->
            case addParent removedCctx wellformed of
                Nothing ->
                    Nothing

                Just newWellformed ->
                    Just ( newCctx, newWellformed )