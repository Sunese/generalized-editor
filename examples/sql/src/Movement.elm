module Movement exposing (..)

import Syntax.Base
import Syntax.CCtx
import Syntax.Cursorless
import Syntax.Wellformed


toCLess_q : Syntax.Base.Q -> Syntax.Cursorless.Q_CLess
toCLess_q q =
    case q of
        Syntax.Base.Select arg1 arg2 arg3 ->
            Syntax.Cursorless.Select_CLess
                (toCLess_id arg1)
                (toCLess_id arg2)
                (toCLess_clause arg3)

        Syntax.Base.Hole_q ->
            Syntax.Cursorless.Hole_q_CLess

        Syntax.Base.Cursor_q cursor ->
            Debug.todo "Not wellformed"


toCLess_cmd : Syntax.Base.Cmd -> Syntax.Cursorless.Cmd_CLess
toCLess_cmd cmd =
    case cmd of
        Syntax.Base.Insert arg1 (boundVars2, arg2) ->
            Syntax.Cursorless.Insert_CLess
                (toCLess_id arg1)
                ( List.map toCLess_id boundVars2, toCLess_q arg2 )

        Syntax.Base.Hole_cmd ->
            Syntax.Cursorless.Hole_cmd_CLess

        Syntax.Base.Cursor_cmd cursor ->
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


toCLess_const : Syntax.Base.Const -> Syntax.Cursorless.Const_CLess
toCLess_const const =
    case const of
        Syntax.Base.Num lit ->
            Syntax.Cursorless.Num_CLess lit

        Syntax.Base.Str lit ->
            Syntax.Cursorless.Str_CLess lit

        Syntax.Base.Hole_const ->
            Syntax.Cursorless.Hole_const_CLess

        Syntax.Base.Cursor_const cursor ->
            Debug.todo "Not wellformed"


toCLess_clause : Syntax.Base.Clause -> Syntax.Cursorless.Clause_CLess
toCLess_clause clause =
    case clause of
        Syntax.Base.Where arg1 ->
            Syntax.Cursorless.Where_CLess (toCLess_cond arg1)

        Syntax.Base.Having arg1 ->
            Syntax.Cursorless.Having_CLess (toCLess_cond arg1)

        Syntax.Base.Hole_clause ->
            Syntax.Cursorless.Hole_clause_CLess

        Syntax.Base.Cursor_clause cursor ->
            Debug.todo "Not wellformed"


toCLess_cond : Syntax.Base.Cond -> Syntax.Cursorless.Cond_CLess
toCLess_cond cond =
    case cond of
        Syntax.Base.Greater arg1 arg2 ->
            Syntax.Cursorless.Greater_CLess
                (toCLess_exp arg1)
                (toCLess_exp arg2)

        Syntax.Base.Equals arg1 arg2 ->
            Syntax.Cursorless.Equals_CLess (toCLess_exp arg1) (toCLess_exp arg2)

        Syntax.Base.Hole_cond ->
            Syntax.Cursorless.Hole_cond_CLess

        Syntax.Base.Cursor_cond cursor ->
            Debug.todo "Not wellformed"


toCLess_exp : Syntax.Base.Exp -> Syntax.Cursorless.Exp_CLess
toCLess_exp exp =
    case exp of
        Syntax.Base.Econst arg1 ->
            Syntax.Cursorless.Econst_CLess (toCLess_const arg1)

        Syntax.Base.Eident arg1 ->
            Syntax.Cursorless.Eident_CLess (toCLess_id arg1)

        Syntax.Base.Hole_exp ->
            Syntax.Cursorless.Hole_exp_CLess

        Syntax.Base.Cursor_exp cursor ->
            Debug.todo "Not wellformed"


toCLess : Syntax.Base.Base -> Syntax.Cursorless.CursorLess
toCLess base =
    case base of
        Syntax.Base.Q arg1 ->
            Syntax.Cursorless.Q_CLess (toCLess_q arg1)

        Syntax.Base.Cmd arg1 ->
            Syntax.Cursorless.Cmd_CLess (toCLess_cmd arg1)

        Syntax.Base.Id arg1 ->
            Syntax.Cursorless.Id_CLess (toCLess_id arg1)

        Syntax.Base.Const arg1 ->
            Syntax.Cursorless.Const_CLess (toCLess_const arg1)

        Syntax.Base.Clause arg1 ->
            Syntax.Cursorless.Clause_CLess (toCLess_clause arg1)

        Syntax.Base.Cond arg1 ->
            Syntax.Cursorless.Cond_CLess (toCLess_cond arg1)

        Syntax.Base.Exp arg1 ->
            Syntax.Cursorless.Exp_CLess (toCLess_exp arg1)


toCCtx_q : Syntax.Base.Q -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx_q q path =
    case path of
        [] ->
            ( Syntax.CCtx.Cctx_hole, Syntax.Base.Q q )

        i :: rest ->
            case q of
                Syntax.Base.Select arg1 arg2 arg3 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg1 rest
                            in
                            ( Syntax.CCtx.Select_CLess_cctx1
                                cctxChild
                                (toCLess_id arg2)
                                (toCLess_clause arg3)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg2 rest
                            in
                            ( Syntax.CCtx.Select_CLess_cctx2
                                (toCLess_id arg1)
                                cctxChild
                                (toCLess_clause arg3)
                            , restTree
                            )

                        3 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_clause arg3 rest
                            in
                            ( Syntax.CCtx.Select_CLess_cctx3
                                (toCLess_id arg1)
                                (toCLess_id arg2)
                                cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Hole_q ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Cursor_q _ ->
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
                Syntax.Base.Insert arg1 (boundVars2, arg2) ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg1 rest
                            in
                            ( Syntax.CCtx.Insert_CLess_cctx1
                                cctxChild
                                ( List.map toCLess_id boundVars2
                                , toCLess_q arg2
                                )
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_q arg2 rest
                            in
                            ( Syntax.CCtx.Insert_CLess_cctx2
                                (toCLess_id arg1)
                                ( List.map toCLess_id boundVars2, cctxChild )
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


toCCtx_const :
    Syntax.Base.Const -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx_const const path =
    case path of
        [] ->
            ( Syntax.CCtx.Cctx_hole, Syntax.Base.Const const )

        i :: rest ->
            case const of
                Syntax.Base.Num lit ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Str lit ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Hole_const ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Cursor_const _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_clause :
    Syntax.Base.Clause -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx_clause clause path =
    case path of
        [] ->
            ( Syntax.CCtx.Cctx_hole, Syntax.Base.Clause clause )

        i :: rest ->
            case clause of
                Syntax.Base.Where arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_cond arg1 rest
                            in
                            ( Syntax.CCtx.Where_CLess_cctx1 cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Having arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_cond arg1 rest
                            in
                            ( Syntax.CCtx.Having_CLess_cctx1 cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Hole_clause ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Cursor_clause _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_cond :
    Syntax.Base.Cond -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx_cond cond path =
    case path of
        [] ->
            ( Syntax.CCtx.Cctx_hole, Syntax.Base.Cond cond )

        i :: rest ->
            case cond of
                Syntax.Base.Greater arg1 arg2 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_exp arg1 rest
                            in
                            ( Syntax.CCtx.Greater_CLess_cctx1
                                cctxChild
                                (toCLess_exp arg2)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_exp arg2 rest
                            in
                            ( Syntax.CCtx.Greater_CLess_cctx2
                                (toCLess_exp arg1)
                                cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Equals arg1 arg2 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_exp arg1 rest
                            in
                            ( Syntax.CCtx.Equals_CLess_cctx1
                                cctxChild
                                (toCLess_exp arg2)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_exp arg2 rest
                            in
                            ( Syntax.CCtx.Equals_CLess_cctx2
                                (toCLess_exp arg1)
                                cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Hole_cond ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Cursor_cond _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_exp :
    Syntax.Base.Exp -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx_exp exp path =
    case path of
        [] ->
            ( Syntax.CCtx.Cctx_hole, Syntax.Base.Exp exp )

        i :: rest ->
            case exp of
                Syntax.Base.Econst arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_const arg1 rest
                            in
                            ( Syntax.CCtx.Econst_CLess_cctx1 cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Eident arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg1 rest
                            in
                            ( Syntax.CCtx.Eident_CLess_cctx1 cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Syntax.Base.Hole_exp ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Syntax.Base.Cursor_exp _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx : Syntax.Base.Base -> List Int -> ( Syntax.CCtx.Cctx, Syntax.Base.Base )
toCCtx base path =
    case base of
        Syntax.Base.Q arg1 ->
            toCCtx_q arg1 path

        Syntax.Base.Cmd arg1 ->
            toCCtx_cmd arg1 path

        Syntax.Base.Id arg1 ->
            toCCtx_id arg1 path

        Syntax.Base.Const arg1 ->
            toCCtx_const arg1 path

        Syntax.Base.Clause arg1 ->
            toCCtx_clause arg1 path

        Syntax.Base.Cond arg1 ->
            toCCtx_cond arg1 path

        Syntax.Base.Exp arg1 ->
            toCCtx_exp arg1 path


toWellformed : Syntax.Base.Base -> Syntax.Wellformed.Wellformed
toWellformed base =
    case consumeCursor base of
        Syntax.Base.Q arg1 ->
            Syntax.Wellformed.Root_q_CLess (toCLess_q arg1)

        Syntax.Base.Cmd arg1 ->
            Syntax.Wellformed.Root_cmd_CLess (toCLess_cmd arg1)

        Syntax.Base.Id arg1 ->
            Syntax.Wellformed.Root_id_CLess (toCLess_id arg1)

        Syntax.Base.Const arg1 ->
            Syntax.Wellformed.Root_const_CLess (toCLess_const arg1)

        Syntax.Base.Clause arg1 ->
            Syntax.Wellformed.Root_clause_CLess (toCLess_clause arg1)

        Syntax.Base.Cond arg1 ->
            Syntax.Wellformed.Root_cond_CLess (toCLess_cond arg1)

        Syntax.Base.Exp arg1 ->
            Syntax.Wellformed.Root_exp_CLess (toCLess_exp arg1)


consumeCursor : Syntax.Base.Base -> Syntax.Base.Base
consumeCursor base =
    case base of
        Syntax.Base.Q arg1 ->
            case arg1 of
                Syntax.Base.Cursor_q underCursor ->
                    Syntax.Base.Q underCursor

                _ ->
                    Syntax.Base.Q arg1

        Syntax.Base.Cmd arg1 ->
            case arg1 of
                Syntax.Base.Cursor_cmd underCursor ->
                    Syntax.Base.Cmd underCursor

                _ ->
                    Syntax.Base.Cmd arg1

        Syntax.Base.Id arg1 ->
            case arg1 of
                Syntax.Base.Cursor_id underCursor ->
                    Syntax.Base.Id underCursor

                _ ->
                    Syntax.Base.Id arg1

        Syntax.Base.Const arg1 ->
            case arg1 of
                Syntax.Base.Cursor_const underCursor ->
                    Syntax.Base.Const underCursor

                _ ->
                    Syntax.Base.Const arg1

        Syntax.Base.Clause arg1 ->
            case arg1 of
                Syntax.Base.Cursor_clause underCursor ->
                    Syntax.Base.Clause underCursor

                _ ->
                    Syntax.Base.Clause arg1

        Syntax.Base.Cond arg1 ->
            case arg1 of
                Syntax.Base.Cursor_cond underCursor ->
                    Syntax.Base.Cond underCursor

                _ ->
                    Syntax.Base.Cond arg1

        Syntax.Base.Exp arg1 ->
            case arg1 of
                Syntax.Base.Cursor_exp underCursor ->
                    Syntax.Base.Exp underCursor

                _ ->
                    Syntax.Base.Exp arg1


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
                Syntax.Cursorless.Q_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.Select_CLess arg1 arg2 arg3 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Select_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole
                                        arg2
                                        arg3

                                2 ->
                                    Syntax.CCtx.Select_CLess_cctx2
                                        arg1
                                        Syntax.CCtx.Cctx_hole
                                        arg3

                                3 ->
                                    Syntax.CCtx.Select_CLess_cctx3
                                        arg1
                                        arg2
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Hole_q_CLess ->
                            Debug.todo "Invalid replacement"

                Syntax.Cursorless.Cmd_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.Insert_CLess arg1 (boundVars2, arg2) ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Insert_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole
                                        (boundVars2, arg2)

                                2 ->
                                    Syntax.CCtx.Insert_CLess_cctx2
                                        arg1
                                        ( boundVars2, Syntax.CCtx.Cctx_hole )

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Hole_cmd_CLess ->
                            Debug.todo "Invalid replacement"

                Syntax.Cursorless.Id_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.Ident_CLess lit ->
                            Debug.todo "Invalid replacement"

                        Syntax.Cursorless.Hole_id_CLess ->
                            Debug.todo "Invalid replacement"

                Syntax.Cursorless.Const_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.Num_CLess lit ->
                            Debug.todo "Invalid replacement"

                        Syntax.Cursorless.Str_CLess lit ->
                            Debug.todo "Invalid replacement"

                        Syntax.Cursorless.Hole_const_CLess ->
                            Debug.todo "Invalid replacement"

                Syntax.Cursorless.Clause_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.Where_CLess arg1 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Where_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Having_CLess arg1 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Having_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Hole_clause_CLess ->
                            Debug.todo "Invalid replacement"

                Syntax.Cursorless.Cond_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.Greater_CLess arg1 arg2 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Greater_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole
                                        arg2

                                2 ->
                                    Syntax.CCtx.Greater_CLess_cctx2
                                        arg1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Equals_CLess arg1 arg2 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Equals_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole
                                        arg2

                                2 ->
                                    Syntax.CCtx.Equals_CLess_cctx2
                                        arg1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Hole_cond_CLess ->
                            Debug.todo "Invalid replacement"

                Syntax.Cursorless.Exp_CLess underCursor0 ->
                    case underCursor0 of
                        Syntax.Cursorless.Econst_CLess arg1 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Econst_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Eident_CLess arg1 ->
                            case i of
                                1 ->
                                    Syntax.CCtx.Eident_CLess_cctx1
                                        Syntax.CCtx.Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Syntax.Cursorless.Hole_exp_CLess ->
                            Debug.todo "Invalid replacement"

        Syntax.CCtx.Select_CLess_cctx1 arg1 arg2 arg3 ->
            Syntax.CCtx.Select_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                arg2
                arg3

        Syntax.CCtx.Select_CLess_cctx2 arg1 arg2 arg3 ->
            Syntax.CCtx.Select_CLess_cctx2
                arg1
                (replaceCctxHole i arg2 underCursor)
                arg3

        Syntax.CCtx.Select_CLess_cctx3 arg1 arg2 arg3 ->
            Syntax.CCtx.Select_CLess_cctx3
                arg1
                arg2
                (replaceCctxHole i arg3 underCursor)

        Syntax.CCtx.Insert_CLess_cctx1 arg1 (boundVars2, arg2) ->
            Syntax.CCtx.Insert_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                (boundVars2, arg2)

        Syntax.CCtx.Insert_CLess_cctx2 arg1 (boundVars2, arg2) ->
            Syntax.CCtx.Insert_CLess_cctx2
                arg1
                ( boundVars2, replaceCctxHole i arg2 underCursor )

        Syntax.CCtx.Where_CLess_cctx1 cctx ->
            Syntax.CCtx.Where_CLess_cctx1 (replaceCctxHole i cctx underCursor)

        Syntax.CCtx.Having_CLess_cctx1 cctx ->
            Syntax.CCtx.Having_CLess_cctx1 (replaceCctxHole i cctx underCursor)

        Syntax.CCtx.Greater_CLess_cctx1 arg1 arg2 ->
            Syntax.CCtx.Greater_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                arg2

        Syntax.CCtx.Greater_CLess_cctx2 arg1 arg2 ->
            Syntax.CCtx.Greater_CLess_cctx2
                arg1
                (replaceCctxHole i arg2 underCursor)

        Syntax.CCtx.Equals_CLess_cctx1 arg1 arg2 ->
            Syntax.CCtx.Equals_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                arg2

        Syntax.CCtx.Equals_CLess_cctx2 arg1 arg2 ->
            Syntax.CCtx.Equals_CLess_cctx2
                arg1
                (replaceCctxHole i arg2 underCursor)

        Syntax.CCtx.Econst_CLess_cctx1 cctx ->
            Syntax.CCtx.Econst_CLess_cctx1 (replaceCctxHole i cctx underCursor)

        Syntax.CCtx.Eident_CLess_cctx1 cctx ->
            Syntax.CCtx.Eident_CLess_cctx1 (replaceCctxHole i cctx underCursor)


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
        Syntax.Wellformed.Root_q_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.Select_CLess arg1 arg2 arg3 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Q_CLess underCursor)
                                , Syntax.Wellformed.Root_id_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Q_CLess underCursor)
                                , Syntax.Wellformed.Root_id_CLess arg2
                                )

                        3 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Q_CLess underCursor)
                                , Syntax.Wellformed.Root_clause_CLess arg3
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Hole_q_CLess ->
                    Nothing

        Syntax.Wellformed.Root_cmd_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.Insert_CLess arg1 (boundVars2, arg2) ->
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
                                , Syntax.Wellformed.Root_q_CLess arg2
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Hole_cmd_CLess ->
                    Nothing

        Syntax.Wellformed.Root_id_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.Ident_CLess lit ->
                    Nothing

                Syntax.Cursorless.Hole_id_CLess ->
                    Nothing

        Syntax.Wellformed.Root_const_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.Num_CLess lit ->
                    Nothing

                Syntax.Cursorless.Str_CLess lit ->
                    Nothing

                Syntax.Cursorless.Hole_const_CLess ->
                    Nothing

        Syntax.Wellformed.Root_clause_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.Where_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Clause_CLess underCursor)
                                , Syntax.Wellformed.Root_cond_CLess arg1
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Having_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Clause_CLess underCursor)
                                , Syntax.Wellformed.Root_cond_CLess arg1
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Hole_clause_CLess ->
                    Nothing

        Syntax.Wellformed.Root_cond_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.Greater_CLess arg1 arg2 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Cond_CLess underCursor)
                                , Syntax.Wellformed.Root_exp_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Cond_CLess underCursor)
                                , Syntax.Wellformed.Root_exp_CLess arg2
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Equals_CLess arg1 arg2 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Cond_CLess underCursor)
                                , Syntax.Wellformed.Root_exp_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Cond_CLess underCursor)
                                , Syntax.Wellformed.Root_exp_CLess arg2
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Hole_cond_CLess ->
                    Nothing

        Syntax.Wellformed.Root_exp_CLess underCursor ->
            case underCursor of
                Syntax.Cursorless.Econst_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Exp_CLess underCursor)
                                , Syntax.Wellformed.Root_const_CLess arg1
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Eident_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Syntax.Cursorless.Exp_CLess underCursor)
                                , Syntax.Wellformed.Root_id_CLess arg1
                                )

                        _ ->
                            Nothing

                Syntax.Cursorless.Hole_exp_CLess ->
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
        Syntax.Wellformed.Root_q_CLess _ ->
            case sub of
                Syntax.Cursorless.Q_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_q_CLess sub0 )

                _ ->
                    Nothing

        Syntax.Wellformed.Root_cmd_CLess _ ->
            case sub of
                Syntax.Cursorless.Cmd_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_cmd_CLess sub0 )

                _ ->
                    Nothing

        Syntax.Wellformed.Root_id_CLess _ ->
            case sub of
                Syntax.Cursorless.Id_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_id_CLess sub0 )

                _ ->
                    Nothing

        Syntax.Wellformed.Root_const_CLess _ ->
            case sub of
                Syntax.Cursorless.Const_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_const_CLess sub0 )

                _ ->
                    Nothing

        Syntax.Wellformed.Root_clause_CLess _ ->
            case sub of
                Syntax.Cursorless.Clause_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_clause_CLess sub0 )

                _ ->
                    Nothing

        Syntax.Wellformed.Root_cond_CLess _ ->
            case sub of
                Syntax.Cursorless.Cond_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_cond_CLess sub0 )

                _ ->
                    Nothing

        Syntax.Wellformed.Root_exp_CLess _ ->
            case sub of
                Syntax.Cursorless.Exp_CLess sub0 ->
                    Just ( cctx, Syntax.Wellformed.Root_exp_CLess sub0 )

                _ ->
                    Nothing


getCursorPath : List Int -> Syntax.Base.Base -> List Int
getCursorPath path base =
    case base of
        Syntax.Base.Q q ->
            case q of
                Syntax.Base.Select arg1 arg2 arg3 ->
                    (getCursorPath
                         (path ++ [ 1 ])
                         (Syntax.Base.Id arg1) ++ getCursorPath
                                                          (path ++ [ 2 ])
                                                          (Syntax.Base.Id arg2)
                    ) ++ getCursorPath (path ++ [ 3 ]) (Syntax.Base.Clause arg3)

                Syntax.Base.Hole_q ->
                    []

                Syntax.Base.Cursor_q _ ->
                    path

        Syntax.Base.Cmd cmd ->
            case cmd of
                Syntax.Base.Insert arg1 (boundVars2, arg2) ->
                    getCursorPath
                        (path ++ [ 1 ])
                        (Syntax.Base.Id arg1) ++ getCursorPath
                                                         (path ++ [ 2 ])
                                                         (Syntax.Base.Q arg2)

                Syntax.Base.Hole_cmd ->
                    []

                Syntax.Base.Cursor_cmd _ ->
                    path

        Syntax.Base.Id id ->
            case id of
                Syntax.Base.Ident lit ->
                    []

                Syntax.Base.Hole_id ->
                    []

                Syntax.Base.Cursor_id _ ->
                    path

        Syntax.Base.Const const ->
            case const of
                Syntax.Base.Num lit ->
                    []

                Syntax.Base.Str lit ->
                    []

                Syntax.Base.Hole_const ->
                    []

                Syntax.Base.Cursor_const _ ->
                    path

        Syntax.Base.Clause clause ->
            case clause of
                Syntax.Base.Where arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Syntax.Base.Cond arg1)

                Syntax.Base.Having arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Syntax.Base.Cond arg1)

                Syntax.Base.Hole_clause ->
                    []

                Syntax.Base.Cursor_clause _ ->
                    path

        Syntax.Base.Cond cond ->
            case cond of
                Syntax.Base.Greater arg1 arg2 ->
                    getCursorPath
                        (path ++ [ 1 ])
                        (Syntax.Base.Exp arg1) ++ getCursorPath
                                                          (path ++ [ 2 ])
                                                          (Syntax.Base.Exp arg2)

                Syntax.Base.Equals arg1 arg2 ->
                    getCursorPath
                        (path ++ [ 1 ])
                        (Syntax.Base.Exp arg1) ++ getCursorPath
                                                          (path ++ [ 2 ])
                                                          (Syntax.Base.Exp arg2)

                Syntax.Base.Hole_cond ->
                    []

                Syntax.Base.Cursor_cond _ ->
                    path

        Syntax.Base.Exp exp ->
            case exp of
                Syntax.Base.Econst arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Syntax.Base.Const arg1)

                Syntax.Base.Eident arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Syntax.Base.Id arg1)

                Syntax.Base.Hole_exp ->
                    []

                Syntax.Base.Cursor_exp _ ->
                    path


getCctxPath : Syntax.CCtx.Cctx -> List Int -> List Int
getCctxPath cctx path =
    case cctx of
        Syntax.CCtx.Cctx_hole ->
            path

        Syntax.CCtx.Select_CLess_cctx1 arg1 arg2 arg3 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Select_CLess_cctx2 arg1 arg2 arg3 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Syntax.CCtx.Select_CLess_cctx3 arg1 arg2 arg3 ->
            getCctxPath arg3 (path ++ [ 3 ])

        Syntax.CCtx.Insert_CLess_cctx1 arg1 (boundVars2, arg2) ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Insert_CLess_cctx2 arg1 (boundVars2, arg2) ->
            getCctxPath arg2 (path ++ [ 2 ])

        Syntax.CCtx.Where_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Having_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Greater_CLess_cctx1 arg1 arg2 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Greater_CLess_cctx2 arg1 arg2 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Syntax.CCtx.Equals_CLess_cctx1 arg1 arg2 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Equals_CLess_cctx2 arg1 arg2 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Syntax.CCtx.Econst_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Syntax.CCtx.Eident_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])


moveCCtxHoleUp :
    Syntax.CCtx.Cctx -> List Int -> Maybe ( Syntax.CCtx.Cctx, Syntax.CCtx.Cctx )
moveCCtxHoleUp cctx path =
    case path of
        [_,_] ->
            case cctx of
                Syntax.CCtx.Cctx_hole ->
                    Nothing

                Syntax.CCtx.Select_CLess_cctx1 arg1 arg2 arg3 ->
                    Just
                        ( Syntax.CCtx.Select_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                            arg2
                            arg3
                        , arg1
                        )

                Syntax.CCtx.Select_CLess_cctx2 arg1 arg2 arg3 ->
                    Just
                        ( Syntax.CCtx.Select_CLess_cctx2
                            arg1
                            Syntax.CCtx.Cctx_hole
                            arg3
                        , arg2
                        )

                Syntax.CCtx.Select_CLess_cctx3 arg1 arg2 arg3 ->
                    Just
                        ( Syntax.CCtx.Select_CLess_cctx3
                            arg1
                            arg2
                            Syntax.CCtx.Cctx_hole
                        , arg3
                        )

                Syntax.CCtx.Insert_CLess_cctx1 arg1 (boundVars2, arg2) ->
                    Just
                        ( Syntax.CCtx.Insert_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                            (boundVars2, arg2)
                        , arg1
                        )

                Syntax.CCtx.Insert_CLess_cctx2 arg1 (boundVars2, arg2) ->
                    Just
                        ( Syntax.CCtx.Insert_CLess_cctx2
                            arg1
                            ( boundVars2, Syntax.CCtx.Cctx_hole )
                        , arg2
                        )

                Syntax.CCtx.Where_CLess_cctx1 arg1 ->
                    Just
                        ( Syntax.CCtx.Where_CLess_cctx1 Syntax.CCtx.Cctx_hole
                        , arg1
                        )

                Syntax.CCtx.Having_CLess_cctx1 arg1 ->
                    Just
                        ( Syntax.CCtx.Having_CLess_cctx1 Syntax.CCtx.Cctx_hole
                        , arg1
                        )

                Syntax.CCtx.Greater_CLess_cctx1 arg1 arg2 ->
                    Just
                        ( Syntax.CCtx.Greater_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                            arg2
                        , arg1
                        )

                Syntax.CCtx.Greater_CLess_cctx2 arg1 arg2 ->
                    Just
                        ( Syntax.CCtx.Greater_CLess_cctx2
                            arg1
                            Syntax.CCtx.Cctx_hole
                        , arg2
                        )

                Syntax.CCtx.Equals_CLess_cctx1 arg1 arg2 ->
                    Just
                        ( Syntax.CCtx.Equals_CLess_cctx1
                            Syntax.CCtx.Cctx_hole
                            arg2
                        , arg1
                        )

                Syntax.CCtx.Equals_CLess_cctx2 arg1 arg2 ->
                    Just
                        ( Syntax.CCtx.Equals_CLess_cctx2
                            arg1
                            Syntax.CCtx.Cctx_hole
                        , arg2
                        )

                Syntax.CCtx.Econst_CLess_cctx1 arg1 ->
                    Just
                        ( Syntax.CCtx.Econst_CLess_cctx1 Syntax.CCtx.Cctx_hole
                        , arg1
                        )

                Syntax.CCtx.Eident_CLess_cctx1 arg1 ->
                    Just
                        ( Syntax.CCtx.Eident_CLess_cctx1 Syntax.CCtx.Cctx_hole
                        , arg1
                        )

        [_] ->
            Just ( Syntax.CCtx.Cctx_hole, cctx )

        _ :: rest ->
            case cctx of
                Syntax.CCtx.Cctx_hole ->
                    Nothing

                Syntax.CCtx.Select_CLess_cctx1 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Select_CLess_cctx1
                                                                     newCctx
                                                                     arg2
                                                                     arg3
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Select_CLess_cctx2 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg2 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Select_CLess_cctx2
                                                                     arg1
                                                                     newCctx
                                                                     arg3
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Select_CLess_cctx3 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg3 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Select_CLess_cctx3
                                                                     arg1
                                                                     arg2
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Insert_CLess_cctx1 arg1 (boundVars2, arg2) ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Insert_CLess_cctx1
                                                                     newCctx
                                                                     (boundVars2, arg2)
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Insert_CLess_cctx2 arg1 (boundVars2, arg2) ->
                    moveCCtxHoleUp arg2 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Insert_CLess_cctx2
                                                                     arg1
                                                                     (boundVars2, newCctx )
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Where_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Where_CLess_cctx1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Having_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Having_CLess_cctx1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Greater_CLess_cctx1 arg1 arg2 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Greater_CLess_cctx1
                                                                     newCctx
                                                                     arg2
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Greater_CLess_cctx2 arg1 arg2 ->
                    moveCCtxHoleUp arg2 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Greater_CLess_cctx2
                                                                     arg1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Equals_CLess_cctx1 arg1 arg2 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Equals_CLess_cctx1
                                                                     newCctx
                                                                     arg2
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Equals_CLess_cctx2 arg1 arg2 ->
                    moveCCtxHoleUp arg2 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Equals_CLess_cctx2
                                                                     arg1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Econst_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Econst_CLess_cctx1
                                                                     newCctx
                                                                 , removedCctx
                                                                 )
                                                        )

                Syntax.CCtx.Eident_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest |> Maybe.map
                                                        (\(newCctx, removedCctx) ->
                                                                 ( Syntax.CCtx.Eident_CLess_cctx1
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

        Syntax.CCtx.Select_CLess_cctx1 arg1 arg2 arg3 ->
            case wellformed of
                Syntax.Wellformed.Root_id_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_q_CLess
                             (Syntax.Cursorless.Select_CLess
                                  underCursor
                                  arg2
                                  arg3
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Select_CLess_cctx2 arg1 arg2 arg3 ->
            case wellformed of
                Syntax.Wellformed.Root_id_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_q_CLess
                             (Syntax.Cursorless.Select_CLess
                                  arg1
                                  underCursor
                                  arg3
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Select_CLess_cctx3 arg1 arg2 arg3 ->
            case wellformed of
                Syntax.Wellformed.Root_clause_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_q_CLess
                             (Syntax.Cursorless.Select_CLess
                                  arg1
                                  arg2
                                  underCursor
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Insert_CLess_cctx1 arg1 (boundVars2, arg2) ->
            case wellformed of
                Syntax.Wellformed.Root_id_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_cmd_CLess
                             (Syntax.Cursorless.Insert_CLess
                                  underCursor
                                  (boundVars2, arg2)
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Insert_CLess_cctx2 arg1 (boundVars2, arg2) ->
            case wellformed of
                Syntax.Wellformed.Root_q_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_cmd_CLess
                             (Syntax.Cursorless.Insert_CLess
                                  arg1
                                  (boundVars2, underCursor )
                             )
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Where_CLess_cctx1 arg1 ->
            case wellformed of
                Syntax.Wellformed.Root_cond_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_clause_CLess
                             (Syntax.Cursorless.Where_CLess underCursor)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Having_CLess_cctx1 arg1 ->
            case wellformed of
                Syntax.Wellformed.Root_cond_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_clause_CLess
                             (Syntax.Cursorless.Having_CLess underCursor)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Greater_CLess_cctx1 arg1 arg2 ->
            case wellformed of
                Syntax.Wellformed.Root_exp_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_cond_CLess
                             (Syntax.Cursorless.Greater_CLess underCursor arg2)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Greater_CLess_cctx2 arg1 arg2 ->
            case wellformed of
                Syntax.Wellformed.Root_exp_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_cond_CLess
                             (Syntax.Cursorless.Greater_CLess arg1 underCursor)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Equals_CLess_cctx1 arg1 arg2 ->
            case wellformed of
                Syntax.Wellformed.Root_exp_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_cond_CLess
                             (Syntax.Cursorless.Equals_CLess underCursor arg2)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Equals_CLess_cctx2 arg1 arg2 ->
            case wellformed of
                Syntax.Wellformed.Root_exp_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_cond_CLess
                             (Syntax.Cursorless.Equals_CLess arg1 underCursor)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Econst_CLess_cctx1 arg1 ->
            case wellformed of
                Syntax.Wellformed.Root_const_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_exp_CLess
                             (Syntax.Cursorless.Econst_CLess underCursor)
                        )

                _ ->
                    Nothing

        Syntax.CCtx.Eident_CLess_cctx1 arg1 ->
            case wellformed of
                Syntax.Wellformed.Root_id_CLess underCursor ->
                    Just
                        (Syntax.Wellformed.Root_exp_CLess
                             (Syntax.Cursorless.Eident_CLess underCursor)
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