module Main exposing (..)


type Q
    = Select Id Id Clause
    | Hole_q
    | Cursor_q Q


type Cmd
    = Insert Id (Bind Id Q)
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


example : Base
example =
    Q (Select (Ident "col-a") (Ident "table-b") (Where (Greater (Eident (Ident "col-a")) (Econst (Num 2)))))


type Q_CLess
    = Select_CLess Id_CLess Id_CLess Clause_CLess
    | Hole_q_CLess


type Cmd_CLess
    = Insert_CLess Id_CLess (Bind Id_CLess Q_CLess)
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


type Cctx
    = Cctx_hole
    | Select_CLess_cctx1 Cctx Id_CLess Clause_CLess
    | Select_CLess_cctx2 Id_CLess Cctx Clause_CLess
    | Select_CLess_cctx3 Id_CLess Id_CLess Cctx
    | Insert_CLess_cctx1 Cctx (Bind Id_CLess Q_CLess)
    | Insert_CLess_cctx2 Id_CLess (Bind Id_CLess Cctx)
    | Where_CLess_cctx1 Cctx
    | Having_CLess_cctx1 Cctx
    | Greater_CLess_cctx1 Cctx Exp_CLess
    | Greater_CLess_cctx2 Exp_CLess Cctx
    | Equals_CLess_cctx1 Cctx Exp_CLess
    | Equals_CLess_cctx2 Exp_CLess Cctx
    | Econst_CLess_cctx1 Cctx
    | Eident_CLess_cctx1 Cctx


type Wellformed
    = Root_q_CLess Q_CLess
    | Root_cmd_CLess Cmd_CLess
    | Root_id_CLess Id_CLess
    | Root_const_CLess Const_CLess
    | Root_clause_CLess Clause_CLess
    | Root_cond_CLess Cond_CLess
    | Root_exp_CLess Exp_CLess


type alias Bind a b =
    ( List a, b )


getCursorPath : List Int -> Base -> List Int
getCursorPath path base =
    case base of
        Q q ->
            case q of
                Select arg1 arg2 arg3 ->
                    (getCursorPath (path ++ [ 1 ]) (Id arg1)
                        ++ getCursorPath
                            (path
                                ++ [ 2
                                   ]
                            )
                            (Id arg2)
                    )
                        ++ getCursorPath (path ++ [ 3 ]) (Clause arg3)

                Hole_q ->
                    []

                Cursor_q _ ->
                    path

        Cmd cmd ->
            case cmd of
                Insert arg1 ( boundVars2, arg2 ) ->
                    getCursorPath (path ++ [ 1 ]) (Id arg1)
                        ++ getCursorPath
                            (path
                                ++ [ 2
                                   ]
                            )
                            (Q arg2)

                Hole_cmd ->
                    []

                Cursor_cmd _ ->
                    path

        Id id ->
            case id of
                Ident lit ->
                    []

                Hole_id ->
                    []

                Cursor_id _ ->
                    path

        Const const ->
            case const of
                Num lit ->
                    []

                Str lit ->
                    []

                Hole_const ->
                    []

                Cursor_const _ ->
                    path

        Clause clause ->
            case clause of
                Where arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Cond arg1)

                Having arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Cond arg1)

                Hole_clause ->
                    []

                Cursor_clause _ ->
                    path

        Cond cond ->
            case cond of
                Greater arg1 arg2 ->
                    getCursorPath (path ++ [ 1 ]) (Exp arg1)
                        ++ getCursorPath
                            (path
                                ++ [ 2
                                   ]
                            )
                            (Exp
                                arg2
                            )

                Equals arg1 arg2 ->
                    getCursorPath (path ++ [ 1 ]) (Exp arg1)
                        ++ getCursorPath
                            (path
                                ++ [ 2
                                   ]
                            )
                            (Exp
                                arg2
                            )

                Hole_cond ->
                    []

                Cursor_cond _ ->
                    path

        Exp exp ->
            case exp of
                Econst arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Const arg1)

                Eident arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Id arg1)

                Hole_exp ->
                    []

                Cursor_exp _ ->
                    path


toCLess_q : Q -> Q_CLess
toCLess_q q =
    case q of
        Select arg1 arg2 arg3 ->
            Select_CLess
                (toCLess_id arg1)
                (toCLess_id arg2)
                (toCLess_clause arg3)

        Hole_q ->
            Hole_q_CLess

        Cursor_q cursor ->
            Debug.todo "Not wellformed"


toCLess_cmd : Cmd -> Cmd_CLess
toCLess_cmd cmd =
    case cmd of
        Insert arg1 ( boundVars2, arg2 ) ->
            Insert_CLess
                (toCLess_id arg1)
                ( List.map toCLess_id boundVars2, toCLess_q arg2 )

        Hole_cmd ->
            Hole_cmd_CLess

        Cursor_cmd cursor ->
            Debug.todo "Not wellformed"


toCLess_id : Id -> Id_CLess
toCLess_id id =
    case id of
        Ident lit ->
            Ident_CLess lit

        Hole_id ->
            Hole_id_CLess

        Cursor_id cursor ->
            Debug.todo "Not wellformed"


toCLess_const : Const -> Const_CLess
toCLess_const const =
    case const of
        Num lit ->
            Num_CLess lit

        Str lit ->
            Str_CLess lit

        Hole_const ->
            Hole_const_CLess

        Cursor_const cursor ->
            Debug.todo "Not wellformed"


toCLess_clause : Clause -> Clause_CLess
toCLess_clause clause =
    case clause of
        Where arg1 ->
            Where_CLess (toCLess_cond arg1)

        Having arg1 ->
            Having_CLess (toCLess_cond arg1)

        Hole_clause ->
            Hole_clause_CLess

        Cursor_clause cursor ->
            Debug.todo "Not wellformed"


toCLess_cond : Cond -> Cond_CLess
toCLess_cond cond =
    case cond of
        Greater arg1 arg2 ->
            Greater_CLess (toCLess_exp arg1) (toCLess_exp arg2)

        Equals arg1 arg2 ->
            Equals_CLess (toCLess_exp arg1) (toCLess_exp arg2)

        Hole_cond ->
            Hole_cond_CLess

        Cursor_cond cursor ->
            Debug.todo "Not wellformed"


toCLess_exp : Exp -> Exp_CLess
toCLess_exp exp =
    case exp of
        Econst arg1 ->
            Econst_CLess (toCLess_const arg1)

        Eident arg1 ->
            Eident_CLess (toCLess_id arg1)

        Hole_exp ->
            Hole_exp_CLess

        Cursor_exp cursor ->
            Debug.todo "Not wellformed"


toCLess : Base -> CursorLess
toCLess base =
    case base of
        Q arg1 ->
            Q_CLess (toCLess_q arg1)

        Cmd arg1 ->
            Cmd_CLess (toCLess_cmd arg1)

        Id arg1 ->
            Id_CLess (toCLess_id arg1)

        Const arg1 ->
            Const_CLess (toCLess_const arg1)

        Clause arg1 ->
            Clause_CLess (toCLess_clause arg1)

        Cond arg1 ->
            Cond_CLess (toCLess_cond arg1)

        Exp arg1 ->
            Exp_CLess (toCLess_exp arg1)


toCCtx_q : Q -> List Int -> ( Cctx, Base )
toCCtx_q q path =
    case path of
        [] ->
            ( Cctx_hole, Q q )

        i :: rest ->
            case q of
                Select arg1 arg2 arg3 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg1 rest
                            in
                            ( Select_CLess_cctx1
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
                            ( Select_CLess_cctx2
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
                            ( Select_CLess_cctx3
                                (toCLess_id arg1)
                                (toCLess_id arg2)
                                cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Hole_q ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cursor_q _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_cmd : Cmd -> List Int -> ( Cctx, Base )
toCCtx_cmd cmd path =
    case path of
        [] ->
            ( Cctx_hole, Cmd cmd )

        i :: rest ->
            case cmd of
                Insert arg1 ( boundVars2, arg2 ) ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg1 rest
                            in
                            ( Insert_CLess_cctx1
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
                            ( Insert_CLess_cctx2
                                (toCLess_id arg1)
                                ( List.map toCLess_id boundVars2, cctxChild )
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Hole_cmd ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cursor_cmd _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_id : Id -> List Int -> ( Cctx, Base )
toCCtx_id id path =
    case path of
        [] ->
            ( Cctx_hole, Id id )

        i :: rest ->
            case id of
                Ident lit ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Hole_id ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cursor_id _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_const : Const -> List Int -> ( Cctx, Base )
toCCtx_const const path =
    case path of
        [] ->
            ( Cctx_hole, Const const )

        i :: rest ->
            case const of
                Num lit ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Str lit ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Hole_const ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cursor_const _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_clause : Clause -> List Int -> ( Cctx, Base )
toCCtx_clause clause path =
    case path of
        [] ->
            ( Cctx_hole, Clause clause )

        i :: rest ->
            case clause of
                Where arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_cond arg1 rest
                            in
                            ( Where_CLess_cctx1 cctxChild, restTree )

                        _ ->
                            Debug.todo "Invalid path"

                Having arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_cond arg1 rest
                            in
                            ( Having_CLess_cctx1 cctxChild, restTree )

                        _ ->
                            Debug.todo "Invalid path"

                Hole_clause ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cursor_clause _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_cond : Cond -> List Int -> ( Cctx, Base )
toCCtx_cond cond path =
    case path of
        [] ->
            ( Cctx_hole, Cond cond )

        i :: rest ->
            case cond of
                Greater arg1 arg2 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_exp arg1 rest
                            in
                            ( Greater_CLess_cctx1 cctxChild (toCLess_exp arg2)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_exp arg2 rest
                            in
                            ( Greater_CLess_cctx2 (toCLess_exp arg1) cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Equals arg1 arg2 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_exp arg1 rest
                            in
                            ( Equals_CLess_cctx1 cctxChild (toCLess_exp arg2)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_exp arg2 rest
                            in
                            ( Equals_CLess_cctx2 (toCLess_exp arg1) cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Hole_cond ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cursor_cond _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_exp : Exp -> List Int -> ( Cctx, Base )
toCCtx_exp exp path =
    case path of
        [] ->
            ( Cctx_hole, Exp exp )

        i :: rest ->
            case exp of
                Econst arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_const arg1 rest
                            in
                            ( Econst_CLess_cctx1 cctxChild, restTree )

                        _ ->
                            Debug.todo "Invalid path"

                Eident arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg1 rest
                            in
                            ( Eident_CLess_cctx1 cctxChild, restTree )

                        _ ->
                            Debug.todo "Invalid path"

                Hole_exp ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cursor_exp _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx : Base -> List Int -> ( Cctx, Base )
toCCtx base path =
    case base of
        Q arg1 ->
            toCCtx_q arg1 path

        Cmd arg1 ->
            toCCtx_cmd arg1 path

        Id arg1 ->
            toCCtx_id arg1 path

        Const arg1 ->
            toCCtx_const arg1 path

        Clause arg1 ->
            toCCtx_clause arg1 path

        Cond arg1 ->
            toCCtx_cond arg1 path

        Exp arg1 ->
            toCCtx_exp arg1 path


toWellformed : Base -> Wellformed
toWellformed base =
    case consumeCursor base of
        Q arg1 ->
            Root_q_CLess (toCLess_q arg1)

        Cmd arg1 ->
            Root_cmd_CLess (toCLess_cmd arg1)

        Id arg1 ->
            Root_id_CLess (toCLess_id arg1)

        Const arg1 ->
            Root_const_CLess (toCLess_const arg1)

        Clause arg1 ->
            Root_clause_CLess (toCLess_clause arg1)

        Cond arg1 ->
            Root_cond_CLess (toCLess_cond arg1)

        Exp arg1 ->
            Root_exp_CLess (toCLess_exp arg1)


consumeCursor : Base -> Base
consumeCursor base =
    case base of
        Q arg1 ->
            case arg1 of
                Cursor_q underCursor ->
                    Q underCursor

                _ ->
                    Q arg1

        Cmd arg1 ->
            case arg1 of
                Cursor_cmd underCursor ->
                    Cmd underCursor

                _ ->
                    Cmd arg1

        Id arg1 ->
            case arg1 of
                Cursor_id underCursor ->
                    Id underCursor

                _ ->
                    Id arg1

        Const arg1 ->
            case arg1 of
                Cursor_const underCursor ->
                    Const underCursor

                _ ->
                    Const arg1

        Clause arg1 ->
            case arg1 of
                Cursor_clause underCursor ->
                    Clause underCursor

                _ ->
                    Clause arg1

        Cond arg1 ->
            case arg1 of
                Cursor_cond underCursor ->
                    Cond underCursor

                _ ->
                    Cond arg1

        Exp arg1 ->
            case arg1 of
                Cursor_exp underCursor ->
                    Exp underCursor

                _ ->
                    Exp arg1


decompose : Base -> ( Cctx, Wellformed )
decompose base =
    let
        ( cctx, rest ) =
            toCCtx base (getCursorPath [] base)
    in
    ( cctx, toWellformed rest )


replaceCctxHole : Int -> Cctx -> CursorLess -> Cctx
replaceCctxHole i orig_cctx underCursor =
    case orig_cctx of
        Cctx_hole ->
            case underCursor of
                Q_CLess underCursor0 ->
                    case underCursor0 of
                        Select_CLess arg1 arg2 arg3 ->
                            case i of
                                1 ->
                                    Select_CLess_cctx1 Cctx_hole arg2 arg3

                                2 ->
                                    Select_CLess_cctx2 arg1 Cctx_hole arg3

                                3 ->
                                    Select_CLess_cctx3 arg1 arg2 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Hole_q_CLess ->
                            Debug.todo "Invalid replacement"

                Cmd_CLess underCursor0 ->
                    case underCursor0 of
                        Insert_CLess arg1 ( boundVars2, arg2 ) ->
                            case i of
                                1 ->
                                    Insert_CLess_cctx1
                                        Cctx_hole
                                        ( boundVars2, arg2 )

                                2 ->
                                    Insert_CLess_cctx2
                                        arg1
                                        ( boundVars2, Cctx_hole )

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Hole_cmd_CLess ->
                            Debug.todo "Invalid replacement"

                Id_CLess underCursor0 ->
                    case underCursor0 of
                        Ident_CLess lit ->
                            Debug.todo "Invalid replacement"

                        Hole_id_CLess ->
                            Debug.todo "Invalid replacement"

                Const_CLess underCursor0 ->
                    case underCursor0 of
                        Num_CLess lit ->
                            Debug.todo "Invalid replacement"

                        Str_CLess lit ->
                            Debug.todo "Invalid replacement"

                        Hole_const_CLess ->
                            Debug.todo "Invalid replacement"

                Clause_CLess underCursor0 ->
                    case underCursor0 of
                        Where_CLess arg1 ->
                            case i of
                                1 ->
                                    Where_CLess_cctx1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Having_CLess arg1 ->
                            case i of
                                1 ->
                                    Having_CLess_cctx1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Hole_clause_CLess ->
                            Debug.todo "Invalid replacement"

                Cond_CLess underCursor0 ->
                    case underCursor0 of
                        Greater_CLess arg1 arg2 ->
                            case i of
                                1 ->
                                    Greater_CLess_cctx1 Cctx_hole arg2

                                2 ->
                                    Greater_CLess_cctx2 arg1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Equals_CLess arg1 arg2 ->
                            case i of
                                1 ->
                                    Equals_CLess_cctx1 Cctx_hole arg2

                                2 ->
                                    Equals_CLess_cctx2 arg1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Hole_cond_CLess ->
                            Debug.todo "Invalid replacement"

                Exp_CLess underCursor0 ->
                    case underCursor0 of
                        Econst_CLess arg1 ->
                            case i of
                                1 ->
                                    Econst_CLess_cctx1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Eident_CLess arg1 ->
                            case i of
                                1 ->
                                    Eident_CLess_cctx1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Hole_exp_CLess ->
                            Debug.todo "Invalid replacement"

        Select_CLess_cctx1 arg1 arg2 arg3 ->
            Select_CLess_cctx1 (replaceCctxHole i arg1 underCursor) arg2 arg3

        Select_CLess_cctx2 arg1 arg2 arg3 ->
            Select_CLess_cctx2 arg1 (replaceCctxHole i arg2 underCursor) arg3

        Select_CLess_cctx3 arg1 arg2 arg3 ->
            Select_CLess_cctx3 arg1 arg2 (replaceCctxHole i arg3 underCursor)

        Insert_CLess_cctx1 arg1 ( boundVars2, arg2 ) ->
            Insert_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                ( boundVars2, arg2 )

        Insert_CLess_cctx2 arg1 ( boundVars2, arg2 ) ->
            Insert_CLess_cctx2
                arg1
                ( boundVars2, replaceCctxHole i arg2 underCursor )

        Where_CLess_cctx1 cctx ->
            Where_CLess_cctx1 (replaceCctxHole i cctx underCursor)

        Having_CLess_cctx1 cctx ->
            Having_CLess_cctx1 (replaceCctxHole i cctx underCursor)

        Greater_CLess_cctx1 arg1 arg2 ->
            Greater_CLess_cctx1 (replaceCctxHole i arg1 underCursor) arg2

        Greater_CLess_cctx2 arg1 arg2 ->
            Greater_CLess_cctx2 arg1 (replaceCctxHole i arg2 underCursor)

        Equals_CLess_cctx1 arg1 arg2 ->
            Equals_CLess_cctx1 (replaceCctxHole i arg1 underCursor) arg2

        Equals_CLess_cctx2 arg1 arg2 ->
            Equals_CLess_cctx2 arg1 (replaceCctxHole i arg2 underCursor)

        Econst_CLess_cctx1 cctx ->
            Econst_CLess_cctx1 (replaceCctxHole i cctx underCursor)

        Eident_CLess_cctx1 cctx ->
            Eident_CLess_cctx1 (replaceCctxHole i cctx underCursor)


child : Int -> ( Cctx, Wellformed ) -> Maybe ( Cctx, Wellformed )
child i decomposed =
    let
        ( cctx, wellformed ) =
            decomposed
    in
    case wellformed of
        Root_q_CLess underCursor ->
            case underCursor of
                Select_CLess arg1 arg2 arg3 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (Q_CLess underCursor)
                                , Root_id_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole i cctx (Q_CLess underCursor)
                                , Root_id_CLess arg2
                                )

                        3 ->
                            Just
                                ( replaceCctxHole i cctx (Q_CLess underCursor)
                                , Root_clause_CLess arg3
                                )

                        _ ->
                            Nothing

                Hole_q_CLess ->
                    Nothing

        Root_cmd_CLess underCursor ->
            case underCursor of
                Insert_CLess arg1 ( boundVars2, arg2 ) ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (Cmd_CLess underCursor)
                                , Root_id_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole i cctx (Cmd_CLess underCursor)
                                , Root_q_CLess arg2
                                )

                        _ ->
                            Nothing

                Hole_cmd_CLess ->
                    Nothing

        Root_id_CLess underCursor ->
            case underCursor of
                Ident_CLess lit ->
                    Nothing

                Hole_id_CLess ->
                    Nothing

        Root_const_CLess underCursor ->
            case underCursor of
                Num_CLess lit ->
                    Nothing

                Str_CLess lit ->
                    Nothing

                Hole_const_CLess ->
                    Nothing

        Root_clause_CLess underCursor ->
            case underCursor of
                Where_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Clause_CLess underCursor)
                                , Root_cond_CLess arg1
                                )

                        _ ->
                            Nothing

                Having_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Clause_CLess underCursor)
                                , Root_cond_CLess arg1
                                )

                        _ ->
                            Nothing

                Hole_clause_CLess ->
                    Nothing

        Root_cond_CLess underCursor ->
            case underCursor of
                Greater_CLess arg1 arg2 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Cond_CLess underCursor)
                                , Root_exp_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Cond_CLess underCursor)
                                , Root_exp_CLess arg2
                                )

                        _ ->
                            Nothing

                Equals_CLess arg1 arg2 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Cond_CLess underCursor)
                                , Root_exp_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Cond_CLess underCursor)
                                , Root_exp_CLess arg2
                                )

                        _ ->
                            Nothing

                Hole_cond_CLess ->
                    Nothing

        Root_exp_CLess underCursor ->
            case underCursor of
                Econst_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (Exp_CLess underCursor)
                                , Root_const_CLess arg1
                                )

                        _ ->
                            Nothing

                Eident_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (Exp_CLess underCursor)
                                , Root_id_CLess arg1
                                )

                        _ ->
                            Nothing

                Hole_exp_CLess ->
                    Nothing


substitute : ( Cctx, Wellformed ) -> CursorLess -> Maybe ( Cctx, Wellformed )
substitute decomposed sub =
    let
        ( cctx, wellformed ) =
            decomposed
    in
    case wellformed of
        Root_q_CLess _ ->
            case sub of
                Q_CLess sub0 ->
                    Just ( cctx, Root_q_CLess sub0 )

                _ ->
                    Nothing

        Root_cmd_CLess _ ->
            case sub of
                Cmd_CLess sub0 ->
                    Just ( cctx, Root_cmd_CLess sub0 )

                _ ->
                    Nothing

        Root_id_CLess _ ->
            case sub of
                Id_CLess sub0 ->
                    Just ( cctx, Root_id_CLess sub0 )

                _ ->
                    Nothing

        Root_const_CLess _ ->
            case sub of
                Const_CLess sub0 ->
                    Just ( cctx, Root_const_CLess sub0 )

                _ ->
                    Nothing

        Root_clause_CLess _ ->
            case sub of
                Clause_CLess sub0 ->
                    Just ( cctx, Root_clause_CLess sub0 )

                _ ->
                    Nothing

        Root_cond_CLess _ ->
            case sub of
                Cond_CLess sub0 ->
                    Just ( cctx, Root_cond_CLess sub0 )

                _ ->
                    Nothing

        Root_exp_CLess _ ->
            case sub of
                Exp_CLess sub0 ->
                    Just ( cctx, Root_exp_CLess sub0 )

                _ ->
                    Nothing


getCctxPath : Cctx -> List Int -> List Int
getCctxPath cctx path =
    case cctx of
        Cctx_hole ->
            path

        Select_CLess_cctx1 arg1 arg2 arg3 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Select_CLess_cctx2 arg1 arg2 arg3 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Select_CLess_cctx3 arg1 arg2 arg3 ->
            getCctxPath arg3 (path ++ [ 3 ])

        Insert_CLess_cctx1 arg1 ( boundVars2, arg2 ) ->
            getCctxPath arg1 (path ++ [ 1 ])

        Insert_CLess_cctx2 arg1 ( boundVars2, arg2 ) ->
            getCctxPath arg2 (path ++ [ 2 ])

        Where_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Having_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Greater_CLess_cctx1 arg1 arg2 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Greater_CLess_cctx2 arg1 arg2 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Equals_CLess_cctx1 arg1 arg2 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Equals_CLess_cctx2 arg1 arg2 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Econst_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Eident_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])


moveCCtxHoleUp : Cctx -> List Int -> Maybe ( Cctx, Cctx )
moveCCtxHoleUp cctx path =
    case path of
        [ _, _ ] ->
            case cctx of
                Cctx_hole ->
                    Nothing

                Select_CLess_cctx1 arg1 arg2 arg3 ->
                    Just ( Select_CLess_cctx1 Cctx_hole arg2 arg3, arg1 )

                Select_CLess_cctx2 arg1 arg2 arg3 ->
                    Just ( Select_CLess_cctx2 arg1 Cctx_hole arg3, arg2 )

                Select_CLess_cctx3 arg1 arg2 arg3 ->
                    Just ( Select_CLess_cctx3 arg1 arg2 Cctx_hole, arg3 )

                Insert_CLess_cctx1 arg1 ( boundVars2, arg2 ) ->
                    Just
                        ( Insert_CLess_cctx1 Cctx_hole ( boundVars2, arg2 )
                        , arg1
                        )

                Insert_CLess_cctx2 arg1 ( boundVars2, arg2 ) ->
                    Just
                        ( Insert_CLess_cctx2 arg1 ( boundVars2, Cctx_hole )
                        , arg2
                        )

                Where_CLess_cctx1 arg1 ->
                    Just ( Where_CLess_cctx1 Cctx_hole, arg1 )

                Having_CLess_cctx1 arg1 ->
                    Just ( Having_CLess_cctx1 Cctx_hole, arg1 )

                Greater_CLess_cctx1 arg1 arg2 ->
                    Just ( Greater_CLess_cctx1 Cctx_hole arg2, arg1 )

                Greater_CLess_cctx2 arg1 arg2 ->
                    Just ( Greater_CLess_cctx2 arg1 Cctx_hole, arg2 )

                Equals_CLess_cctx1 arg1 arg2 ->
                    Just ( Equals_CLess_cctx1 Cctx_hole arg2, arg1 )

                Equals_CLess_cctx2 arg1 arg2 ->
                    Just ( Equals_CLess_cctx2 arg1 Cctx_hole, arg2 )

                Econst_CLess_cctx1 arg1 ->
                    Just ( Econst_CLess_cctx1 Cctx_hole, arg1 )

                Eident_CLess_cctx1 arg1 ->
                    Just ( Eident_CLess_cctx1 Cctx_hole, arg1 )

        [ _ ] ->
            Just ( Cctx_hole, cctx )

        _ :: rest ->
            case cctx of
                Cctx_hole ->
                    Nothing

                Select_CLess_cctx1 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Select_CLess_cctx1
                                    newCctx
                                    arg2
                                    arg3
                                , removedCctx
                                )
                            )

                Select_CLess_cctx2 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg2 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Select_CLess_cctx2
                                    arg1
                                    newCctx
                                    arg3
                                , removedCctx
                                )
                            )

                Select_CLess_cctx3 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg3 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Select_CLess_cctx3
                                    arg1
                                    arg2
                                    newCctx
                                , removedCctx
                                )
                            )

                Insert_CLess_cctx1 arg1 ( boundVars2, arg2 ) ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Insert_CLess_cctx1
                                    newCctx
                                    ( boundVars2, arg2 )
                                , removedCctx
                                )
                            )

                Insert_CLess_cctx2 arg1 ( boundVars2, arg2 ) ->
                    moveCCtxHoleUp arg2 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Insert_CLess_cctx2
                                    arg1
                                    ( boundVars2, newCctx )
                                , removedCctx
                                )
                            )

                Where_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Where_CLess_cctx1
                                    newCctx
                                , removedCctx
                                )
                            )

                Having_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Having_CLess_cctx1
                                    newCctx
                                , removedCctx
                                )
                            )

                Greater_CLess_cctx1 arg1 arg2 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Greater_CLess_cctx1
                                    newCctx
                                    arg2
                                , removedCctx
                                )
                            )

                Greater_CLess_cctx2 arg1 arg2 ->
                    moveCCtxHoleUp arg2 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Greater_CLess_cctx2
                                    arg1
                                    newCctx
                                , removedCctx
                                )
                            )

                Equals_CLess_cctx1 arg1 arg2 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Equals_CLess_cctx1
                                    newCctx
                                    arg2
                                , removedCctx
                                )
                            )

                Equals_CLess_cctx2 arg1 arg2 ->
                    moveCCtxHoleUp arg2 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Equals_CLess_cctx2
                                    arg1
                                    newCctx
                                , removedCctx
                                )
                            )

                Econst_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Econst_CLess_cctx1
                                    newCctx
                                , removedCctx
                                )
                            )

                Eident_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Eident_CLess_cctx1
                                    newCctx
                                , removedCctx
                                )
                            )

        [] ->
            Nothing


addParent : Cctx -> Wellformed -> Maybe Wellformed
addParent cctx wellformed =
    case cctx of
        Cctx_hole ->
            Nothing

        Select_CLess_cctx1 arg1 arg2 arg3 ->
            case wellformed of
                Root_id_CLess underCursor ->
                    Just (Root_q_CLess (Select_CLess underCursor arg2 arg3))

                _ ->
                    Nothing

        Select_CLess_cctx2 arg1 arg2 arg3 ->
            case wellformed of
                Root_id_CLess underCursor ->
                    Just (Root_q_CLess (Select_CLess arg1 underCursor arg3))

                _ ->
                    Nothing

        Select_CLess_cctx3 arg1 arg2 arg3 ->
            case wellformed of
                Root_clause_CLess underCursor ->
                    Just (Root_q_CLess (Select_CLess arg1 arg2 underCursor))

                _ ->
                    Nothing

        Insert_CLess_cctx1 arg1 ( boundVars2, arg2 ) ->
            case wellformed of
                Root_id_CLess underCursor ->
                    Just
                        (Root_cmd_CLess
                            (Insert_CLess underCursor ( boundVars2, arg2 ))
                        )

                _ ->
                    Nothing

        Insert_CLess_cctx2 arg1 ( boundVars2, arg2 ) ->
            case wellformed of
                Root_q_CLess underCursor ->
                    Just
                        (Root_cmd_CLess
                            (Insert_CLess arg1 ( boundVars2, underCursor ))
                        )

                _ ->
                    Nothing

        Where_CLess_cctx1 arg1 ->
            case wellformed of
                Root_cond_CLess underCursor ->
                    Just (Root_clause_CLess (Where_CLess underCursor))

                _ ->
                    Nothing

        Having_CLess_cctx1 arg1 ->
            case wellformed of
                Root_cond_CLess underCursor ->
                    Just (Root_clause_CLess (Having_CLess underCursor))

                _ ->
                    Nothing

        Greater_CLess_cctx1 arg1 arg2 ->
            case wellformed of
                Root_exp_CLess underCursor ->
                    Just (Root_cond_CLess (Greater_CLess underCursor arg2))

                _ ->
                    Nothing

        Greater_CLess_cctx2 arg1 arg2 ->
            case wellformed of
                Root_exp_CLess underCursor ->
                    Just (Root_cond_CLess (Greater_CLess arg1 underCursor))

                _ ->
                    Nothing

        Equals_CLess_cctx1 arg1 arg2 ->
            case wellformed of
                Root_exp_CLess underCursor ->
                    Just (Root_cond_CLess (Equals_CLess underCursor arg2))

                _ ->
                    Nothing

        Equals_CLess_cctx2 arg1 arg2 ->
            case wellformed of
                Root_exp_CLess underCursor ->
                    Just (Root_cond_CLess (Equals_CLess arg1 underCursor))

                _ ->
                    Nothing

        Econst_CLess_cctx1 arg1 ->
            case wellformed of
                Root_const_CLess underCursor ->
                    Just (Root_exp_CLess (Econst_CLess underCursor))

                _ ->
                    Nothing

        Eident_CLess_cctx1 arg1 ->
            case wellformed of
                Root_id_CLess underCursor ->
                    Just (Root_exp_CLess (Eident_CLess underCursor))

                _ ->
                    Nothing


parent : ( Cctx, Wellformed ) -> Maybe ( Cctx, Wellformed )
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
