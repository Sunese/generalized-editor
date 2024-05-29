module Main exposing (..)


type D
    = Latexdoc Id A A (Bind Id C)
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


example : Base
example =
    D (Latexdoc (Ident "article") Hole_a Hole_a ( [ Ident "myenv" ], Cursor_c (TextContent "Hello World!") ))


type D_CLess
    = Latexdoc_CLess Id_CLess A_CLess A_CLess (Bind Id_CLess C_CLess)
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


type Cctx
    = Cctx_hole
    | Latexdoc_CLess_cctx1 Cctx A_CLess A_CLess (Bind Id_CLess C_CLess)
    | Latexdoc_CLess_cctx2 Id_CLess Cctx A_CLess (Bind Id_CLess C_CLess)
    | Latexdoc_CLess_cctx3 Id_CLess A_CLess Cctx (Bind Id_CLess C_CLess)
    | Latexdoc_CLess_cctx4 Id_CLess A_CLess A_CLess (Bind Id_CLess Cctx)
    | Environment_CLess_cctx1 Cctx C_CLess Id_CLess
    | Environment_CLess_cctx2 Id_CLess Cctx Id_CLess
    | Environment_CLess_cctx3 Id_CLess C_CLess Cctx
    | Command_CLess_cctx1 Cctx A_CLess
    | Command_CLess_cctx2 Id_CLess Cctx
    | Argument_CLess_cctx1 Cctx
    | CmdContent_CLess_cctx1 Cctx
    | EnvContent_CLess_cctx1 Cctx
    | SeqContent_CLess_cctx1 Cctx C_CLess
    | SeqContent_CLess_cctx2 C_CLess Cctx


type Wellformed
    = Root_d_CLess D_CLess
    | Root_e_CLess E_CLess
    | Root_cmd_CLess Cmd_CLess
    | Root_a_CLess A_CLess
    | Root_id_CLess Id_CLess
    | Root_c_CLess C_CLess


type alias Bind a b =
    ( List a, b )


getCursorPath : List Int -> Base -> List Int
getCursorPath path base =
    case base of
        D d ->
            case d of
                Latexdoc arg1 arg2 arg3 ( boundVars4, arg4 ) ->
                    ((getCursorPath (path ++ [ 1 ]) (Id arg1)
                        ++ getCursorPath
                            (path
                                ++ [ 2
                                   ]
                            )
                            (A arg2)
                     )
                        ++ getCursorPath (path ++ [ 3 ]) (A arg3)
                    )
                        ++ getCursorPath (path ++ [ 4 ]) (C arg4)

                Hole_d ->
                    []

                Cursor_d _ ->
                    path

        Id id ->
            case id of
                Ident lit ->
                    []

                Hole_id ->
                    []

                Cursor_id _ ->
                    path

        E e ->
            case e of
                Environment arg1 arg2 arg3 ->
                    (getCursorPath (path ++ [ 1 ]) (Id arg1)
                        ++ getCursorPath
                            (path
                                ++ [ 2
                                   ]
                            )
                            (C arg2)
                    )
                        ++ getCursorPath (path ++ [ 3 ]) (Id arg3)

                Hole_e ->
                    []

                Cursor_e _ ->
                    path

        Cmd cmd ->
            case cmd of
                Command arg1 arg2 ->
                    getCursorPath (path ++ [ 1 ]) (Id arg1)
                        ++ getCursorPath
                            (path
                                ++ [ 2
                                   ]
                            )
                            (A arg2)

                Hole_cmd ->
                    []

                Cursor_cmd _ ->
                    path

        A a ->
            case a of
                Argument arg1 ->
                    getCursorPath (path ++ [ 1 ]) (C arg1)

                Hole_a ->
                    []

                Cursor_a _ ->
                    path

        C c ->
            case c of
                TextContent lit ->
                    []

                CmdContent arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Cmd arg1)

                EnvContent arg1 ->
                    getCursorPath (path ++ [ 1 ]) (E arg1)

                SeqContent arg1 arg2 ->
                    getCursorPath (path ++ [ 1 ]) (C arg1)
                        ++ getCursorPath
                            (path
                                ++ [ 2
                                   ]
                            )
                            (C arg2)

                Hole_c ->
                    []

                Cursor_c _ ->
                    path


toCLess_d : D -> D_CLess
toCLess_d d =
    case d of
        Latexdoc arg1 arg2 arg3 ( boundVars4, arg4 ) ->
            Latexdoc_CLess
                (toCLess_id arg1)
                (toCLess_a arg2)
                (toCLess_a arg3)
                ( List.map toCLess_id boundVars4, toCLess_c arg4 )

        Hole_d ->
            Hole_d_CLess

        Cursor_d cursor ->
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


toCLess_e : E -> E_CLess
toCLess_e e =
    case e of
        Environment arg1 arg2 arg3 ->
            Environment_CLess
                (toCLess_id arg1)
                (toCLess_c arg2)
                (toCLess_id arg3)

        Hole_e ->
            Hole_e_CLess

        Cursor_e cursor ->
            Debug.todo "Not wellformed"


toCLess_cmd : Cmd -> Cmd_CLess
toCLess_cmd cmd =
    case cmd of
        Command arg1 arg2 ->
            Command_CLess (toCLess_id arg1) (toCLess_a arg2)

        Hole_cmd ->
            Hole_cmd_CLess

        Cursor_cmd cursor ->
            Debug.todo "Not wellformed"


toCLess_a : A -> A_CLess
toCLess_a a =
    case a of
        Argument arg1 ->
            Argument_CLess (toCLess_c arg1)

        Hole_a ->
            Hole_a_CLess

        Cursor_a cursor ->
            Debug.todo "Not wellformed"


toCLess_c : C -> C_CLess
toCLess_c c =
    case c of
        TextContent lit ->
            TextContent_CLess lit

        CmdContent arg1 ->
            CmdContent_CLess (toCLess_cmd arg1)

        EnvContent arg1 ->
            EnvContent_CLess (toCLess_e arg1)

        SeqContent arg1 arg2 ->
            SeqContent_CLess (toCLess_c arg1) (toCLess_c arg2)

        Hole_c ->
            Hole_c_CLess

        Cursor_c cursor ->
            Debug.todo "Not wellformed"


toCLess : Base -> CursorLess
toCLess base =
    case base of
        D arg1 ->
            D_CLess (toCLess_d arg1)

        Id arg1 ->
            Id_CLess (toCLess_id arg1)

        E arg1 ->
            E_CLess (toCLess_e arg1)

        Cmd arg1 ->
            Cmd_CLess (toCLess_cmd arg1)

        A arg1 ->
            A_CLess (toCLess_a arg1)

        C arg1 ->
            C_CLess (toCLess_c arg1)


toCCtx_d : D -> List Int -> ( Cctx, Base )
toCCtx_d d path =
    case path of
        [] ->
            ( Cctx_hole, D d )

        i :: rest ->
            case d of
                Latexdoc arg1 arg2 arg3 ( boundVars4, arg4 ) ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg1 rest
                            in
                            ( Latexdoc_CLess_cctx1
                                cctxChild
                                (toCLess_a arg2)
                                (toCLess_a arg3)
                                ( List.map toCLess_id boundVars4
                                , toCLess_c arg4
                                )
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_a arg2 rest
                            in
                            ( Latexdoc_CLess_cctx2
                                (toCLess_id arg1)
                                cctxChild
                                (toCLess_a arg3)
                                ( List.map toCLess_id boundVars4
                                , toCLess_c arg4
                                )
                            , restTree
                            )

                        3 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_a arg3 rest
                            in
                            ( Latexdoc_CLess_cctx3
                                (toCLess_id arg1)
                                (toCLess_a arg2)
                                cctxChild
                                ( List.map toCLess_id boundVars4
                                , toCLess_c arg4
                                )
                            , restTree
                            )

                        4 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_c arg4 rest
                            in
                            ( Latexdoc_CLess_cctx4
                                (toCLess_id arg1)
                                (toCLess_a arg2)
                                (toCLess_a arg3)
                                ( List.map toCLess_id boundVars4, cctxChild )
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Hole_d ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cursor_d _ ->
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


toCCtx_e : E -> List Int -> ( Cctx, Base )
toCCtx_e e path =
    case path of
        [] ->
            ( Cctx_hole, E e )

        i :: rest ->
            case e of
                Environment arg1 arg2 arg3 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg1 rest
                            in
                            ( Environment_CLess_cctx1
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
                            ( Environment_CLess_cctx2
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
                            ( Environment_CLess_cctx3
                                (toCLess_id arg1)
                                (toCLess_c arg2)
                                cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Hole_e ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cursor_e _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_cmd : Cmd -> List Int -> ( Cctx, Base )
toCCtx_cmd cmd path =
    case path of
        [] ->
            ( Cctx_hole, Cmd cmd )

        i :: rest ->
            case cmd of
                Command arg1 arg2 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg1 rest
                            in
                            ( Command_CLess_cctx1 cctxChild (toCLess_a arg2)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_a arg2 rest
                            in
                            ( Command_CLess_cctx2 (toCLess_id arg1) cctxChild
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


toCCtx_a : A -> List Int -> ( Cctx, Base )
toCCtx_a a path =
    case path of
        [] ->
            ( Cctx_hole, A a )

        i :: rest ->
            case a of
                Argument arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_c arg1 rest
                            in
                            ( Argument_CLess_cctx1 cctxChild, restTree )

                        _ ->
                            Debug.todo "Invalid path"

                Hole_a ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cursor_a _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_c : C -> List Int -> ( Cctx, Base )
toCCtx_c c path =
    case path of
        [] ->
            ( Cctx_hole, C c )

        i :: rest ->
            case c of
                TextContent lit ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                CmdContent arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_cmd arg1 rest
                            in
                            ( CmdContent_CLess_cctx1 cctxChild, restTree )

                        _ ->
                            Debug.todo "Invalid path"

                EnvContent arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_e arg1 rest
                            in
                            ( EnvContent_CLess_cctx1 cctxChild, restTree )

                        _ ->
                            Debug.todo "Invalid path"

                SeqContent arg1 arg2 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_c arg1 rest
                            in
                            ( SeqContent_CLess_cctx1 cctxChild (toCLess_c arg2)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_c arg2 rest
                            in
                            ( SeqContent_CLess_cctx2 (toCLess_c arg1) cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Hole_c ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cursor_c _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx : Base -> List Int -> ( Cctx, Base )
toCCtx base path =
    case base of
        D arg1 ->
            toCCtx_d arg1 path

        Id arg1 ->
            toCCtx_id arg1 path

        E arg1 ->
            toCCtx_e arg1 path

        Cmd arg1 ->
            toCCtx_cmd arg1 path

        A arg1 ->
            toCCtx_a arg1 path

        C arg1 ->
            toCCtx_c arg1 path


toWellformed : Base -> Wellformed
toWellformed base =
    case consumeCursor base of
        D arg1 ->
            Root_d_CLess (toCLess_d arg1)

        Id arg1 ->
            Root_id_CLess (toCLess_id arg1)

        E arg1 ->
            Root_e_CLess (toCLess_e arg1)

        Cmd arg1 ->
            Root_cmd_CLess (toCLess_cmd arg1)

        A arg1 ->
            Root_a_CLess (toCLess_a arg1)

        C arg1 ->
            Root_c_CLess (toCLess_c arg1)


consumeCursor : Base -> Base
consumeCursor base =
    case base of
        D arg1 ->
            case arg1 of
                Cursor_d underCursor ->
                    D underCursor

                _ ->
                    D arg1

        Id arg1 ->
            case arg1 of
                Cursor_id underCursor ->
                    Id underCursor

                _ ->
                    Id arg1

        E arg1 ->
            case arg1 of
                Cursor_e underCursor ->
                    E underCursor

                _ ->
                    E arg1

        Cmd arg1 ->
            case arg1 of
                Cursor_cmd underCursor ->
                    Cmd underCursor

                _ ->
                    Cmd arg1

        A arg1 ->
            case arg1 of
                Cursor_a underCursor ->
                    A underCursor

                _ ->
                    A arg1

        C arg1 ->
            case arg1 of
                Cursor_c underCursor ->
                    C underCursor

                _ ->
                    C arg1


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
                D_CLess underCursor0 ->
                    case underCursor0 of
                        Latexdoc_CLess arg1 arg2 arg3 ( boundVars4, arg4 ) ->
                            case i of
                                1 ->
                                    Latexdoc_CLess_cctx1
                                        Cctx_hole
                                        arg2
                                        arg3
                                        ( boundVars4, arg4 )

                                2 ->
                                    Latexdoc_CLess_cctx2
                                        arg1
                                        Cctx_hole
                                        arg3
                                        ( boundVars4, arg4 )

                                3 ->
                                    Latexdoc_CLess_cctx3
                                        arg1
                                        arg2
                                        Cctx_hole
                                        ( boundVars4, arg4 )

                                4 ->
                                    Latexdoc_CLess_cctx4
                                        arg1
                                        arg2
                                        arg3
                                        ( boundVars4, Cctx_hole )

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Hole_d_CLess ->
                            Debug.todo "Invalid replacement"

                Id_CLess underCursor0 ->
                    case underCursor0 of
                        Ident_CLess lit ->
                            Debug.todo "Invalid replacement"

                        Hole_id_CLess ->
                            Debug.todo "Invalid replacement"

                E_CLess underCursor0 ->
                    case underCursor0 of
                        Environment_CLess arg1 arg2 arg3 ->
                            case i of
                                1 ->
                                    Environment_CLess_cctx1 Cctx_hole arg2 arg3

                                2 ->
                                    Environment_CLess_cctx2 arg1 Cctx_hole arg3

                                3 ->
                                    Environment_CLess_cctx3 arg1 arg2 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Hole_e_CLess ->
                            Debug.todo "Invalid replacement"

                Cmd_CLess underCursor0 ->
                    case underCursor0 of
                        Command_CLess arg1 arg2 ->
                            case i of
                                1 ->
                                    Command_CLess_cctx1 Cctx_hole arg2

                                2 ->
                                    Command_CLess_cctx2 arg1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Hole_cmd_CLess ->
                            Debug.todo "Invalid replacement"

                A_CLess underCursor0 ->
                    case underCursor0 of
                        Argument_CLess arg1 ->
                            case i of
                                1 ->
                                    Argument_CLess_cctx1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Hole_a_CLess ->
                            Debug.todo "Invalid replacement"

                C_CLess underCursor0 ->
                    case underCursor0 of
                        TextContent_CLess lit ->
                            Debug.todo "Invalid replacement"

                        CmdContent_CLess arg1 ->
                            case i of
                                1 ->
                                    CmdContent_CLess_cctx1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        EnvContent_CLess arg1 ->
                            case i of
                                1 ->
                                    EnvContent_CLess_cctx1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        SeqContent_CLess arg1 arg2 ->
                            case i of
                                1 ->
                                    SeqContent_CLess_cctx1 Cctx_hole arg2

                                2 ->
                                    SeqContent_CLess_cctx2 arg1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Hole_c_CLess ->
                            Debug.todo "Invalid replacement"

        Latexdoc_CLess_cctx1 arg1 arg2 arg3 ( boundVars4, arg4 ) ->
            Latexdoc_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                arg2
                arg3
                ( boundVars4, arg4 )

        Latexdoc_CLess_cctx2 arg1 arg2 arg3 ( boundVars4, arg4 ) ->
            Latexdoc_CLess_cctx2
                arg1
                (replaceCctxHole i arg2 underCursor)
                arg3
                ( boundVars4, arg4 )

        Latexdoc_CLess_cctx3 arg1 arg2 arg3 ( boundVars4, arg4 ) ->
            Latexdoc_CLess_cctx3
                arg1
                arg2
                (replaceCctxHole i arg3 underCursor)
                ( boundVars4, arg4 )

        Latexdoc_CLess_cctx4 arg1 arg2 arg3 ( boundVars4, arg4 ) ->
            Latexdoc_CLess_cctx4
                arg1
                arg2
                arg3
                ( boundVars4, replaceCctxHole i arg4 underCursor )

        Environment_CLess_cctx1 arg1 arg2 arg3 ->
            Environment_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                arg2
                arg3

        Environment_CLess_cctx2 arg1 arg2 arg3 ->
            Environment_CLess_cctx2
                arg1
                (replaceCctxHole i arg2 underCursor)
                arg3

        Environment_CLess_cctx3 arg1 arg2 arg3 ->
            Environment_CLess_cctx3
                arg1
                arg2
                (replaceCctxHole i arg3 underCursor)

        Command_CLess_cctx1 arg1 arg2 ->
            Command_CLess_cctx1 (replaceCctxHole i arg1 underCursor) arg2

        Command_CLess_cctx2 arg1 arg2 ->
            Command_CLess_cctx2 arg1 (replaceCctxHole i arg2 underCursor)

        Argument_CLess_cctx1 cctx ->
            Argument_CLess_cctx1 (replaceCctxHole i cctx underCursor)

        CmdContent_CLess_cctx1 cctx ->
            CmdContent_CLess_cctx1 (replaceCctxHole i cctx underCursor)

        EnvContent_CLess_cctx1 cctx ->
            EnvContent_CLess_cctx1 (replaceCctxHole i cctx underCursor)

        SeqContent_CLess_cctx1 arg1 arg2 ->
            SeqContent_CLess_cctx1 (replaceCctxHole i arg1 underCursor) arg2

        SeqContent_CLess_cctx2 arg1 arg2 ->
            SeqContent_CLess_cctx2 arg1 (replaceCctxHole i arg2 underCursor)


child : Int -> ( Cctx, Wellformed ) -> Maybe ( Cctx, Wellformed )
child i decomposed =
    let
        ( cctx, wellformed ) =
            decomposed
    in
    case wellformed of
        Root_d_CLess underCursor ->
            case underCursor of
                Latexdoc_CLess arg1 arg2 arg3 ( boundVars4, arg4 ) ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (D_CLess underCursor)
                                , Root_id_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole i cctx (D_CLess underCursor)
                                , Root_a_CLess arg2
                                )

                        3 ->
                            Just
                                ( replaceCctxHole i cctx (D_CLess underCursor)
                                , Root_a_CLess arg3
                                )

                        4 ->
                            Just
                                ( replaceCctxHole i cctx (D_CLess underCursor)
                                , Root_c_CLess arg4
                                )

                        _ ->
                            Nothing

                Hole_d_CLess ->
                    Nothing

        Root_e_CLess underCursor ->
            case underCursor of
                Environment_CLess arg1 arg2 arg3 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (E_CLess underCursor)
                                , Root_id_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole i cctx (E_CLess underCursor)
                                , Root_c_CLess arg2
                                )

                        3 ->
                            Just
                                ( replaceCctxHole i cctx (E_CLess underCursor)
                                , Root_id_CLess arg3
                                )

                        _ ->
                            Nothing

                Hole_e_CLess ->
                    Nothing

        Root_cmd_CLess underCursor ->
            case underCursor of
                Command_CLess arg1 arg2 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (Cmd_CLess underCursor)
                                , Root_id_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole i cctx (Cmd_CLess underCursor)
                                , Root_a_CLess arg2
                                )

                        _ ->
                            Nothing

                Hole_cmd_CLess ->
                    Nothing

        Root_a_CLess underCursor ->
            case underCursor of
                Argument_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (A_CLess underCursor)
                                , Root_c_CLess arg1
                                )

                        _ ->
                            Nothing

                Hole_a_CLess ->
                    Nothing

        Root_id_CLess underCursor ->
            case underCursor of
                Ident_CLess lit ->
                    Nothing

                Hole_id_CLess ->
                    Nothing

        Root_c_CLess underCursor ->
            case underCursor of
                TextContent_CLess lit ->
                    Nothing

                CmdContent_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (C_CLess underCursor)
                                , Root_cmd_CLess arg1
                                )

                        _ ->
                            Nothing

                EnvContent_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (C_CLess underCursor)
                                , Root_e_CLess arg1
                                )

                        _ ->
                            Nothing

                SeqContent_CLess arg1 arg2 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (C_CLess underCursor)
                                , Root_c_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole i cctx (C_CLess underCursor)
                                , Root_c_CLess arg2
                                )

                        _ ->
                            Nothing

                Hole_c_CLess ->
                    Nothing


substitute : ( Cctx, Wellformed ) -> CursorLess -> Maybe ( Cctx, Wellformed )
substitute decomposed sub =
    let
        ( cctx, wellformed ) =
            decomposed
    in
    case wellformed of
        Root_d_CLess _ ->
            case sub of
                D_CLess sub0 ->
                    Just ( cctx, Root_d_CLess sub0 )

                _ ->
                    Nothing

        Root_e_CLess _ ->
            case sub of
                E_CLess sub0 ->
                    Just ( cctx, Root_e_CLess sub0 )

                _ ->
                    Nothing

        Root_cmd_CLess _ ->
            case sub of
                Cmd_CLess sub0 ->
                    Just ( cctx, Root_cmd_CLess sub0 )

                _ ->
                    Nothing

        Root_a_CLess _ ->
            case sub of
                A_CLess sub0 ->
                    Just ( cctx, Root_a_CLess sub0 )

                _ ->
                    Nothing

        Root_id_CLess _ ->
            case sub of
                Id_CLess sub0 ->
                    Just ( cctx, Root_id_CLess sub0 )

                _ ->
                    Nothing

        Root_c_CLess _ ->
            case sub of
                C_CLess sub0 ->
                    Just ( cctx, Root_c_CLess sub0 )

                _ ->
                    Nothing


getCctxPath : Cctx -> List Int -> List Int
getCctxPath cctx path =
    case cctx of
        Cctx_hole ->
            path

        Latexdoc_CLess_cctx1 arg1 arg2 arg3 ( boundVars4, arg4 ) ->
            getCctxPath arg1 (path ++ [ 1 ])

        Latexdoc_CLess_cctx2 arg1 arg2 arg3 ( boundVars4, arg4 ) ->
            getCctxPath arg2 (path ++ [ 2 ])

        Latexdoc_CLess_cctx3 arg1 arg2 arg3 ( boundVars4, arg4 ) ->
            getCctxPath arg3 (path ++ [ 3 ])

        Latexdoc_CLess_cctx4 arg1 arg2 arg3 ( boundVars4, arg4 ) ->
            getCctxPath arg4 (path ++ [ 4 ])

        Environment_CLess_cctx1 arg1 arg2 arg3 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Environment_CLess_cctx2 arg1 arg2 arg3 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Environment_CLess_cctx3 arg1 arg2 arg3 ->
            getCctxPath arg3 (path ++ [ 3 ])

        Command_CLess_cctx1 arg1 arg2 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Command_CLess_cctx2 arg1 arg2 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Argument_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        CmdContent_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        EnvContent_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        SeqContent_CLess_cctx1 arg1 arg2 ->
            getCctxPath arg1 (path ++ [ 1 ])

        SeqContent_CLess_cctx2 arg1 arg2 ->
            getCctxPath arg2 (path ++ [ 2 ])


moveCCtxHoleUp : Cctx -> List Int -> Maybe ( Cctx, Cctx )
moveCCtxHoleUp cctx path =
    case path of
        [ _, _ ] ->
            case cctx of
                Cctx_hole ->
                    Nothing

                Latexdoc_CLess_cctx1 arg1 arg2 arg3 ( boundVars4, arg4 ) ->
                    Just
                        ( Latexdoc_CLess_cctx1
                            Cctx_hole
                            arg2
                            arg3
                            ( boundVars4, arg4 )
                        , arg1
                        )

                Latexdoc_CLess_cctx2 arg1 arg2 arg3 ( boundVars4, arg4 ) ->
                    Just
                        ( Latexdoc_CLess_cctx2
                            arg1
                            Cctx_hole
                            arg3
                            ( boundVars4, arg4 )
                        , arg2
                        )

                Latexdoc_CLess_cctx3 arg1 arg2 arg3 ( boundVars4, arg4 ) ->
                    Just
                        ( Latexdoc_CLess_cctx3
                            arg1
                            arg2
                            Cctx_hole
                            ( boundVars4, arg4 )
                        , arg3
                        )

                Latexdoc_CLess_cctx4 arg1 arg2 arg3 ( boundVars4, arg4 ) ->
                    Just
                        ( Latexdoc_CLess_cctx4
                            arg1
                            arg2
                            arg3
                            ( boundVars4, Cctx_hole )
                        , arg4
                        )

                Environment_CLess_cctx1 arg1 arg2 arg3 ->
                    Just ( Environment_CLess_cctx1 Cctx_hole arg2 arg3, arg1 )

                Environment_CLess_cctx2 arg1 arg2 arg3 ->
                    Just ( Environment_CLess_cctx2 arg1 Cctx_hole arg3, arg2 )

                Environment_CLess_cctx3 arg1 arg2 arg3 ->
                    Just ( Environment_CLess_cctx3 arg1 arg2 Cctx_hole, arg3 )

                Command_CLess_cctx1 arg1 arg2 ->
                    Just ( Command_CLess_cctx1 Cctx_hole arg2, arg1 )

                Command_CLess_cctx2 arg1 arg2 ->
                    Just ( Command_CLess_cctx2 arg1 Cctx_hole, arg2 )

                Argument_CLess_cctx1 arg1 ->
                    Just ( Argument_CLess_cctx1 Cctx_hole, arg1 )

                CmdContent_CLess_cctx1 arg1 ->
                    Just ( CmdContent_CLess_cctx1 Cctx_hole, arg1 )

                EnvContent_CLess_cctx1 arg1 ->
                    Just ( EnvContent_CLess_cctx1 Cctx_hole, arg1 )

                SeqContent_CLess_cctx1 arg1 arg2 ->
                    Just ( SeqContent_CLess_cctx1 Cctx_hole arg2, arg1 )

                SeqContent_CLess_cctx2 arg1 arg2 ->
                    Just ( SeqContent_CLess_cctx2 arg1 Cctx_hole, arg2 )

        [ _ ] ->
            Just ( Cctx_hole, cctx )

        _ :: rest ->
            case cctx of
                Cctx_hole ->
                    Nothing

                Latexdoc_CLess_cctx1 arg1 arg2 arg3 ( boundVars4, arg4 ) ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Latexdoc_CLess_cctx1
                                    newCctx
                                    arg2
                                    arg3
                                    ( boundVars4, arg4 )
                                , removedCctx
                                )
                            )

                Latexdoc_CLess_cctx2 arg1 arg2 arg3 ( boundVars4, arg4 ) ->
                    moveCCtxHoleUp arg2 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Latexdoc_CLess_cctx2
                                    arg1
                                    newCctx
                                    arg3
                                    ( boundVars4, arg4 )
                                , removedCctx
                                )
                            )

                Latexdoc_CLess_cctx3 arg1 arg2 arg3 ( boundVars4, arg4 ) ->
                    moveCCtxHoleUp arg3 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Latexdoc_CLess_cctx3
                                    arg1
                                    arg2
                                    newCctx
                                    ( boundVars4, arg4 )
                                , removedCctx
                                )
                            )

                Latexdoc_CLess_cctx4 arg1 arg2 arg3 ( boundVars4, arg4 ) ->
                    moveCCtxHoleUp arg4 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Latexdoc_CLess_cctx4
                                    arg1
                                    arg2
                                    arg3
                                    ( boundVars4, newCctx )
                                , removedCctx
                                )
                            )

                Environment_CLess_cctx1 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Environment_CLess_cctx1
                                    newCctx
                                    arg2
                                    arg3
                                , removedCctx
                                )
                            )

                Environment_CLess_cctx2 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg2 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Environment_CLess_cctx2
                                    arg1
                                    newCctx
                                    arg3
                                , removedCctx
                                )
                            )

                Environment_CLess_cctx3 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg3 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Environment_CLess_cctx3
                                    arg1
                                    arg2
                                    newCctx
                                , removedCctx
                                )
                            )

                Command_CLess_cctx1 arg1 arg2 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Command_CLess_cctx1
                                    newCctx
                                    arg2
                                , removedCctx
                                )
                            )

                Command_CLess_cctx2 arg1 arg2 ->
                    moveCCtxHoleUp arg2 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Command_CLess_cctx2
                                    arg1
                                    newCctx
                                , removedCctx
                                )
                            )

                Argument_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Argument_CLess_cctx1
                                    newCctx
                                , removedCctx
                                )
                            )

                CmdContent_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( CmdContent_CLess_cctx1
                                    newCctx
                                , removedCctx
                                )
                            )

                EnvContent_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( EnvContent_CLess_cctx1
                                    newCctx
                                , removedCctx
                                )
                            )

                SeqContent_CLess_cctx1 arg1 arg2 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( SeqContent_CLess_cctx1
                                    newCctx
                                    arg2
                                , removedCctx
                                )
                            )

                SeqContent_CLess_cctx2 arg1 arg2 ->
                    moveCCtxHoleUp arg2 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( SeqContent_CLess_cctx2
                                    arg1
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

        Latexdoc_CLess_cctx1 arg1 arg2 arg3 ( boundVars4, arg4 ) ->
            case wellformed of
                Root_id_CLess underCursor ->
                    Just
                        (Root_d_CLess
                            (Latexdoc_CLess
                                underCursor
                                arg2
                                arg3
                                ( boundVars4, arg4 )
                            )
                        )

                _ ->
                    Nothing

        Latexdoc_CLess_cctx2 arg1 arg2 arg3 ( boundVars4, arg4 ) ->
            case wellformed of
                Root_a_CLess underCursor ->
                    Just
                        (Root_d_CLess
                            (Latexdoc_CLess
                                arg1
                                underCursor
                                arg3
                                ( boundVars4, arg4 )
                            )
                        )

                _ ->
                    Nothing

        Latexdoc_CLess_cctx3 arg1 arg2 arg3 ( boundVars4, arg4 ) ->
            case wellformed of
                Root_a_CLess underCursor ->
                    Just
                        (Root_d_CLess
                            (Latexdoc_CLess
                                arg1
                                arg2
                                underCursor
                                ( boundVars4, arg4 )
                            )
                        )

                _ ->
                    Nothing

        Latexdoc_CLess_cctx4 arg1 arg2 arg3 ( boundVars4, arg4 ) ->
            case wellformed of
                Root_c_CLess underCursor ->
                    Just
                        (Root_d_CLess
                            (Latexdoc_CLess
                                arg1
                                arg2
                                arg3
                                ( boundVars4, underCursor )
                            )
                        )

                _ ->
                    Nothing

        Environment_CLess_cctx1 arg1 arg2 arg3 ->
            case wellformed of
                Root_id_CLess underCursor ->
                    Just
                        (Root_e_CLess (Environment_CLess underCursor arg2 arg3))

                _ ->
                    Nothing

        Environment_CLess_cctx2 arg1 arg2 arg3 ->
            case wellformed of
                Root_c_CLess underCursor ->
                    Just
                        (Root_e_CLess (Environment_CLess arg1 underCursor arg3))

                _ ->
                    Nothing

        Environment_CLess_cctx3 arg1 arg2 arg3 ->
            case wellformed of
                Root_id_CLess underCursor ->
                    Just
                        (Root_e_CLess (Environment_CLess arg1 arg2 underCursor))

                _ ->
                    Nothing

        Command_CLess_cctx1 arg1 arg2 ->
            case wellformed of
                Root_id_CLess underCursor ->
                    Just (Root_cmd_CLess (Command_CLess underCursor arg2))

                _ ->
                    Nothing

        Command_CLess_cctx2 arg1 arg2 ->
            case wellformed of
                Root_a_CLess underCursor ->
                    Just (Root_cmd_CLess (Command_CLess arg1 underCursor))

                _ ->
                    Nothing

        Argument_CLess_cctx1 arg1 ->
            case wellformed of
                Root_c_CLess underCursor ->
                    Just (Root_a_CLess (Argument_CLess underCursor))

                _ ->
                    Nothing

        CmdContent_CLess_cctx1 arg1 ->
            case wellformed of
                Root_cmd_CLess underCursor ->
                    Just (Root_c_CLess (CmdContent_CLess underCursor))

                _ ->
                    Nothing

        EnvContent_CLess_cctx1 arg1 ->
            case wellformed of
                Root_e_CLess underCursor ->
                    Just (Root_c_CLess (EnvContent_CLess underCursor))

                _ ->
                    Nothing

        SeqContent_CLess_cctx1 arg1 arg2 ->
            case wellformed of
                Root_c_CLess underCursor ->
                    Just (Root_c_CLess (SeqContent_CLess underCursor arg2))

                _ ->
                    Nothing

        SeqContent_CLess_cctx2 arg1 arg2 ->
            case wellformed of
                Root_c_CLess underCursor ->
                    Just (Root_c_CLess (SeqContent_CLess arg1 underCursor))

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
