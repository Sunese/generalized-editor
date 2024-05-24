module Main exposing (..)


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
    = Vardecl T E (Bind Id Bi)
    | Hole_vd
    | Cursor_vd Vd


type Fd
    = Fundecl1 T (Bind Id Fd) T (Bind Id B)
    | Fundecl2 T (Bind Id Fd) T T (Bind Id B)
    | Hole_fd
    | Cursor_fd Fd


type T
    = Tint
    | Tchar
    | Tbool
    | Hole_t
    | Cursor_t T


type Id
    = Ident
    | Hole_id
    | Cursor_id Id


type E
    = Cint
    | Cchar
    | Cbool
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
    = Blockdecls
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


type P_CLess
    = Program_CLess Fd_CLess
    | Hole_p_CLess


type S_CLess
    = Assignment_CLess Id_CLess E_CLess
    | Stmtfuncall_CLess Id_CLess Fa_CLess
    | Return_CLess E_CLess
    | Conditional_CLess Cond_CLess
    | Compstmt_CLess S_CLess S_CLess
    | Hole_s_CLess


type Vd_CLess
    = Vardecl_CLess T_CLess E_CLess (Bind Id_CLess Bi_CLess)
    | Hole_vd_CLess


type Fd_CLess
    = Fundecl1_CLess T_CLess (Bind Id_CLess Fd_CLess) T_CLess (Bind Id_CLess B_CLess)
    | Fundecl2_CLess T_CLess (Bind Id_CLess Fd_CLess) T_CLess T_CLess (Bind Id_CLess B_CLess)
    | Hole_fd_CLess


type T_CLess
    = Tint_CLess
    | Tchar_CLess
    | Tbool_CLess
    | Hole_t_CLess


type Id_CLess
    = Ident_CLess
    | Hole_id_CLess


type E_CLess
    = Cint_CLess
    | Cchar_CLess
    | Cbool_CLess
    | Plus_CLess E_CLess E_CLess
    | Equals_CLess E_CLess E_CLess
    | Expfuncall_CLess Id_CLess Fa_CLess
    | Expident_CLess Id_CLess
    | Hole_e_CLess


type B_CLess
    = Block_CLess Bi_CLess
    | Hole_b_CLess


type Bi_CLess
    = Blockdecls_CLess
    | Blockstmts_CLess S_CLess
    | Blockdone_CLess
    | Hole_bi_CLess


type Fa_CLess
    = Funarg_CLess T_CLess Id_CLess
    | Funargs_CLess T_CLess Id_CLess Fa_CLess
    | Hole_fa_CLess


type Cond_CLess
    = Ifelse_CLess E_CLess B_CLess B_CLess
    | Hole_cond_CLess


type CursorLess
    = P_CLess P_CLess
    | S_CLess S_CLess
    | Vd_CLess Vd_CLess
    | Fd_CLess Fd_CLess
    | T_CLess T_CLess
    | Id_CLess Id_CLess
    | E_CLess E_CLess
    | B_CLess B_CLess
    | Bi_CLess Bi_CLess
    | Fa_CLess Fa_CLess
    | Cond_CLess Cond_CLess


type Cctx
    = Cctx_hole
    | Program_CLess_cctx1 Cctx
    | Block_CLess_cctx1 Cctx
    | Blockstmts_CLess_cctx1 Cctx
    | Vardecl_CLess_cctx1 Cctx E_CLess (Bind Id_CLess Bi_CLess)
    | Vardecl_CLess_cctx2 T_CLess Cctx (Bind Id_CLess Bi_CLess)
    | Vardecl_CLess_cctx3 T_CLess E_CLess (Bind Id_CLess Cctx)
    | Fundecl1_CLess_cctx1 Cctx (Bind Id_CLess Fd_CLess) T_CLess (Bind Id_CLess B_CLess)
    | Fundecl1_CLess_cctx2 T_CLess (Bind Id_CLess Cctx) T_CLess (Bind Id_CLess B_CLess)
    | Fundecl1_CLess_cctx3 T_CLess (Bind Id_CLess Fd_CLess) Cctx (Bind Id_CLess B_CLess)
    | Fundecl1_CLess_cctx4 T_CLess (Bind Id_CLess Fd_CLess) T_CLess (Bind Id_CLess Cctx)
    | Fundecl2_CLess_cctx1 Cctx (Bind Id_CLess Fd_CLess) T_CLess T_CLess (Bind Id_CLess B_CLess)
    | Fundecl2_CLess_cctx2 T_CLess (Bind Id_CLess Cctx) T_CLess T_CLess (Bind Id_CLess B_CLess)
    | Fundecl2_CLess_cctx3 T_CLess (Bind Id_CLess Fd_CLess) Cctx T_CLess (Bind Id_CLess B_CLess)
    | Fundecl2_CLess_cctx4 T_CLess (Bind Id_CLess Fd_CLess) T_CLess Cctx (Bind Id_CLess B_CLess)
    | Fundecl2_CLess_cctx5 T_CLess (Bind Id_CLess Fd_CLess) T_CLess T_CLess (Bind Id_CLess Cctx)
    | Assignment_CLess_cctx1 Cctx E_CLess
    | Assignment_CLess_cctx2 Id_CLess Cctx
    | Stmtfuncall_CLess_cctx1 Cctx Fa_CLess
    | Stmtfuncall_CLess_cctx2 Id_CLess Cctx
    | Return_CLess_cctx1 Cctx
    | Conditional_CLess_cctx1 Cctx
    | Compstmt_CLess_cctx1 Cctx S_CLess
    | Compstmt_CLess_cctx2 S_CLess Cctx
    | Funarg_CLess_cctx1 Cctx Id_CLess
    | Funarg_CLess_cctx2 T_CLess Cctx
    | Funargs_CLess_cctx1 Cctx Id_CLess Fa_CLess
    | Funargs_CLess_cctx2 T_CLess Cctx Fa_CLess
    | Funargs_CLess_cctx3 T_CLess Id_CLess Cctx
    | Ifelse_CLess_cctx1 Cctx B_CLess B_CLess
    | Ifelse_CLess_cctx2 E_CLess Cctx B_CLess
    | Ifelse_CLess_cctx3 E_CLess B_CLess Cctx
    | Plus_CLess_cctx1 Cctx E_CLess
    | Plus_CLess_cctx2 E_CLess Cctx
    | Equals_CLess_cctx1 Cctx E_CLess
    | Equals_CLess_cctx2 E_CLess Cctx
    | Expfuncall_CLess_cctx1 Cctx Fa_CLess
    | Expfuncall_CLess_cctx2 Id_CLess Cctx
    | Expident_CLess_cctx1 Cctx


type Wellformed
    = Root_p_CLess P_CLess
    | Root_s_CLess S_CLess
    | Root_vd_CLess Vd_CLess
    | Root_fd_CLess Fd_CLess
    | Root_t_CLess T_CLess
    | Root_id_CLess Id_CLess
    | Root_e_CLess E_CLess
    | Root_b_CLess B_CLess
    | Root_bi_CLess Bi_CLess
    | Root_fa_CLess Fa_CLess
    | Root_cond_CLess Cond_CLess


type alias Bind a b =
    ( List a, b )


getCursorPath : List Int -> Base -> List Int
getCursorPath path base =
    case base of
        P p ->
            case p of
                Program arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Fd arg1)

                Hole_p ->
                    []

                Cursor_p _ ->
                    path

        B b ->
            case b of
                Block arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Bi arg1)

                Hole_b ->
                    []

                Cursor_b _ ->
                    path

        Bi bi ->
            case bi of
                Blockdecls ->
                    []

                Blockstmts arg1 ->
                    getCursorPath (path ++ [ 1 ]) (S arg1)

                Blockdone ->
                    []

                Hole_bi ->
                    []

                Cursor_bi _ ->
                    path

        Vd vd ->
            case vd of
                Vardecl arg1 arg2 ( boundVars3, arg3 ) ->
                    (getCursorPath
                        (path ++ [ 1 ])
                        (T arg1)
                        ++ getCursorPath
                            (path ++ [ 2 ])
                            (E arg2)
                    )
                        ++ getCursorPath
                            (path ++ [ 3 ])
                            (Bi arg3)

                Hole_vd ->
                    []

                Cursor_vd _ ->
                    path

        Fd fd ->
            case fd of
                Fundecl1 arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
                    ((getCursorPath (path ++ [ 1 ]) (T arg1)
                        ++ getCursorPath
                            (path
                                ++ [ 2
                                   ]
                            )
                            (Fd arg2)
                     )
                        ++ getCursorPath (path ++ [ 3 ]) (T arg3)
                    )
                        ++ getCursorPath (path ++ [ 4 ]) (B arg4)

                Fundecl2 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
                    (((getCursorPath (path ++ [ 1 ]) (T arg1)
                        ++ getCursorPath
                            (path
                                ++ [ 2
                                   ]
                            )
                            (Fd
                                arg2
                            )
                      )
                        ++ getCursorPath (path ++ [ 3 ]) (T arg3)
                     )
                        ++ getCursorPath (path ++ [ 4 ]) (T arg4)
                    )
                        ++ getCursorPath (path ++ [ 5 ]) (B arg5)

                Hole_fd ->
                    []

                Cursor_fd _ ->
                    path

        S s ->
            case s of
                Assignment arg1 arg2 ->
                    getCursorPath (path ++ [ 1 ]) (Id arg1)
                        ++ getCursorPath
                            (path
                                ++ [ 2
                                   ]
                            )
                            (E arg2)

                Stmtfuncall arg1 arg2 ->
                    getCursorPath (path ++ [ 1 ]) (Id arg1)
                        ++ getCursorPath
                            (path
                                ++ [ 2
                                   ]
                            )
                            (Fa arg2)

                Return arg1 ->
                    getCursorPath (path ++ [ 1 ]) (E arg1)

                Conditional arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Cond arg1)

                Compstmt arg1 arg2 ->
                    getCursorPath (path ++ [ 1 ]) (S arg1)
                        ++ getCursorPath
                            (path
                                ++ [ 2
                                   ]
                            )
                            (S arg2)

                Hole_s ->
                    []

                Cursor_s _ ->
                    path

        Fa fa ->
            case fa of
                Funarg arg1 arg2 ->
                    getCursorPath (path ++ [ 1 ]) (T arg1)
                        ++ getCursorPath
                            (path
                                ++ [ 2
                                   ]
                            )
                            (Id arg2)

                Funargs arg1 arg2 arg3 ->
                    (getCursorPath (path ++ [ 1 ]) (T arg1)
                        ++ getCursorPath
                            (path
                                ++ [ 2
                                   ]
                            )
                            (Id arg2)
                    )
                        ++ getCursorPath (path ++ [ 3 ]) (Fa arg3)

                Hole_fa ->
                    []

                Cursor_fa _ ->
                    path

        Cond cond ->
            case cond of
                Ifelse arg1 arg2 arg3 ->
                    (getCursorPath (path ++ [ 1 ]) (E arg1)
                        ++ getCursorPath
                            (path
                                ++ [ 2
                                   ]
                            )
                            (B arg2)
                    )
                        ++ getCursorPath (path ++ [ 3 ]) (B arg3)

                Hole_cond ->
                    []

                Cursor_cond _ ->
                    path

        T t ->
            case t of
                Tint ->
                    []

                Tchar ->
                    []

                Tbool ->
                    []

                Hole_t ->
                    []

                Cursor_t _ ->
                    path

        E e ->
            case e of
                Cint ->
                    []

                Cchar ->
                    []

                Cbool ->
                    []

                Plus arg1 arg2 ->
                    getCursorPath (path ++ [ 1 ]) (E arg1)
                        ++ getCursorPath
                            (path
                                ++ [ 2
                                   ]
                            )
                            (E arg2)

                Equals arg1 arg2 ->
                    getCursorPath (path ++ [ 1 ]) (E arg1)
                        ++ getCursorPath
                            (path
                                ++ [ 2
                                   ]
                            )
                            (E arg2)

                Expfuncall arg1 arg2 ->
                    getCursorPath (path ++ [ 1 ]) (Id arg1)
                        ++ getCursorPath
                            (path
                                ++ [ 2
                                   ]
                            )
                            (Fa arg2)

                Expident arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Id arg1)

                Hole_e ->
                    []

                Cursor_e _ ->
                    path

        Id id ->
            case id of
                Ident ->
                    []

                Hole_id ->
                    []

                Cursor_id _ ->
                    path


toCLess_p : P -> P_CLess
toCLess_p p =
    case p of
        Program arg1 ->
            Program_CLess (toCLess_fd arg1)

        Hole_p ->
            Hole_p_CLess

        Cursor_p cursor ->
            Debug.todo "Not wellformed"


toCLess_b : B -> B_CLess
toCLess_b b =
    case b of
        Block arg1 ->
            Block_CLess (toCLess_bi arg1)

        Hole_b ->
            Hole_b_CLess

        Cursor_b cursor ->
            Debug.todo "Not wellformed"


toCLess_bi : Bi -> Bi_CLess
toCLess_bi bi =
    case bi of
        Blockdecls ->
            Blockdecls_CLess

        Blockstmts arg1 ->
            Blockstmts_CLess (toCLess_s arg1)

        Blockdone ->
            Blockdone_CLess

        Hole_bi ->
            Hole_bi_CLess

        Cursor_bi cursor ->
            Debug.todo "Not wellformed"


toCLess_vd : Vd -> Vd_CLess
toCLess_vd vd =
    case vd of
        Vardecl arg1 arg2 ( boundVars3, arg3 ) ->
            Vardecl_CLess
                (toCLess_t arg1)
                (toCLess_e arg2)
                ( List.map toCLess_id boundVars3, toCLess_bi arg3 )

        Hole_vd ->
            Hole_vd_CLess

        Cursor_vd cursor ->
            Debug.todo "Not wellformed"


toCLess_fd : Fd -> Fd_CLess
toCLess_fd fd =
    case fd of
        Fundecl1 arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
            Fundecl1_CLess
                (toCLess_t arg1)
                ( List.map toCLess_id boundVars2, toCLess_fd arg2 )
                (toCLess_t arg3)
                ( List.map toCLess_id boundVars4, toCLess_b arg4 )

        Fundecl2 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
            Fundecl2_CLess
                (toCLess_t arg1)
                ( List.map toCLess_id boundVars2, toCLess_fd arg2 )
                (toCLess_t arg3)
                (toCLess_t arg4)
                ( List.map toCLess_id boundVars5, toCLess_b arg5 )

        Hole_fd ->
            Hole_fd_CLess

        Cursor_fd cursor ->
            Debug.todo "Not wellformed"


toCLess_s : S -> S_CLess
toCLess_s s =
    case s of
        Assignment arg1 arg2 ->
            Assignment_CLess (toCLess_id arg1) (toCLess_e arg2)

        Stmtfuncall arg1 arg2 ->
            Stmtfuncall_CLess (toCLess_id arg1) (toCLess_fa arg2)

        Return arg1 ->
            Return_CLess (toCLess_e arg1)

        Conditional arg1 ->
            Conditional_CLess (toCLess_cond arg1)

        Compstmt arg1 arg2 ->
            Compstmt_CLess (toCLess_s arg1) (toCLess_s arg2)

        Hole_s ->
            Hole_s_CLess

        Cursor_s cursor ->
            Debug.todo "Not wellformed"


toCLess_fa : Fa -> Fa_CLess
toCLess_fa fa =
    case fa of
        Funarg arg1 arg2 ->
            Funarg_CLess (toCLess_t arg1) (toCLess_id arg2)

        Funargs arg1 arg2 arg3 ->
            Funargs_CLess (toCLess_t arg1) (toCLess_id arg2) (toCLess_fa arg3)

        Hole_fa ->
            Hole_fa_CLess

        Cursor_fa cursor ->
            Debug.todo "Not wellformed"


toCLess_cond : Cond -> Cond_CLess
toCLess_cond cond =
    case cond of
        Ifelse arg1 arg2 arg3 ->
            Ifelse_CLess (toCLess_e arg1) (toCLess_b arg2) (toCLess_b arg3)

        Hole_cond ->
            Hole_cond_CLess

        Cursor_cond cursor ->
            Debug.todo "Not wellformed"


toCLess_t : T -> T_CLess
toCLess_t t =
    case t of
        Tint ->
            Tint_CLess

        Tchar ->
            Tchar_CLess

        Tbool ->
            Tbool_CLess

        Hole_t ->
            Hole_t_CLess

        Cursor_t cursor ->
            Debug.todo "Not wellformed"


toCLess_e : E -> E_CLess
toCLess_e e =
    case e of
        Cint ->
            Cint_CLess

        Cchar ->
            Cchar_CLess

        Cbool ->
            Cbool_CLess

        Plus arg1 arg2 ->
            Plus_CLess (toCLess_e arg1) (toCLess_e arg2)

        Equals arg1 arg2 ->
            Equals_CLess (toCLess_e arg1) (toCLess_e arg2)

        Expfuncall arg1 arg2 ->
            Expfuncall_CLess (toCLess_id arg1) (toCLess_fa arg2)

        Expident arg1 ->
            Expident_CLess (toCLess_id arg1)

        Hole_e ->
            Hole_e_CLess

        Cursor_e cursor ->
            Debug.todo "Not wellformed"


toCLess_id : Id -> Id_CLess
toCLess_id id =
    case id of
        Ident ->
            Ident_CLess

        Hole_id ->
            Hole_id_CLess

        Cursor_id cursor ->
            Debug.todo "Not wellformed"


toCLess : Base -> CursorLess
toCLess base =
    case base of
        P arg1 ->
            P_CLess (toCLess_p arg1)

        B arg1 ->
            B_CLess (toCLess_b arg1)

        Bi arg1 ->
            Bi_CLess (toCLess_bi arg1)

        Vd arg1 ->
            Vd_CLess (toCLess_vd arg1)

        Fd arg1 ->
            Fd_CLess (toCLess_fd arg1)

        S arg1 ->
            S_CLess (toCLess_s arg1)

        Fa arg1 ->
            Fa_CLess (toCLess_fa arg1)

        Cond arg1 ->
            Cond_CLess (toCLess_cond arg1)

        T arg1 ->
            T_CLess (toCLess_t arg1)

        E arg1 ->
            E_CLess (toCLess_e arg1)

        Id arg1 ->
            Id_CLess (toCLess_id arg1)


toCCtx_p : P -> List Int -> ( Cctx, Base )
toCCtx_p p path =
    case path of
        [] ->
            ( Cctx_hole, P p )

        i :: rest ->
            case p of
                Program arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_fd arg1 rest
                            in
                            ( Program_CLess_cctx1 cctxChild, restTree )

                        _ ->
                            Debug.todo "Invalid path"

                Hole_p ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cursor_p _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_b : B -> List Int -> ( Cctx, Base )
toCCtx_b b path =
    case path of
        [] ->
            ( Cctx_hole, B b )

        i :: rest ->
            case b of
                Block arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_bi arg1 rest
                            in
                            ( Block_CLess_cctx1 cctxChild, restTree )

                        _ ->
                            Debug.todo "Invalid path"

                Hole_b ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cursor_b _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_bi : Bi -> List Int -> ( Cctx, Base )
toCCtx_bi bi path =
    case path of
        [] ->
            ( Cctx_hole, Bi bi )

        i :: rest ->
            case bi of
                Blockdecls ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Blockstmts arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_s arg1 rest
                            in
                            ( Blockstmts_CLess_cctx1 cctxChild, restTree )

                        _ ->
                            Debug.todo "Invalid path"

                Blockdone ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Hole_bi ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cursor_bi _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_vd : Vd -> List Int -> ( Cctx, Base )
toCCtx_vd vd path =
    case path of
        [] ->
            ( Cctx_hole, Vd vd )

        i :: rest ->
            case vd of
                Vardecl arg1 arg2 ( boundVars3, arg3 ) ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_t arg1 rest
                            in
                            ( Vardecl_CLess_cctx1
                                cctxChild
                                (toCLess_e arg2)
                                ( List.map toCLess_id boundVars3
                                , toCLess_bi arg3
                                )
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_e arg2 rest
                            in
                            ( Vardecl_CLess_cctx2
                                (toCLess_t arg1)
                                cctxChild
                                ( List.map toCLess_id boundVars3
                                , toCLess_bi arg3
                                )
                            , restTree
                            )

                        3 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_bi arg3 rest
                            in
                            ( Vardecl_CLess_cctx3
                                (toCLess_t arg1)
                                (toCLess_e arg2)
                                ( List.map toCLess_id boundVars3, cctxChild )
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Hole_vd ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cursor_vd _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_fd : Fd -> List Int -> ( Cctx, Base )
toCCtx_fd fd path =
    case path of
        [] ->
            ( Cctx_hole, Fd fd )

        i :: rest ->
            case fd of
                Fundecl1 arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_t arg1 rest
                            in
                            ( Fundecl1_CLess_cctx1
                                cctxChild
                                ( List.map toCLess_id boundVars2
                                , toCLess_fd arg2
                                )
                                (toCLess_t arg3)
                                ( List.map toCLess_id boundVars4
                                , toCLess_b arg4
                                )
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_fd arg2 rest
                            in
                            ( Fundecl1_CLess_cctx2
                                (toCLess_t arg1)
                                ( List.map toCLess_id boundVars2, cctxChild )
                                (toCLess_t arg3)
                                ( List.map toCLess_id boundVars4
                                , toCLess_b arg4
                                )
                            , restTree
                            )

                        3 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_t arg3 rest
                            in
                            ( Fundecl1_CLess_cctx3
                                (toCLess_t arg1)
                                ( List.map toCLess_id boundVars2
                                , toCLess_fd arg2
                                )
                                cctxChild
                                ( List.map toCLess_id boundVars4
                                , toCLess_b arg4
                                )
                            , restTree
                            )

                        4 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_b arg4 rest
                            in
                            ( Fundecl1_CLess_cctx4
                                (toCLess_t arg1)
                                ( List.map toCLess_id boundVars2
                                , toCLess_fd arg2
                                )
                                (toCLess_t arg3)
                                ( List.map toCLess_id boundVars4, cctxChild )
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Fundecl2 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_t arg1 rest
                            in
                            ( Fundecl2_CLess_cctx1
                                cctxChild
                                ( List.map toCLess_id boundVars2
                                , toCLess_fd arg2
                                )
                                (toCLess_t arg3)
                                (toCLess_t arg4)
                                ( List.map toCLess_id boundVars5
                                , toCLess_b arg5
                                )
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_fd arg2 rest
                            in
                            ( Fundecl2_CLess_cctx2
                                (toCLess_t arg1)
                                ( List.map toCLess_id boundVars2, cctxChild )
                                (toCLess_t arg3)
                                (toCLess_t arg4)
                                ( List.map toCLess_id boundVars5
                                , toCLess_b arg5
                                )
                            , restTree
                            )

                        3 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_t arg3 rest
                            in
                            ( Fundecl2_CLess_cctx3
                                (toCLess_t arg1)
                                ( List.map toCLess_id boundVars2
                                , toCLess_fd arg2
                                )
                                cctxChild
                                (toCLess_t arg4)
                                ( List.map toCLess_id boundVars5
                                , toCLess_b arg5
                                )
                            , restTree
                            )

                        4 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_t arg4 rest
                            in
                            ( Fundecl2_CLess_cctx4
                                (toCLess_t arg1)
                                ( List.map toCLess_id boundVars2
                                , toCLess_fd arg2
                                )
                                (toCLess_t arg3)
                                cctxChild
                                ( List.map toCLess_id boundVars5
                                , toCLess_b arg5
                                )
                            , restTree
                            )

                        5 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_b arg5 rest
                            in
                            ( Fundecl2_CLess_cctx5
                                (toCLess_t arg1)
                                ( List.map toCLess_id boundVars2
                                , toCLess_fd arg2
                                )
                                (toCLess_t arg3)
                                (toCLess_t arg4)
                                ( List.map toCLess_id boundVars5, cctxChild )
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Hole_fd ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cursor_fd _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_s : S -> List Int -> ( Cctx, Base )
toCCtx_s s path =
    case path of
        [] ->
            ( Cctx_hole, S s )

        i :: rest ->
            case s of
                Assignment arg1 arg2 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg1 rest
                            in
                            ( Assignment_CLess_cctx1 cctxChild (toCLess_e arg2)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_e arg2 rest
                            in
                            ( Assignment_CLess_cctx2 (toCLess_id arg1) cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Stmtfuncall arg1 arg2 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg1 rest
                            in
                            ( Stmtfuncall_CLess_cctx1
                                cctxChild
                                (toCLess_fa arg2)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_fa arg2 rest
                            in
                            ( Stmtfuncall_CLess_cctx2
                                (toCLess_id arg1)
                                cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Return arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_e arg1 rest
                            in
                            ( Return_CLess_cctx1 cctxChild, restTree )

                        _ ->
                            Debug.todo "Invalid path"

                Conditional arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_cond arg1 rest
                            in
                            ( Conditional_CLess_cctx1 cctxChild, restTree )

                        _ ->
                            Debug.todo "Invalid path"

                Compstmt arg1 arg2 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_s arg1 rest
                            in
                            ( Compstmt_CLess_cctx1 cctxChild (toCLess_s arg2)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_s arg2 rest
                            in
                            ( Compstmt_CLess_cctx2 (toCLess_s arg1) cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Hole_s ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cursor_s _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_fa : Fa -> List Int -> ( Cctx, Base )
toCCtx_fa fa path =
    case path of
        [] ->
            ( Cctx_hole, Fa fa )

        i :: rest ->
            case fa of
                Funarg arg1 arg2 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_t arg1 rest
                            in
                            ( Funarg_CLess_cctx1 cctxChild (toCLess_id arg2)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg2 rest
                            in
                            ( Funarg_CLess_cctx2 (toCLess_t arg1) cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Funargs arg1 arg2 arg3 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_t arg1 rest
                            in
                            ( Funargs_CLess_cctx1
                                cctxChild
                                (toCLess_id arg2)
                                (toCLess_fa arg3)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg2 rest
                            in
                            ( Funargs_CLess_cctx2
                                (toCLess_t arg1)
                                cctxChild
                                (toCLess_fa arg3)
                            , restTree
                            )

                        3 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_fa arg3 rest
                            in
                            ( Funargs_CLess_cctx3
                                (toCLess_t arg1)
                                (toCLess_id arg2)
                                cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Hole_fa ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cursor_fa _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_cond : Cond -> List Int -> ( Cctx, Base )
toCCtx_cond cond path =
    case path of
        [] ->
            ( Cctx_hole, Cond cond )

        i :: rest ->
            case cond of
                Ifelse arg1 arg2 arg3 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_e arg1 rest
                            in
                            ( Ifelse_CLess_cctx1
                                cctxChild
                                (toCLess_b arg2)
                                (toCLess_b arg3)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_b arg2 rest
                            in
                            ( Ifelse_CLess_cctx2
                                (toCLess_e arg1)
                                cctxChild
                                (toCLess_b arg3)
                            , restTree
                            )

                        3 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_b arg3 rest
                            in
                            ( Ifelse_CLess_cctx3
                                (toCLess_e arg1)
                                (toCLess_b arg2)
                                cctxChild
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


toCCtx_t : T -> List Int -> ( Cctx, Base )
toCCtx_t t path =
    case path of
        [] ->
            ( Cctx_hole, T t )

        i :: rest ->
            case t of
                Tint ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Tchar ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Tbool ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Hole_t ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cursor_t _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_e : E -> List Int -> ( Cctx, Base )
toCCtx_e e path =
    case path of
        [] ->
            ( Cctx_hole, E e )

        i :: rest ->
            case e of
                Cint ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cchar ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cbool ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Plus arg1 arg2 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_e arg1 rest
                            in
                            ( Plus_CLess_cctx1 cctxChild (toCLess_e arg2)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_e arg2 rest
                            in
                            ( Plus_CLess_cctx2 (toCLess_e arg1) cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Equals arg1 arg2 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_e arg1 rest
                            in
                            ( Equals_CLess_cctx1 cctxChild (toCLess_e arg2)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_e arg2 rest
                            in
                            ( Equals_CLess_cctx2 (toCLess_e arg1) cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Expfuncall arg1 arg2 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg1 rest
                            in
                            ( Expfuncall_CLess_cctx1 cctxChild (toCLess_fa arg2)
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_fa arg2 rest
                            in
                            ( Expfuncall_CLess_cctx2 (toCLess_id arg1) cctxChild
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Expident arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_id arg1 rest
                            in
                            ( Expident_CLess_cctx1 cctxChild, restTree )

                        _ ->
                            Debug.todo "Invalid path"

                Hole_e ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cursor_e _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_id : Id -> List Int -> ( Cctx, Base )
toCCtx_id id path =
    case path of
        [] ->
            ( Cctx_hole, Id id )

        i :: rest ->
            case id of
                Ident ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Hole_id ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cursor_id _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx : Base -> List Int -> ( Cctx, Base )
toCCtx base path =
    case base of
        P arg1 ->
            toCCtx_p arg1 path

        B arg1 ->
            toCCtx_b arg1 path

        Bi arg1 ->
            toCCtx_bi arg1 path

        Vd arg1 ->
            toCCtx_vd arg1 path

        Fd arg1 ->
            toCCtx_fd arg1 path

        S arg1 ->
            toCCtx_s arg1 path

        Fa arg1 ->
            toCCtx_fa arg1 path

        Cond arg1 ->
            toCCtx_cond arg1 path

        T arg1 ->
            toCCtx_t arg1 path

        E arg1 ->
            toCCtx_e arg1 path

        Id arg1 ->
            toCCtx_id arg1 path


toWellformed : Base -> Wellformed
toWellformed base =
    case consumeCursor base of
        P arg1 ->
            Root_p_CLess (toCLess_p arg1)

        B arg1 ->
            Root_b_CLess (toCLess_b arg1)

        Bi arg1 ->
            Root_bi_CLess (toCLess_bi arg1)

        Vd arg1 ->
            Root_vd_CLess (toCLess_vd arg1)

        Fd arg1 ->
            Root_fd_CLess (toCLess_fd arg1)

        S arg1 ->
            Root_s_CLess (toCLess_s arg1)

        Fa arg1 ->
            Root_fa_CLess (toCLess_fa arg1)

        Cond arg1 ->
            Root_cond_CLess (toCLess_cond arg1)

        T arg1 ->
            Root_t_CLess (toCLess_t arg1)

        E arg1 ->
            Root_e_CLess (toCLess_e arg1)

        Id arg1 ->
            Root_id_CLess (toCLess_id arg1)


consumeCursor : Base -> Base
consumeCursor base =
    case base of
        P arg1 ->
            case arg1 of
                Cursor_p underCursor ->
                    P underCursor

                _ ->
                    P arg1

        B arg1 ->
            case arg1 of
                Cursor_b underCursor ->
                    B underCursor

                _ ->
                    B arg1

        Bi arg1 ->
            case arg1 of
                Cursor_bi underCursor ->
                    Bi underCursor

                _ ->
                    Bi arg1

        Vd arg1 ->
            case arg1 of
                Cursor_vd underCursor ->
                    Vd underCursor

                _ ->
                    Vd arg1

        Fd arg1 ->
            case arg1 of
                Cursor_fd underCursor ->
                    Fd underCursor

                _ ->
                    Fd arg1

        S arg1 ->
            case arg1 of
                Cursor_s underCursor ->
                    S underCursor

                _ ->
                    S arg1

        Fa arg1 ->
            case arg1 of
                Cursor_fa underCursor ->
                    Fa underCursor

                _ ->
                    Fa arg1

        Cond arg1 ->
            case arg1 of
                Cursor_cond underCursor ->
                    Cond underCursor

                _ ->
                    Cond arg1

        T arg1 ->
            case arg1 of
                Cursor_t underCursor ->
                    T underCursor

                _ ->
                    T arg1

        E arg1 ->
            case arg1 of
                Cursor_e underCursor ->
                    E underCursor

                _ ->
                    E arg1

        Id arg1 ->
            case arg1 of
                Cursor_id underCursor ->
                    Id underCursor

                _ ->
                    Id arg1


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
                P_CLess underCursor0 ->
                    case underCursor0 of
                        Program_CLess arg1 ->
                            case i of
                                1 ->
                                    Program_CLess_cctx1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Hole_p_CLess ->
                            Debug.todo "Invalid replacement"

                B_CLess underCursor0 ->
                    case underCursor0 of
                        Block_CLess arg1 ->
                            case i of
                                1 ->
                                    Block_CLess_cctx1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Hole_b_CLess ->
                            Debug.todo "Invalid replacement"

                Bi_CLess underCursor0 ->
                    case underCursor0 of
                        Blockdecls_CLess ->
                            Debug.todo "Invalid replacement"

                        Blockstmts_CLess arg1 ->
                            case i of
                                1 ->
                                    Blockstmts_CLess_cctx1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Blockdone_CLess ->
                            Debug.todo "Invalid replacement"

                        Hole_bi_CLess ->
                            Debug.todo "Invalid replacement"

                Vd_CLess underCursor0 ->
                    case underCursor0 of
                        Vardecl_CLess arg1 arg2 ( boundVars3, arg3 ) ->
                            case i of
                                1 ->
                                    Vardecl_CLess_cctx1
                                        Cctx_hole
                                        arg2
                                        ( boundVars3, arg3 )

                                2 ->
                                    Vardecl_CLess_cctx2
                                        arg1
                                        Cctx_hole
                                        ( boundVars3, arg3 )

                                3 ->
                                    Vardecl_CLess_cctx3
                                        arg1
                                        arg2
                                        ( boundVars3, Cctx_hole )

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Hole_vd_CLess ->
                            Debug.todo "Invalid replacement"

                Fd_CLess underCursor0 ->
                    case underCursor0 of
                        Fundecl1_CLess arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
                            case i of
                                1 ->
                                    Fundecl1_CLess_cctx1
                                        Cctx_hole
                                        ( boundVars2, arg2 )
                                        arg3
                                        ( boundVars4, arg4 )

                                2 ->
                                    Fundecl1_CLess_cctx2
                                        arg1
                                        ( boundVars2, Cctx_hole )
                                        arg3
                                        ( boundVars4, arg4 )

                                3 ->
                                    Fundecl1_CLess_cctx3
                                        arg1
                                        ( boundVars2, arg2 )
                                        Cctx_hole
                                        ( boundVars4, arg4 )

                                4 ->
                                    Fundecl1_CLess_cctx4
                                        arg1
                                        ( boundVars2, arg2 )
                                        arg3
                                        ( boundVars4, Cctx_hole )

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Fundecl2_CLess arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
                            case i of
                                1 ->
                                    Fundecl2_CLess_cctx1
                                        Cctx_hole
                                        ( boundVars2, arg2 )
                                        arg3
                                        arg4
                                        ( boundVars5, arg5 )

                                2 ->
                                    Fundecl2_CLess_cctx2
                                        arg1
                                        ( boundVars2, Cctx_hole )
                                        arg3
                                        arg4
                                        ( boundVars5, arg5 )

                                3 ->
                                    Fundecl2_CLess_cctx3
                                        arg1
                                        ( boundVars2, arg2 )
                                        Cctx_hole
                                        arg4
                                        ( boundVars5, arg5 )

                                4 ->
                                    Fundecl2_CLess_cctx4
                                        arg1
                                        ( boundVars2, arg2 )
                                        arg3
                                        Cctx_hole
                                        ( boundVars5, arg5 )

                                5 ->
                                    Fundecl2_CLess_cctx5
                                        arg1
                                        ( boundVars2, arg2 )
                                        arg3
                                        arg4
                                        ( boundVars5, Cctx_hole )

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Hole_fd_CLess ->
                            Debug.todo "Invalid replacement"

                S_CLess underCursor0 ->
                    case underCursor0 of
                        Assignment_CLess arg1 arg2 ->
                            case i of
                                1 ->
                                    Assignment_CLess_cctx1 Cctx_hole arg2

                                2 ->
                                    Assignment_CLess_cctx2 arg1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Stmtfuncall_CLess arg1 arg2 ->
                            case i of
                                1 ->
                                    Stmtfuncall_CLess_cctx1 Cctx_hole arg2

                                2 ->
                                    Stmtfuncall_CLess_cctx2 arg1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Return_CLess arg1 ->
                            case i of
                                1 ->
                                    Return_CLess_cctx1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Conditional_CLess arg1 ->
                            case i of
                                1 ->
                                    Conditional_CLess_cctx1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Compstmt_CLess arg1 arg2 ->
                            case i of
                                1 ->
                                    Compstmt_CLess_cctx1 Cctx_hole arg2

                                2 ->
                                    Compstmt_CLess_cctx2 arg1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Hole_s_CLess ->
                            Debug.todo "Invalid replacement"

                Fa_CLess underCursor0 ->
                    case underCursor0 of
                        Funarg_CLess arg1 arg2 ->
                            case i of
                                1 ->
                                    Funarg_CLess_cctx1 Cctx_hole arg2

                                2 ->
                                    Funarg_CLess_cctx2 arg1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Funargs_CLess arg1 arg2 arg3 ->
                            case i of
                                1 ->
                                    Funargs_CLess_cctx1 Cctx_hole arg2 arg3

                                2 ->
                                    Funargs_CLess_cctx2 arg1 Cctx_hole arg3

                                3 ->
                                    Funargs_CLess_cctx3 arg1 arg2 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Hole_fa_CLess ->
                            Debug.todo "Invalid replacement"

                Cond_CLess underCursor0 ->
                    case underCursor0 of
                        Ifelse_CLess arg1 arg2 arg3 ->
                            case i of
                                1 ->
                                    Ifelse_CLess_cctx1 Cctx_hole arg2 arg3

                                2 ->
                                    Ifelse_CLess_cctx2 arg1 Cctx_hole arg3

                                3 ->
                                    Ifelse_CLess_cctx3 arg1 arg2 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Hole_cond_CLess ->
                            Debug.todo "Invalid replacement"

                T_CLess underCursor0 ->
                    case underCursor0 of
                        Tint_CLess ->
                            Debug.todo "Invalid replacement"

                        Tchar_CLess ->
                            Debug.todo "Invalid replacement"

                        Tbool_CLess ->
                            Debug.todo "Invalid replacement"

                        Hole_t_CLess ->
                            Debug.todo "Invalid replacement"

                E_CLess underCursor0 ->
                    case underCursor0 of
                        Cint_CLess ->
                            Debug.todo "Invalid replacement"

                        Cchar_CLess ->
                            Debug.todo "Invalid replacement"

                        Cbool_CLess ->
                            Debug.todo "Invalid replacement"

                        Plus_CLess arg1 arg2 ->
                            case i of
                                1 ->
                                    Plus_CLess_cctx1 Cctx_hole arg2

                                2 ->
                                    Plus_CLess_cctx2 arg1 Cctx_hole

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

                        Expfuncall_CLess arg1 arg2 ->
                            case i of
                                1 ->
                                    Expfuncall_CLess_cctx1 Cctx_hole arg2

                                2 ->
                                    Expfuncall_CLess_cctx2 arg1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Expident_CLess arg1 ->
                            case i of
                                1 ->
                                    Expident_CLess_cctx1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Hole_e_CLess ->
                            Debug.todo "Invalid replacement"

                Id_CLess underCursor0 ->
                    case underCursor0 of
                        Ident_CLess ->
                            Debug.todo "Invalid replacement"

                        Hole_id_CLess ->
                            Debug.todo "Invalid replacement"

        Program_CLess_cctx1 cctx ->
            Program_CLess_cctx1 (replaceCctxHole i cctx underCursor)

        Block_CLess_cctx1 cctx ->
            Block_CLess_cctx1 (replaceCctxHole i cctx underCursor)

        Blockstmts_CLess_cctx1 cctx ->
            Blockstmts_CLess_cctx1 (replaceCctxHole i cctx underCursor)

        Vardecl_CLess_cctx1 arg1 arg2 ( boundVars3, arg3 ) ->
            Vardecl_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                arg2
                ( boundVars3, arg3 )

        Vardecl_CLess_cctx2 arg1 arg2 ( boundVars3, arg3 ) ->
            Vardecl_CLess_cctx2
                arg1
                (replaceCctxHole i arg2 underCursor)
                ( boundVars3, arg3 )

        Vardecl_CLess_cctx3 arg1 arg2 ( boundVars3, arg3 ) ->
            Vardecl_CLess_cctx3
                arg1
                arg2
                ( boundVars3, replaceCctxHole i arg3 underCursor )

        Fundecl1_CLess_cctx1 arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
            Fundecl1_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                ( boundVars2, arg2 )
                arg3
                ( boundVars4, arg4 )

        Fundecl1_CLess_cctx2 arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
            Fundecl1_CLess_cctx2
                arg1
                ( boundVars2, replaceCctxHole i arg2 underCursor )
                arg3
                ( boundVars4, arg4 )

        Fundecl1_CLess_cctx3 arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
            Fundecl1_CLess_cctx3
                arg1
                ( boundVars2, arg2 )
                (replaceCctxHole i arg3 underCursor)
                ( boundVars4, arg4 )

        Fundecl1_CLess_cctx4 arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
            Fundecl1_CLess_cctx4
                arg1
                ( boundVars2, arg2 )
                arg3
                ( boundVars4, replaceCctxHole i arg4 underCursor )

        Fundecl2_CLess_cctx1 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
            Fundecl2_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                ( boundVars2, arg2 )
                arg3
                arg4
                ( boundVars5, arg5 )

        Fundecl2_CLess_cctx2 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
            Fundecl2_CLess_cctx2
                arg1
                ( boundVars2, replaceCctxHole i arg2 underCursor )
                arg3
                arg4
                ( boundVars5, arg5 )

        Fundecl2_CLess_cctx3 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
            Fundecl2_CLess_cctx3
                arg1
                ( boundVars2, arg2 )
                (replaceCctxHole i arg3 underCursor)
                arg4
                ( boundVars5, arg5 )

        Fundecl2_CLess_cctx4 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
            Fundecl2_CLess_cctx4
                arg1
                ( boundVars2, arg2 )
                arg3
                (replaceCctxHole i arg4 underCursor)
                ( boundVars5, arg5 )

        Fundecl2_CLess_cctx5 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
            Fundecl2_CLess_cctx5
                arg1
                ( boundVars2, arg2 )
                arg3
                arg4
                ( boundVars5, replaceCctxHole i arg5 underCursor )

        Assignment_CLess_cctx1 arg1 arg2 ->
            Assignment_CLess_cctx1 (replaceCctxHole i arg1 underCursor) arg2

        Assignment_CLess_cctx2 arg1 arg2 ->
            Assignment_CLess_cctx2 arg1 (replaceCctxHole i arg2 underCursor)

        Stmtfuncall_CLess_cctx1 arg1 arg2 ->
            Stmtfuncall_CLess_cctx1 (replaceCctxHole i arg1 underCursor) arg2

        Stmtfuncall_CLess_cctx2 arg1 arg2 ->
            Stmtfuncall_CLess_cctx2 arg1 (replaceCctxHole i arg2 underCursor)

        Return_CLess_cctx1 cctx ->
            Return_CLess_cctx1 (replaceCctxHole i cctx underCursor)

        Conditional_CLess_cctx1 cctx ->
            Conditional_CLess_cctx1 (replaceCctxHole i cctx underCursor)

        Compstmt_CLess_cctx1 arg1 arg2 ->
            Compstmt_CLess_cctx1 (replaceCctxHole i arg1 underCursor) arg2

        Compstmt_CLess_cctx2 arg1 arg2 ->
            Compstmt_CLess_cctx2 arg1 (replaceCctxHole i arg2 underCursor)

        Funarg_CLess_cctx1 arg1 arg2 ->
            Funarg_CLess_cctx1 (replaceCctxHole i arg1 underCursor) arg2

        Funarg_CLess_cctx2 arg1 arg2 ->
            Funarg_CLess_cctx2 arg1 (replaceCctxHole i arg2 underCursor)

        Funargs_CLess_cctx1 arg1 arg2 arg3 ->
            Funargs_CLess_cctx1 (replaceCctxHole i arg1 underCursor) arg2 arg3

        Funargs_CLess_cctx2 arg1 arg2 arg3 ->
            Funargs_CLess_cctx2 arg1 (replaceCctxHole i arg2 underCursor) arg3

        Funargs_CLess_cctx3 arg1 arg2 arg3 ->
            Funargs_CLess_cctx3 arg1 arg2 (replaceCctxHole i arg3 underCursor)

        Ifelse_CLess_cctx1 arg1 arg2 arg3 ->
            Ifelse_CLess_cctx1 (replaceCctxHole i arg1 underCursor) arg2 arg3

        Ifelse_CLess_cctx2 arg1 arg2 arg3 ->
            Ifelse_CLess_cctx2 arg1 (replaceCctxHole i arg2 underCursor) arg3

        Ifelse_CLess_cctx3 arg1 arg2 arg3 ->
            Ifelse_CLess_cctx3 arg1 arg2 (replaceCctxHole i arg3 underCursor)

        Plus_CLess_cctx1 arg1 arg2 ->
            Plus_CLess_cctx1 (replaceCctxHole i arg1 underCursor) arg2

        Plus_CLess_cctx2 arg1 arg2 ->
            Plus_CLess_cctx2 arg1 (replaceCctxHole i arg2 underCursor)

        Equals_CLess_cctx1 arg1 arg2 ->
            Equals_CLess_cctx1 (replaceCctxHole i arg1 underCursor) arg2

        Equals_CLess_cctx2 arg1 arg2 ->
            Equals_CLess_cctx2 arg1 (replaceCctxHole i arg2 underCursor)

        Expfuncall_CLess_cctx1 arg1 arg2 ->
            Expfuncall_CLess_cctx1 (replaceCctxHole i arg1 underCursor) arg2

        Expfuncall_CLess_cctx2 arg1 arg2 ->
            Expfuncall_CLess_cctx2 arg1 (replaceCctxHole i arg2 underCursor)

        Expident_CLess_cctx1 cctx ->
            Expident_CLess_cctx1 (replaceCctxHole i cctx underCursor)


child : Int -> ( Cctx, Wellformed ) -> Maybe ( Cctx, Wellformed )
child i decomposed =
    let
        ( cctx, wellformed ) =
            decomposed
    in
    case wellformed of
        Root_p_CLess underCursor ->
            case underCursor of
                Program_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (P_CLess underCursor)
                                , Root_fd_CLess arg1
                                )

                        _ ->
                            Nothing

                Hole_p_CLess ->
                    Nothing

        Root_s_CLess underCursor ->
            case underCursor of
                Assignment_CLess arg1 arg2 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (S_CLess underCursor)
                                , Root_id_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole i cctx (S_CLess underCursor)
                                , Root_e_CLess arg2
                                )

                        _ ->
                            Nothing

                Stmtfuncall_CLess arg1 arg2 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (S_CLess underCursor)
                                , Root_id_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole i cctx (S_CLess underCursor)
                                , Root_fa_CLess arg2
                                )

                        _ ->
                            Nothing

                Return_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (S_CLess underCursor)
                                , Root_e_CLess arg1
                                )

                        _ ->
                            Nothing

                Conditional_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (S_CLess underCursor)
                                , Root_cond_CLess arg1
                                )

                        _ ->
                            Nothing

                Compstmt_CLess arg1 arg2 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (S_CLess underCursor)
                                , Root_s_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole i cctx (S_CLess underCursor)
                                , Root_s_CLess arg2
                                )

                        _ ->
                            Nothing

                Hole_s_CLess ->
                    Nothing

        Root_vd_CLess underCursor ->
            case underCursor of
                Vardecl_CLess arg1 arg2 ( boundVars3, arg3 ) ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (Vd_CLess underCursor)
                                , Root_t_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole i cctx (Vd_CLess underCursor)
                                , Root_e_CLess arg2
                                )

                        3 ->
                            Just
                                ( replaceCctxHole i cctx (Vd_CLess underCursor)
                                , Root_bi_CLess arg3
                                )

                        _ ->
                            Nothing

                Hole_vd_CLess ->
                    Nothing

        Root_fd_CLess underCursor ->
            case underCursor of
                Fundecl1_CLess arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (Fd_CLess underCursor)
                                , Root_t_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole i cctx (Fd_CLess underCursor)
                                , Root_fd_CLess arg2
                                )

                        3 ->
                            Just
                                ( replaceCctxHole i cctx (Fd_CLess underCursor)
                                , Root_t_CLess arg3
                                )

                        4 ->
                            Just
                                ( replaceCctxHole i cctx (Fd_CLess underCursor)
                                , Root_b_CLess arg4
                                )

                        _ ->
                            Nothing

                Fundecl2_CLess arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (Fd_CLess underCursor)
                                , Root_t_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole i cctx (Fd_CLess underCursor)
                                , Root_fd_CLess arg2
                                )

                        3 ->
                            Just
                                ( replaceCctxHole i cctx (Fd_CLess underCursor)
                                , Root_t_CLess arg3
                                )

                        4 ->
                            Just
                                ( replaceCctxHole i cctx (Fd_CLess underCursor)
                                , Root_t_CLess arg4
                                )

                        5 ->
                            Just
                                ( replaceCctxHole i cctx (Fd_CLess underCursor)
                                , Root_b_CLess arg5
                                )

                        _ ->
                            Nothing

                Hole_fd_CLess ->
                    Nothing

        Root_t_CLess underCursor ->
            case underCursor of
                Tint_CLess ->
                    Nothing

                Tchar_CLess ->
                    Nothing

                Tbool_CLess ->
                    Nothing

                Hole_t_CLess ->
                    Nothing

        Root_id_CLess underCursor ->
            case underCursor of
                Ident_CLess ->
                    Nothing

                Hole_id_CLess ->
                    Nothing

        Root_e_CLess underCursor ->
            case underCursor of
                Cint_CLess ->
                    Nothing

                Cchar_CLess ->
                    Nothing

                Cbool_CLess ->
                    Nothing

                Plus_CLess arg1 arg2 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (E_CLess underCursor)
                                , Root_e_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole i cctx (E_CLess underCursor)
                                , Root_e_CLess arg2
                                )

                        _ ->
                            Nothing

                Equals_CLess arg1 arg2 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (E_CLess underCursor)
                                , Root_e_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole i cctx (E_CLess underCursor)
                                , Root_e_CLess arg2
                                )

                        _ ->
                            Nothing

                Expfuncall_CLess arg1 arg2 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (E_CLess underCursor)
                                , Root_id_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole i cctx (E_CLess underCursor)
                                , Root_fa_CLess arg2
                                )

                        _ ->
                            Nothing

                Expident_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (E_CLess underCursor)
                                , Root_id_CLess arg1
                                )

                        _ ->
                            Nothing

                Hole_e_CLess ->
                    Nothing

        Root_b_CLess underCursor ->
            case underCursor of
                Block_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (B_CLess underCursor)
                                , Root_bi_CLess arg1
                                )

                        _ ->
                            Nothing

                Hole_b_CLess ->
                    Nothing

        Root_bi_CLess underCursor ->
            case underCursor of
                Blockdecls_CLess ->
                    Nothing

                Blockstmts_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (Bi_CLess underCursor)
                                , Root_s_CLess arg1
                                )

                        _ ->
                            Nothing

                Blockdone_CLess ->
                    Nothing

                Hole_bi_CLess ->
                    Nothing

        Root_fa_CLess underCursor ->
            case underCursor of
                Funarg_CLess arg1 arg2 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (Fa_CLess underCursor)
                                , Root_t_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole i cctx (Fa_CLess underCursor)
                                , Root_id_CLess arg2
                                )

                        _ ->
                            Nothing

                Funargs_CLess arg1 arg2 arg3 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (Fa_CLess underCursor)
                                , Root_t_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole i cctx (Fa_CLess underCursor)
                                , Root_id_CLess arg2
                                )

                        3 ->
                            Just
                                ( replaceCctxHole i cctx (Fa_CLess underCursor)
                                , Root_fa_CLess arg3
                                )

                        _ ->
                            Nothing

                Hole_fa_CLess ->
                    Nothing

        Root_cond_CLess underCursor ->
            case underCursor of
                Ifelse_CLess arg1 arg2 arg3 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Cond_CLess underCursor)
                                , Root_e_CLess arg1
                                )

                        2 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Cond_CLess underCursor)
                                , Root_b_CLess arg2
                                )

                        3 ->
                            Just
                                ( replaceCctxHole
                                    i
                                    cctx
                                    (Cond_CLess underCursor)
                                , Root_b_CLess arg3
                                )

                        _ ->
                            Nothing

                Hole_cond_CLess ->
                    Nothing
