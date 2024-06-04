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
    | Fundecldone_CLess
    | Hole_fd_CLess


type T_CLess
    = Tint_CLess
    | Tchar_CLess
    | Tbool_CLess
    | Hole_t_CLess


type Id_CLess
    = Ident_CLess String
    | Hole_id_CLess


type E_CLess
    = Int_CLess Int
    | Char_CLess Char
    | Bool_CLess Bool
    | Plus_CLess E_CLess E_CLess
    | Equals_CLess E_CLess E_CLess
    | Expfuncall_CLess Id_CLess Fa_CLess
    | Expident_CLess Id_CLess
    | Hole_e_CLess


type B_CLess
    = Block_CLess Bi_CLess
    | Hole_b_CLess


type Bi_CLess
    = Blockdecls_CLess Vd_CLess
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
    | Blockdecls_CLess_cctx1 Cctx
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
                Blockdecls arg1 ->
                    getCursorPath (path ++ [ 1 ]) (Vd arg1)

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
                    (getCursorPath (path ++ [ 1 ]) (T arg1)
                        ++ getCursorPath
                            (path
                                ++ [ 2
                                   ]
                            )
                            (E arg2)
                    )
                        ++ getCursorPath (path ++ [ 3 ]) (Bi arg3)

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

                Fundecldone ->
                    []

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
                Int lit ->
                    []

                Char lit ->
                    []

                Bool lit ->
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
                Ident lit ->
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
        Blockdecls arg1 ->
            Blockdecls_CLess (toCLess_vd arg1)

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

        Fundecldone ->
            Fundecldone_CLess

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
        Int lit ->
            Int_CLess lit

        Char lit ->
            Char_CLess lit

        Bool lit ->
            Bool_CLess lit

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
        Ident lit ->
            Ident_CLess lit

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
                Blockdecls arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_vd arg1 rest
                            in
                            ( Blockdecls_CLess_cctx1 cctxChild, restTree )

                        _ ->
                            Debug.todo "Invalid path"

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

                Fundecldone ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

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
                Int lit ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Char lit ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Bool lit ->
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
                Ident lit ->
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
                        Blockdecls_CLess arg1 ->
                            case i of
                                1 ->
                                    Blockdecls_CLess_cctx1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

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

                        Fundecldone_CLess ->
                            Debug.todo "Invalid replacement"

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
                        Int_CLess lit ->
                            Debug.todo "Invalid replacement"

                        Char_CLess lit ->
                            Debug.todo "Invalid replacement"

                        Bool_CLess lit ->
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
                        Ident_CLess lit ->
                            Debug.todo "Invalid replacement"

                        Hole_id_CLess ->
                            Debug.todo "Invalid replacement"

        Program_CLess_cctx1 cctx ->
            Program_CLess_cctx1 (replaceCctxHole i cctx underCursor)

        Block_CLess_cctx1 cctx ->
            Block_CLess_cctx1 (replaceCctxHole i cctx underCursor)

        Blockdecls_CLess_cctx1 cctx ->
            Blockdecls_CLess_cctx1 (replaceCctxHole i cctx underCursor)

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

                Fundecldone_CLess ->
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
                Ident_CLess lit ->
                    Nothing

                Hole_id_CLess ->
                    Nothing

        Root_e_CLess underCursor ->
            case underCursor of
                Int_CLess lit ->
                    Nothing

                Char_CLess lit ->
                    Nothing

                Bool_CLess lit ->
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
                Blockdecls_CLess arg1 ->
                    case i of
                        1 ->
                            Just
                                ( replaceCctxHole i cctx (Bi_CLess underCursor)
                                , Root_vd_CLess arg1
                                )

                        _ ->
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


substitute : ( Cctx, Wellformed ) -> CursorLess -> Maybe ( Cctx, Wellformed )
substitute decomposed sub =
    let
        ( cctx, wellformed ) =
            decomposed
    in
    case wellformed of
        Root_p_CLess _ ->
            case sub of
                P_CLess sub0 ->
                    Just ( cctx, Root_p_CLess sub0 )

                _ ->
                    Nothing

        Root_s_CLess _ ->
            case sub of
                S_CLess sub0 ->
                    Just ( cctx, Root_s_CLess sub0 )

                _ ->
                    Nothing

        Root_vd_CLess _ ->
            case sub of
                Vd_CLess sub0 ->
                    Just ( cctx, Root_vd_CLess sub0 )

                _ ->
                    Nothing

        Root_fd_CLess _ ->
            case sub of
                Fd_CLess sub0 ->
                    Just ( cctx, Root_fd_CLess sub0 )

                _ ->
                    Nothing

        Root_t_CLess _ ->
            case sub of
                T_CLess sub0 ->
                    Just ( cctx, Root_t_CLess sub0 )

                _ ->
                    Nothing

        Root_id_CLess _ ->
            case sub of
                Id_CLess sub0 ->
                    Just ( cctx, Root_id_CLess sub0 )

                _ ->
                    Nothing

        Root_e_CLess _ ->
            case sub of
                E_CLess sub0 ->
                    Just ( cctx, Root_e_CLess sub0 )

                _ ->
                    Nothing

        Root_b_CLess _ ->
            case sub of
                B_CLess sub0 ->
                    Just ( cctx, Root_b_CLess sub0 )

                _ ->
                    Nothing

        Root_bi_CLess _ ->
            case sub of
                Bi_CLess sub0 ->
                    Just ( cctx, Root_bi_CLess sub0 )

                _ ->
                    Nothing

        Root_fa_CLess _ ->
            case sub of
                Fa_CLess sub0 ->
                    Just ( cctx, Root_fa_CLess sub0 )

                _ ->
                    Nothing

        Root_cond_CLess _ ->
            case sub of
                Cond_CLess sub0 ->
                    Just ( cctx, Root_cond_CLess sub0 )

                _ ->
                    Nothing


getCctxPath : Cctx -> List Int -> List Int
getCctxPath cctx path =
    case cctx of
        Cctx_hole ->
            path

        Program_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Block_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Blockdecls_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Blockstmts_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Vardecl_CLess_cctx1 arg1 arg2 ( boundVars3, arg3 ) ->
            getCctxPath arg1 (path ++ [ 1 ])

        Vardecl_CLess_cctx2 arg1 arg2 ( boundVars3, arg3 ) ->
            getCctxPath arg2 (path ++ [ 2 ])

        Vardecl_CLess_cctx3 arg1 arg2 ( boundVars3, arg3 ) ->
            getCctxPath arg3 (path ++ [ 3 ])

        Fundecl1_CLess_cctx1 arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
            getCctxPath arg1 (path ++ [ 1 ])

        Fundecl1_CLess_cctx2 arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
            getCctxPath arg2 (path ++ [ 2 ])

        Fundecl1_CLess_cctx3 arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
            getCctxPath arg3 (path ++ [ 3 ])

        Fundecl1_CLess_cctx4 arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
            getCctxPath arg4 (path ++ [ 4 ])

        Fundecl2_CLess_cctx1 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
            getCctxPath arg1 (path ++ [ 1 ])

        Fundecl2_CLess_cctx2 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
            getCctxPath arg2 (path ++ [ 2 ])

        Fundecl2_CLess_cctx3 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
            getCctxPath arg3 (path ++ [ 3 ])

        Fundecl2_CLess_cctx4 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
            getCctxPath arg4 (path ++ [ 4 ])

        Fundecl2_CLess_cctx5 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
            getCctxPath arg5 (path ++ [ 5 ])

        Assignment_CLess_cctx1 arg1 arg2 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Assignment_CLess_cctx2 arg1 arg2 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Stmtfuncall_CLess_cctx1 arg1 arg2 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Stmtfuncall_CLess_cctx2 arg1 arg2 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Return_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Conditional_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Compstmt_CLess_cctx1 arg1 arg2 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Compstmt_CLess_cctx2 arg1 arg2 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Funarg_CLess_cctx1 arg1 arg2 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Funarg_CLess_cctx2 arg1 arg2 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Funargs_CLess_cctx1 arg1 arg2 arg3 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Funargs_CLess_cctx2 arg1 arg2 arg3 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Funargs_CLess_cctx3 arg1 arg2 arg3 ->
            getCctxPath arg3 (path ++ [ 3 ])

        Ifelse_CLess_cctx1 arg1 arg2 arg3 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Ifelse_CLess_cctx2 arg1 arg2 arg3 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Ifelse_CLess_cctx3 arg1 arg2 arg3 ->
            getCctxPath arg3 (path ++ [ 3 ])

        Plus_CLess_cctx1 arg1 arg2 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Plus_CLess_cctx2 arg1 arg2 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Equals_CLess_cctx1 arg1 arg2 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Equals_CLess_cctx2 arg1 arg2 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Expfuncall_CLess_cctx1 arg1 arg2 ->
            getCctxPath arg1 (path ++ [ 1 ])

        Expfuncall_CLess_cctx2 arg1 arg2 ->
            getCctxPath arg2 (path ++ [ 2 ])

        Expident_CLess_cctx1 arg1 ->
            getCctxPath arg1 (path ++ [ 1 ])


moveCCtxHoleUp : Cctx -> List Int -> Maybe ( Cctx, Cctx )
moveCCtxHoleUp cctx path =
    case path of
        [ _, _ ] ->
            case cctx of
                Cctx_hole ->
                    Nothing

                Program_CLess_cctx1 arg1 ->
                    Just ( Program_CLess_cctx1 Cctx_hole, arg1 )

                Block_CLess_cctx1 arg1 ->
                    Just ( Block_CLess_cctx1 Cctx_hole, arg1 )

                Blockdecls_CLess_cctx1 arg1 ->
                    Just ( Blockdecls_CLess_cctx1 Cctx_hole, arg1 )

                Blockstmts_CLess_cctx1 arg1 ->
                    Just ( Blockstmts_CLess_cctx1 Cctx_hole, arg1 )

                Vardecl_CLess_cctx1 arg1 arg2 ( boundVars3, arg3 ) ->
                    Just
                        ( Vardecl_CLess_cctx1 Cctx_hole arg2 ( boundVars3, arg3 )
                        , arg1
                        )

                Vardecl_CLess_cctx2 arg1 arg2 ( boundVars3, arg3 ) ->
                    Just
                        ( Vardecl_CLess_cctx2 arg1 Cctx_hole ( boundVars3, arg3 )
                        , arg2
                        )

                Vardecl_CLess_cctx3 arg1 arg2 ( boundVars3, arg3 ) ->
                    Just
                        ( Vardecl_CLess_cctx3 arg1 arg2 ( boundVars3, Cctx_hole )
                        , arg3
                        )

                Fundecl1_CLess_cctx1 arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
                    Just
                        ( Fundecl1_CLess_cctx1
                            Cctx_hole
                            ( boundVars2, arg2 )
                            arg3
                            ( boundVars4, arg4 )
                        , arg1
                        )

                Fundecl1_CLess_cctx2 arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
                    Just
                        ( Fundecl1_CLess_cctx2
                            arg1
                            ( boundVars2, Cctx_hole )
                            arg3
                            ( boundVars4, arg4 )
                        , arg2
                        )

                Fundecl1_CLess_cctx3 arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
                    Just
                        ( Fundecl1_CLess_cctx3
                            arg1
                            ( boundVars2, arg2 )
                            Cctx_hole
                            ( boundVars4, arg4 )
                        , arg3
                        )

                Fundecl1_CLess_cctx4 arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
                    Just
                        ( Fundecl1_CLess_cctx4
                            arg1
                            ( boundVars2, arg2 )
                            arg3
                            ( boundVars4, Cctx_hole )
                        , arg4
                        )

                Fundecl2_CLess_cctx1 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
                    Just
                        ( Fundecl2_CLess_cctx1
                            Cctx_hole
                            ( boundVars2, arg2 )
                            arg3
                            arg4
                            ( boundVars5, arg5 )
                        , arg1
                        )

                Fundecl2_CLess_cctx2 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
                    Just
                        ( Fundecl2_CLess_cctx2
                            arg1
                            ( boundVars2, Cctx_hole )
                            arg3
                            arg4
                            ( boundVars5, arg5 )
                        , arg2
                        )

                Fundecl2_CLess_cctx3 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
                    Just
                        ( Fundecl2_CLess_cctx3
                            arg1
                            ( boundVars2, arg2 )
                            Cctx_hole
                            arg4
                            ( boundVars5, arg5 )
                        , arg3
                        )

                Fundecl2_CLess_cctx4 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
                    Just
                        ( Fundecl2_CLess_cctx4
                            arg1
                            ( boundVars2, arg2 )
                            arg3
                            Cctx_hole
                            ( boundVars5, arg5 )
                        , arg4
                        )

                Fundecl2_CLess_cctx5 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
                    Just
                        ( Fundecl2_CLess_cctx5
                            arg1
                            ( boundVars2, arg2 )
                            arg3
                            arg4
                            ( boundVars5, Cctx_hole )
                        , arg5
                        )

                Assignment_CLess_cctx1 arg1 arg2 ->
                    Just ( Assignment_CLess_cctx1 Cctx_hole arg2, arg1 )

                Assignment_CLess_cctx2 arg1 arg2 ->
                    Just ( Assignment_CLess_cctx2 arg1 Cctx_hole, arg2 )

                Stmtfuncall_CLess_cctx1 arg1 arg2 ->
                    Just ( Stmtfuncall_CLess_cctx1 Cctx_hole arg2, arg1 )

                Stmtfuncall_CLess_cctx2 arg1 arg2 ->
                    Just ( Stmtfuncall_CLess_cctx2 arg1 Cctx_hole, arg2 )

                Return_CLess_cctx1 arg1 ->
                    Just ( Return_CLess_cctx1 Cctx_hole, arg1 )

                Conditional_CLess_cctx1 arg1 ->
                    Just ( Conditional_CLess_cctx1 Cctx_hole, arg1 )

                Compstmt_CLess_cctx1 arg1 arg2 ->
                    Just ( Compstmt_CLess_cctx1 Cctx_hole arg2, arg1 )

                Compstmt_CLess_cctx2 arg1 arg2 ->
                    Just ( Compstmt_CLess_cctx2 arg1 Cctx_hole, arg2 )

                Funarg_CLess_cctx1 arg1 arg2 ->
                    Just ( Funarg_CLess_cctx1 Cctx_hole arg2, arg1 )

                Funarg_CLess_cctx2 arg1 arg2 ->
                    Just ( Funarg_CLess_cctx2 arg1 Cctx_hole, arg2 )

                Funargs_CLess_cctx1 arg1 arg2 arg3 ->
                    Just ( Funargs_CLess_cctx1 Cctx_hole arg2 arg3, arg1 )

                Funargs_CLess_cctx2 arg1 arg2 arg3 ->
                    Just ( Funargs_CLess_cctx2 arg1 Cctx_hole arg3, arg2 )

                Funargs_CLess_cctx3 arg1 arg2 arg3 ->
                    Just ( Funargs_CLess_cctx3 arg1 arg2 Cctx_hole, arg3 )

                Ifelse_CLess_cctx1 arg1 arg2 arg3 ->
                    Just ( Ifelse_CLess_cctx1 Cctx_hole arg2 arg3, arg1 )

                Ifelse_CLess_cctx2 arg1 arg2 arg3 ->
                    Just ( Ifelse_CLess_cctx2 arg1 Cctx_hole arg3, arg2 )

                Ifelse_CLess_cctx3 arg1 arg2 arg3 ->
                    Just ( Ifelse_CLess_cctx3 arg1 arg2 Cctx_hole, arg3 )

                Plus_CLess_cctx1 arg1 arg2 ->
                    Just ( Plus_CLess_cctx1 Cctx_hole arg2, arg1 )

                Plus_CLess_cctx2 arg1 arg2 ->
                    Just ( Plus_CLess_cctx2 arg1 Cctx_hole, arg2 )

                Equals_CLess_cctx1 arg1 arg2 ->
                    Just ( Equals_CLess_cctx1 Cctx_hole arg2, arg1 )

                Equals_CLess_cctx2 arg1 arg2 ->
                    Just ( Equals_CLess_cctx2 arg1 Cctx_hole, arg2 )

                Expfuncall_CLess_cctx1 arg1 arg2 ->
                    Just ( Expfuncall_CLess_cctx1 Cctx_hole arg2, arg1 )

                Expfuncall_CLess_cctx2 arg1 arg2 ->
                    Just ( Expfuncall_CLess_cctx2 arg1 Cctx_hole, arg2 )

                Expident_CLess_cctx1 arg1 ->
                    Just ( Expident_CLess_cctx1 Cctx_hole, arg1 )

        [ _ ] ->
            Just ( Cctx_hole, cctx )

        _ :: rest ->
            case cctx of
                Cctx_hole ->
                    Nothing

                Program_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Program_CLess_cctx1
                                    newCctx
                                , removedCctx
                                )
                            )

                Block_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Block_CLess_cctx1
                                    newCctx
                                , removedCctx
                                )
                            )

                Blockdecls_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Blockdecls_CLess_cctx1
                                    newCctx
                                , removedCctx
                                )
                            )

                Blockstmts_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Blockstmts_CLess_cctx1
                                    newCctx
                                , removedCctx
                                )
                            )

                Vardecl_CLess_cctx1 arg1 arg2 ( boundVars3, arg3 ) ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Vardecl_CLess_cctx1
                                    newCctx
                                    arg2
                                    ( boundVars3, arg3 )
                                , removedCctx
                                )
                            )

                Vardecl_CLess_cctx2 arg1 arg2 ( boundVars3, arg3 ) ->
                    moveCCtxHoleUp arg2 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Vardecl_CLess_cctx2
                                    arg1
                                    newCctx
                                    ( boundVars3, arg3 )
                                , removedCctx
                                )
                            )

                Vardecl_CLess_cctx3 arg1 arg2 ( boundVars3, arg3 ) ->
                    moveCCtxHoleUp arg3 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Vardecl_CLess_cctx3
                                    arg1
                                    arg2
                                    ( boundVars3, newCctx )
                                , removedCctx
                                )
                            )

                Fundecl1_CLess_cctx1 arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Fundecl1_CLess_cctx1
                                    newCctx
                                    ( boundVars2, arg2 )
                                    arg3
                                    ( boundVars4, arg4 )
                                , removedCctx
                                )
                            )

                Fundecl1_CLess_cctx2 arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
                    moveCCtxHoleUp arg2 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Fundecl1_CLess_cctx2
                                    arg1
                                    ( boundVars2, newCctx )
                                    arg3
                                    ( boundVars4, arg4 )
                                , removedCctx
                                )
                            )

                Fundecl1_CLess_cctx3 arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
                    moveCCtxHoleUp arg3 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Fundecl1_CLess_cctx3
                                    arg1
                                    ( boundVars2, arg2 )
                                    newCctx
                                    ( boundVars4, arg4 )
                                , removedCctx
                                )
                            )

                Fundecl1_CLess_cctx4 arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
                    moveCCtxHoleUp arg4 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Fundecl1_CLess_cctx4
                                    arg1
                                    ( boundVars2, arg2 )
                                    arg3
                                    ( boundVars4, newCctx )
                                , removedCctx
                                )
                            )

                Fundecl2_CLess_cctx1 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Fundecl2_CLess_cctx1
                                    newCctx
                                    ( boundVars2, arg2 )
                                    arg3
                                    arg4
                                    ( boundVars5, arg5 )
                                , removedCctx
                                )
                            )

                Fundecl2_CLess_cctx2 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
                    moveCCtxHoleUp arg2 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Fundecl2_CLess_cctx2
                                    arg1
                                    ( boundVars2, newCctx )
                                    arg3
                                    arg4
                                    ( boundVars5, arg5 )
                                , removedCctx
                                )
                            )

                Fundecl2_CLess_cctx3 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
                    moveCCtxHoleUp arg3 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Fundecl2_CLess_cctx3
                                    arg1
                                    ( boundVars2, arg2 )
                                    newCctx
                                    arg4
                                    ( boundVars5, arg5 )
                                , removedCctx
                                )
                            )

                Fundecl2_CLess_cctx4 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
                    moveCCtxHoleUp arg4 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Fundecl2_CLess_cctx4
                                    arg1
                                    ( boundVars2, arg2 )
                                    arg3
                                    newCctx
                                    ( boundVars5, arg5 )
                                , removedCctx
                                )
                            )

                Fundecl2_CLess_cctx5 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
                    moveCCtxHoleUp arg5 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Fundecl2_CLess_cctx5
                                    arg1
                                    ( boundVars2, arg2 )
                                    arg3
                                    arg4
                                    ( boundVars5, newCctx )
                                , removedCctx
                                )
                            )

                Assignment_CLess_cctx1 arg1 arg2 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Assignment_CLess_cctx1
                                    newCctx
                                    arg2
                                , removedCctx
                                )
                            )

                Assignment_CLess_cctx2 arg1 arg2 ->
                    moveCCtxHoleUp arg2 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Assignment_CLess_cctx2
                                    arg1
                                    newCctx
                                , removedCctx
                                )
                            )

                Stmtfuncall_CLess_cctx1 arg1 arg2 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Stmtfuncall_CLess_cctx1
                                    newCctx
                                    arg2
                                , removedCctx
                                )
                            )

                Stmtfuncall_CLess_cctx2 arg1 arg2 ->
                    moveCCtxHoleUp arg2 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Stmtfuncall_CLess_cctx2
                                    arg1
                                    newCctx
                                , removedCctx
                                )
                            )

                Return_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Return_CLess_cctx1
                                    newCctx
                                , removedCctx
                                )
                            )

                Conditional_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Conditional_CLess_cctx1
                                    newCctx
                                , removedCctx
                                )
                            )

                Compstmt_CLess_cctx1 arg1 arg2 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Compstmt_CLess_cctx1
                                    newCctx
                                    arg2
                                , removedCctx
                                )
                            )

                Compstmt_CLess_cctx2 arg1 arg2 ->
                    moveCCtxHoleUp arg2 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Compstmt_CLess_cctx2
                                    arg1
                                    newCctx
                                , removedCctx
                                )
                            )

                Funarg_CLess_cctx1 arg1 arg2 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Funarg_CLess_cctx1
                                    newCctx
                                    arg2
                                , removedCctx
                                )
                            )

                Funarg_CLess_cctx2 arg1 arg2 ->
                    moveCCtxHoleUp arg2 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Funarg_CLess_cctx2
                                    arg1
                                    newCctx
                                , removedCctx
                                )
                            )

                Funargs_CLess_cctx1 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Funargs_CLess_cctx1
                                    newCctx
                                    arg2
                                    arg3
                                , removedCctx
                                )
                            )

                Funargs_CLess_cctx2 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg2 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Funargs_CLess_cctx2
                                    arg1
                                    newCctx
                                    arg3
                                , removedCctx
                                )
                            )

                Funargs_CLess_cctx3 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg3 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Funargs_CLess_cctx3
                                    arg1
                                    arg2
                                    newCctx
                                , removedCctx
                                )
                            )

                Ifelse_CLess_cctx1 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Ifelse_CLess_cctx1
                                    newCctx
                                    arg2
                                    arg3
                                , removedCctx
                                )
                            )

                Ifelse_CLess_cctx2 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg2 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Ifelse_CLess_cctx2
                                    arg1
                                    newCctx
                                    arg3
                                , removedCctx
                                )
                            )

                Ifelse_CLess_cctx3 arg1 arg2 arg3 ->
                    moveCCtxHoleUp arg3 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Ifelse_CLess_cctx3
                                    arg1
                                    arg2
                                    newCctx
                                , removedCctx
                                )
                            )

                Plus_CLess_cctx1 arg1 arg2 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Plus_CLess_cctx1
                                    newCctx
                                    arg2
                                , removedCctx
                                )
                            )

                Plus_CLess_cctx2 arg1 arg2 ->
                    moveCCtxHoleUp arg2 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Plus_CLess_cctx2
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

                Expfuncall_CLess_cctx1 arg1 arg2 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Expfuncall_CLess_cctx1
                                    newCctx
                                    arg2
                                , removedCctx
                                )
                            )

                Expfuncall_CLess_cctx2 arg1 arg2 ->
                    moveCCtxHoleUp arg2 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Expfuncall_CLess_cctx2
                                    arg1
                                    newCctx
                                , removedCctx
                                )
                            )

                Expident_CLess_cctx1 arg1 ->
                    moveCCtxHoleUp arg1 rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Expident_CLess_cctx1
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

        Program_CLess_cctx1 arg1 ->
            case wellformed of
                Root_fd_CLess underCursor ->
                    Just (Root_p_CLess (Program_CLess underCursor))

                _ ->
                    Nothing

        Block_CLess_cctx1 arg1 ->
            case wellformed of
                Root_bi_CLess underCursor ->
                    Just (Root_b_CLess (Block_CLess underCursor))

                _ ->
                    Nothing

        Blockdecls_CLess_cctx1 arg1 ->
            case wellformed of
                Root_vd_CLess underCursor ->
                    Just (Root_bi_CLess (Blockdecls_CLess underCursor))

                _ ->
                    Nothing

        Blockstmts_CLess_cctx1 arg1 ->
            case wellformed of
                Root_s_CLess underCursor ->
                    Just (Root_bi_CLess (Blockstmts_CLess underCursor))

                _ ->
                    Nothing

        Vardecl_CLess_cctx1 arg1 arg2 ( boundVars3, arg3 ) ->
            case wellformed of
                Root_t_CLess underCursor ->
                    Just
                        (Root_vd_CLess
                            (Vardecl_CLess underCursor arg2 ( boundVars3, arg3 ))
                        )

                _ ->
                    Nothing

        Vardecl_CLess_cctx2 arg1 arg2 ( boundVars3, arg3 ) ->
            case wellformed of
                Root_e_CLess underCursor ->
                    Just
                        (Root_vd_CLess
                            (Vardecl_CLess arg1 underCursor ( boundVars3, arg3 ))
                        )

                _ ->
                    Nothing

        Vardecl_CLess_cctx3 arg1 arg2 ( boundVars3, arg3 ) ->
            case wellformed of
                Root_bi_CLess underCursor ->
                    Just
                        (Root_vd_CLess
                            (Vardecl_CLess arg1 arg2 ( boundVars3, underCursor ))
                        )

                _ ->
                    Nothing

        Fundecl1_CLess_cctx1 arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
            case wellformed of
                Root_t_CLess underCursor ->
                    Just
                        (Root_fd_CLess
                            (Fundecl1_CLess
                                underCursor
                                ( boundVars2, arg2 )
                                arg3
                                ( boundVars4, arg4 )
                            )
                        )

                _ ->
                    Nothing

        Fundecl1_CLess_cctx2 arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
            case wellformed of
                Root_fd_CLess underCursor ->
                    Just
                        (Root_fd_CLess
                            (Fundecl1_CLess
                                arg1
                                ( boundVars2, underCursor )
                                arg3
                                ( boundVars4, arg4 )
                            )
                        )

                _ ->
                    Nothing

        Fundecl1_CLess_cctx3 arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
            case wellformed of
                Root_t_CLess underCursor ->
                    Just
                        (Root_fd_CLess
                            (Fundecl1_CLess
                                arg1
                                ( boundVars2, arg2 )
                                underCursor
                                ( boundVars4, arg4 )
                            )
                        )

                _ ->
                    Nothing

        Fundecl1_CLess_cctx4 arg1 ( boundVars2, arg2 ) arg3 ( boundVars4, arg4 ) ->
            case wellformed of
                Root_b_CLess underCursor ->
                    Just
                        (Root_fd_CLess
                            (Fundecl1_CLess
                                arg1
                                ( boundVars2, arg2 )
                                arg3
                                ( boundVars4, underCursor )
                            )
                        )

                _ ->
                    Nothing

        Fundecl2_CLess_cctx1 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
            case wellformed of
                Root_t_CLess underCursor ->
                    Just
                        (Root_fd_CLess
                            (Fundecl2_CLess
                                underCursor
                                ( boundVars2, arg2 )
                                arg3
                                arg4
                                ( boundVars5, arg5 )
                            )
                        )

                _ ->
                    Nothing

        Fundecl2_CLess_cctx2 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
            case wellformed of
                Root_fd_CLess underCursor ->
                    Just
                        (Root_fd_CLess
                            (Fundecl2_CLess
                                arg1
                                ( boundVars2, underCursor )
                                arg3
                                arg4
                                ( boundVars5, arg5 )
                            )
                        )

                _ ->
                    Nothing

        Fundecl2_CLess_cctx3 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
            case wellformed of
                Root_t_CLess underCursor ->
                    Just
                        (Root_fd_CLess
                            (Fundecl2_CLess
                                arg1
                                ( boundVars2, arg2 )
                                underCursor
                                arg4
                                ( boundVars5, arg5 )
                            )
                        )

                _ ->
                    Nothing

        Fundecl2_CLess_cctx4 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
            case wellformed of
                Root_t_CLess underCursor ->
                    Just
                        (Root_fd_CLess
                            (Fundecl2_CLess
                                arg1
                                ( boundVars2, arg2 )
                                arg3
                                underCursor
                                ( boundVars5, arg5 )
                            )
                        )

                _ ->
                    Nothing

        Fundecl2_CLess_cctx5 arg1 ( boundVars2, arg2 ) arg3 arg4 ( boundVars5, arg5 ) ->
            case wellformed of
                Root_b_CLess underCursor ->
                    Just
                        (Root_fd_CLess
                            (Fundecl2_CLess
                                arg1
                                ( boundVars2, arg2 )
                                arg3
                                arg4
                                ( boundVars5, underCursor )
                            )
                        )

                _ ->
                    Nothing

        Assignment_CLess_cctx1 arg1 arg2 ->
            case wellformed of
                Root_id_CLess underCursor ->
                    Just (Root_s_CLess (Assignment_CLess underCursor arg2))

                _ ->
                    Nothing

        Assignment_CLess_cctx2 arg1 arg2 ->
            case wellformed of
                Root_e_CLess underCursor ->
                    Just (Root_s_CLess (Assignment_CLess arg1 underCursor))

                _ ->
                    Nothing

        Stmtfuncall_CLess_cctx1 arg1 arg2 ->
            case wellformed of
                Root_id_CLess underCursor ->
                    Just (Root_s_CLess (Stmtfuncall_CLess underCursor arg2))

                _ ->
                    Nothing

        Stmtfuncall_CLess_cctx2 arg1 arg2 ->
            case wellformed of
                Root_fa_CLess underCursor ->
                    Just (Root_s_CLess (Stmtfuncall_CLess arg1 underCursor))

                _ ->
                    Nothing

        Return_CLess_cctx1 arg1 ->
            case wellformed of
                Root_e_CLess underCursor ->
                    Just (Root_s_CLess (Return_CLess underCursor))

                _ ->
                    Nothing

        Conditional_CLess_cctx1 arg1 ->
            case wellformed of
                Root_cond_CLess underCursor ->
                    Just (Root_s_CLess (Conditional_CLess underCursor))

                _ ->
                    Nothing

        Compstmt_CLess_cctx1 arg1 arg2 ->
            case wellformed of
                Root_s_CLess underCursor ->
                    Just (Root_s_CLess (Compstmt_CLess underCursor arg2))

                _ ->
                    Nothing

        Compstmt_CLess_cctx2 arg1 arg2 ->
            case wellformed of
                Root_s_CLess underCursor ->
                    Just (Root_s_CLess (Compstmt_CLess arg1 underCursor))

                _ ->
                    Nothing

        Funarg_CLess_cctx1 arg1 arg2 ->
            case wellformed of
                Root_t_CLess underCursor ->
                    Just (Root_fa_CLess (Funarg_CLess underCursor arg2))

                _ ->
                    Nothing

        Funarg_CLess_cctx2 arg1 arg2 ->
            case wellformed of
                Root_id_CLess underCursor ->
                    Just (Root_fa_CLess (Funarg_CLess arg1 underCursor))

                _ ->
                    Nothing

        Funargs_CLess_cctx1 arg1 arg2 arg3 ->
            case wellformed of
                Root_t_CLess underCursor ->
                    Just (Root_fa_CLess (Funargs_CLess underCursor arg2 arg3))

                _ ->
                    Nothing

        Funargs_CLess_cctx2 arg1 arg2 arg3 ->
            case wellformed of
                Root_id_CLess underCursor ->
                    Just (Root_fa_CLess (Funargs_CLess arg1 underCursor arg3))

                _ ->
                    Nothing

        Funargs_CLess_cctx3 arg1 arg2 arg3 ->
            case wellformed of
                Root_fa_CLess underCursor ->
                    Just (Root_fa_CLess (Funargs_CLess arg1 arg2 underCursor))

                _ ->
                    Nothing

        Ifelse_CLess_cctx1 arg1 arg2 arg3 ->
            case wellformed of
                Root_e_CLess underCursor ->
                    Just (Root_cond_CLess (Ifelse_CLess underCursor arg2 arg3))

                _ ->
                    Nothing

        Ifelse_CLess_cctx2 arg1 arg2 arg3 ->
            case wellformed of
                Root_b_CLess underCursor ->
                    Just (Root_cond_CLess (Ifelse_CLess arg1 underCursor arg3))

                _ ->
                    Nothing

        Ifelse_CLess_cctx3 arg1 arg2 arg3 ->
            case wellformed of
                Root_b_CLess underCursor ->
                    Just (Root_cond_CLess (Ifelse_CLess arg1 arg2 underCursor))

                _ ->
                    Nothing

        Plus_CLess_cctx1 arg1 arg2 ->
            case wellformed of
                Root_e_CLess underCursor ->
                    Just (Root_e_CLess (Plus_CLess underCursor arg2))

                _ ->
                    Nothing

        Plus_CLess_cctx2 arg1 arg2 ->
            case wellformed of
                Root_e_CLess underCursor ->
                    Just (Root_e_CLess (Plus_CLess arg1 underCursor))

                _ ->
                    Nothing

        Equals_CLess_cctx1 arg1 arg2 ->
            case wellformed of
                Root_e_CLess underCursor ->
                    Just (Root_e_CLess (Equals_CLess underCursor arg2))

                _ ->
                    Nothing

        Equals_CLess_cctx2 arg1 arg2 ->
            case wellformed of
                Root_e_CLess underCursor ->
                    Just (Root_e_CLess (Equals_CLess arg1 underCursor))

                _ ->
                    Nothing

        Expfuncall_CLess_cctx1 arg1 arg2 ->
            case wellformed of
                Root_id_CLess underCursor ->
                    Just (Root_e_CLess (Expfuncall_CLess underCursor arg2))

                _ ->
                    Nothing

        Expfuncall_CLess_cctx2 arg1 arg2 ->
            case wellformed of
                Root_fa_CLess underCursor ->
                    Just (Root_e_CLess (Expfuncall_CLess arg1 underCursor))

                _ ->
                    Nothing

        Expident_CLess_cctx1 arg1 ->
            case wellformed of
                Root_id_CLess underCursor ->
                    Just (Root_e_CLess (Expident_CLess underCursor))

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


type EditorCond
    = Neg EditorCond
    | Conjunction EditorCond EditorCond
    | Disjunction EditorCond EditorCond
    | At CursorLess
    | Possibly CursorLess
    | Necessarily CursorLess


type alias Decomposed =
    ( Cctx, Wellformed )


evalCond : Decomposed -> EditorCond -> Bool
evalCond decomposed cond =
    case cond of
        Neg arg1 ->
            not (evalCond decomposed arg1)

        Conjunction arg1 arg2 ->
            evalCond decomposed arg1 && evalCond decomposed arg2

        Disjunction arg1 arg2 ->
            evalCond decomposed arg1 || evalCond decomposed arg2

        At cursorlessOp ->
            atOp cursorlessOp (Just decomposed)

        Possibly cursorlessOp ->
            possibly cursorlessOp (Just decomposed)

        Necessarily cursorlessOp ->
            necessity cursorlessOp decomposed


atOp : CursorLess -> Maybe Decomposed -> Bool
atOp cursorlessop maybedecomposed =
    case maybedecomposed of
        Nothing ->
            False

        Just decomposed ->
            case ( cursorlessop, getOpAtCursor decomposed ) of
                ( P_CLess query, P_CLess op ) ->
                    same_p_CLess query op

                ( B_CLess query, B_CLess op ) ->
                    same_b_CLess query op

                ( Bi_CLess query, Bi_CLess op ) ->
                    same_bi_CLess query op

                ( Vd_CLess query, Vd_CLess op ) ->
                    same_vd_CLess query op

                ( Fd_CLess query, Fd_CLess op ) ->
                    same_fd_CLess query op

                ( S_CLess query, S_CLess op ) ->
                    same_s_CLess query op

                ( Fa_CLess query, Fa_CLess op ) ->
                    same_fa_CLess query op

                ( Cond_CLess query, Cond_CLess op ) ->
                    same_cond_CLess query op

                ( T_CLess query, T_CLess op ) ->
                    same_t_CLess query op

                ( E_CLess query, E_CLess op ) ->
                    same_e_CLess query op

                ( Id_CLess query, Id_CLess op ) ->
                    same_id_CLess query op

                _ ->
                    False


getOpAtCursor : Decomposed -> CursorLess
getOpAtCursor decomposed =
    let
        ( cctx, wellformed ) =
            decomposed
    in
    case wellformed of
        Root_p_CLess arg1 ->
            P_CLess arg1

        Root_s_CLess arg1 ->
            S_CLess arg1

        Root_vd_CLess arg1 ->
            Vd_CLess arg1

        Root_fd_CLess arg1 ->
            Fd_CLess arg1

        Root_t_CLess arg1 ->
            T_CLess arg1

        Root_id_CLess arg1 ->
            Id_CLess arg1

        Root_e_CLess arg1 ->
            E_CLess arg1

        Root_b_CLess arg1 ->
            B_CLess arg1

        Root_bi_CLess arg1 ->
            Bi_CLess arg1

        Root_fa_CLess arg1 ->
            Fa_CLess arg1

        Root_cond_CLess arg1 ->
            Cond_CLess arg1


possibly : CursorLess -> Maybe Decomposed -> Bool
possibly cursorlessop maybedecomposed =
    case maybedecomposed of
        Nothing ->
            False

        Just decomposed ->
            atOp cursorlessop (Just decomposed)
                || possibly
                    cursorlessop
                    (child 1 decomposed)
                || possibly
                    cursorlessop
                    (child
                        2
                        decomposed
                    )
                || possibly
                    cursorlessop
                    (child
                        3
                        decomposed
                    )
                || possibly
                    cursorlessop
                    (child
                        4
                        decomposed
                    )
                || possibly
                    cursorlessop
                    (child
                        5
                        decomposed
                    )


necessity : CursorLess -> Decomposed -> Bool
necessity cursorlessop decomposed =
    case getOpAtCursor decomposed of
        P_CLess arg1 ->
            case arg1 of
                Program_CLess _ ->
                    possibly cursorlessop (child 1 decomposed)

                Hole_p_CLess ->
                    False

        B_CLess arg1 ->
            case arg1 of
                Block_CLess _ ->
                    possibly cursorlessop (child 1 decomposed)

                Hole_b_CLess ->
                    False

        Bi_CLess arg1 ->
            case arg1 of
                Blockdecls_CLess _ ->
                    possibly cursorlessop (child 1 decomposed)

                Blockstmts_CLess _ ->
                    possibly cursorlessop (child 1 decomposed)

                Blockdone_CLess ->
                    False

                Hole_bi_CLess ->
                    False

        Vd_CLess arg1 ->
            case arg1 of
                Vardecl_CLess _ _ _ ->
                    (possibly cursorlessop (child 1 decomposed)
                        && possibly
                            cursorlessop
                            (child
                                2
                                decomposed
                            )
                    )
                        && possibly cursorlessop (child 3 decomposed)

                Hole_vd_CLess ->
                    False

        Fd_CLess arg1 ->
            case arg1 of
                Fundecl1_CLess _ _ _ _ ->
                    ((possibly cursorlessop (child 1 decomposed)
                        && possibly
                            cursorlessop
                            (child
                                2
                                decomposed
                            )
                     )
                        && possibly cursorlessop (child 3 decomposed)
                    )
                        && possibly cursorlessop (child 4 decomposed)

                Fundecl2_CLess _ _ _ _ _ ->
                    (((possibly cursorlessop (child 1 decomposed)
                        && possibly
                            cursorlessop
                            (child
                                2
                                decomposed
                            )
                      )
                        && possibly cursorlessop (child 3 decomposed)
                     )
                        && possibly cursorlessop (child 4 decomposed)
                    )
                        && possibly cursorlessop (child 5 decomposed)

                Fundecldone_CLess ->
                    False

                Hole_fd_CLess ->
                    False

        S_CLess arg1 ->
            case arg1 of
                Assignment_CLess _ _ ->
                    possibly cursorlessop (child 1 decomposed)
                        && possibly
                            cursorlessop
                            (child
                                2
                                decomposed
                            )

                Stmtfuncall_CLess _ _ ->
                    possibly cursorlessop (child 1 decomposed)
                        && possibly
                            cursorlessop
                            (child
                                2
                                decomposed
                            )

                Return_CLess _ ->
                    possibly cursorlessop (child 1 decomposed)

                Conditional_CLess _ ->
                    possibly cursorlessop (child 1 decomposed)

                Compstmt_CLess _ _ ->
                    possibly cursorlessop (child 1 decomposed)
                        && possibly
                            cursorlessop
                            (child
                                2
                                decomposed
                            )

                Hole_s_CLess ->
                    False

        Fa_CLess arg1 ->
            case arg1 of
                Funarg_CLess _ _ ->
                    possibly cursorlessop (child 1 decomposed)
                        && possibly
                            cursorlessop
                            (child
                                2
                                decomposed
                            )

                Funargs_CLess _ _ _ ->
                    (possibly cursorlessop (child 1 decomposed)
                        && possibly
                            cursorlessop
                            (child
                                2
                                decomposed
                            )
                    )
                        && possibly cursorlessop (child 3 decomposed)

                Hole_fa_CLess ->
                    False

        Cond_CLess arg1 ->
            case arg1 of
                Ifelse_CLess _ _ _ ->
                    (possibly cursorlessop (child 1 decomposed)
                        && possibly
                            cursorlessop
                            (child
                                2
                                decomposed
                            )
                    )
                        && possibly cursorlessop (child 3 decomposed)

                Hole_cond_CLess ->
                    False

        T_CLess arg1 ->
            case arg1 of
                Tint_CLess ->
                    False

                Tchar_CLess ->
                    False

                Tbool_CLess ->
                    False

                Hole_t_CLess ->
                    False

        E_CLess arg1 ->
            case arg1 of
                Int_CLess _ ->
                    False

                Char_CLess _ ->
                    False

                Bool_CLess _ ->
                    False

                Plus_CLess _ _ ->
                    possibly cursorlessop (child 1 decomposed)
                        && possibly
                            cursorlessop
                            (child
                                2
                                decomposed
                            )

                Equals_CLess _ _ ->
                    possibly cursorlessop (child 1 decomposed)
                        && possibly
                            cursorlessop
                            (child
                                2
                                decomposed
                            )

                Expfuncall_CLess _ _ ->
                    possibly cursorlessop (child 1 decomposed)
                        && possibly
                            cursorlessop
                            (child
                                2
                                decomposed
                            )

                Expident_CLess _ ->
                    possibly cursorlessop (child 1 decomposed)

                Hole_e_CLess ->
                    False

        Id_CLess arg1 ->
            case arg1 of
                Ident_CLess _ ->
                    False

                Hole_id_CLess ->
                    False


same_p_CLess : P_CLess -> P_CLess -> Bool
same_p_CLess query op =
    case ( query, op ) of
        ( Program_CLess _, Program_CLess _ ) ->
            True

        ( Hole_p_CLess, Hole_p_CLess ) ->
            True

        _ ->
            False


same_b_CLess : B_CLess -> B_CLess -> Bool
same_b_CLess query op =
    case ( query, op ) of
        ( Block_CLess _, Block_CLess _ ) ->
            True

        ( Hole_b_CLess, Hole_b_CLess ) ->
            True

        _ ->
            False


same_bi_CLess : Bi_CLess -> Bi_CLess -> Bool
same_bi_CLess query op =
    case ( query, op ) of
        ( Blockdecls_CLess _, Blockdecls_CLess _ ) ->
            True

        ( Blockstmts_CLess _, Blockstmts_CLess _ ) ->
            True

        ( Blockdone_CLess, Blockdone_CLess ) ->
            True

        ( Hole_bi_CLess, Hole_bi_CLess ) ->
            True

        _ ->
            False


same_vd_CLess : Vd_CLess -> Vd_CLess -> Bool
same_vd_CLess query op =
    case ( query, op ) of
        ( Vardecl_CLess _ _ _, Vardecl_CLess _ _ _ ) ->
            True

        ( Hole_vd_CLess, Hole_vd_CLess ) ->
            True

        _ ->
            False


same_fd_CLess : Fd_CLess -> Fd_CLess -> Bool
same_fd_CLess query op =
    case ( query, op ) of
        ( Fundecl1_CLess _ _ _ _, Fundecl1_CLess _ _ _ _ ) ->
            True

        ( Fundecl2_CLess _ _ _ _ _, Fundecl2_CLess _ _ _ _ _ ) ->
            True

        ( Fundecldone_CLess, Fundecldone_CLess ) ->
            True

        ( Hole_fd_CLess, Hole_fd_CLess ) ->
            True

        _ ->
            False


same_s_CLess : S_CLess -> S_CLess -> Bool
same_s_CLess query op =
    case ( query, op ) of
        ( Assignment_CLess _ _, Assignment_CLess _ _ ) ->
            True

        ( Stmtfuncall_CLess _ _, Stmtfuncall_CLess _ _ ) ->
            True

        ( Return_CLess _, Return_CLess _ ) ->
            True

        ( Conditional_CLess _, Conditional_CLess _ ) ->
            True

        ( Compstmt_CLess _ _, Compstmt_CLess _ _ ) ->
            True

        ( Hole_s_CLess, Hole_s_CLess ) ->
            True

        _ ->
            False


same_fa_CLess : Fa_CLess -> Fa_CLess -> Bool
same_fa_CLess query op =
    case ( query, op ) of
        ( Funarg_CLess _ _, Funarg_CLess _ _ ) ->
            True

        ( Funargs_CLess _ _ _, Funargs_CLess _ _ _ ) ->
            True

        ( Hole_fa_CLess, Hole_fa_CLess ) ->
            True

        _ ->
            False


same_cond_CLess : Cond_CLess -> Cond_CLess -> Bool
same_cond_CLess query op =
    case ( query, op ) of
        ( Ifelse_CLess _ _ _, Ifelse_CLess _ _ _ ) ->
            True

        ( Hole_cond_CLess, Hole_cond_CLess ) ->
            True

        _ ->
            False


same_t_CLess : T_CLess -> T_CLess -> Bool
same_t_CLess query op =
    case ( query, op ) of
        ( Tint_CLess, Tint_CLess ) ->
            True

        ( Tchar_CLess, Tchar_CLess ) ->
            True

        ( Tbool_CLess, Tbool_CLess ) ->
            True

        ( Hole_t_CLess, Hole_t_CLess ) ->
            True

        _ ->
            False


same_e_CLess : E_CLess -> E_CLess -> Bool
same_e_CLess query op =
    case ( query, op ) of
        ( Int_CLess _, Int_CLess _ ) ->
            True

        ( Char_CLess _, Char_CLess _ ) ->
            True

        ( Bool_CLess _, Bool_CLess _ ) ->
            True

        ( Plus_CLess _ _, Plus_CLess _ _ ) ->
            True

        ( Equals_CLess _ _, Equals_CLess _ _ ) ->
            True

        ( Expfuncall_CLess _ _, Expfuncall_CLess _ _ ) ->
            True

        ( Expident_CLess _, Expident_CLess _ ) ->
            True

        ( Hole_e_CLess, Hole_e_CLess ) ->
            True

        _ ->
            False


same_id_CLess : Id_CLess -> Id_CLess -> Bool
same_id_CLess query op =
    case ( query, op ) of
        ( Ident_CLess _, Ident_CLess _ ) ->
            True

        ( Hole_id_CLess, Hole_id_CLess ) ->
            True

        _ ->
            False
