module Conditionals exposing (..)

import Movement
import Syntax.CCtx
import Syntax.Cursorless
import Syntax.Wellformed


type EditorCond
    = Neg EditorCond
    | Conjunction EditorCond EditorCond
    | Disjunction EditorCond EditorCond
    | At Syntax.Cursorless.CursorLess
    | Possibly Syntax.Cursorless.CursorLess
    | Necessarily Syntax.Cursorless.CursorLess


type alias Decomposed =
    ( Syntax.CCtx.Cctx, Syntax.Wellformed.Wellformed )


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


atOp : Syntax.Cursorless.CursorLess -> Maybe Decomposed -> Bool
atOp cursorlessop maybedecomposed =
    case maybedecomposed of
        Nothing ->
            False

        Just decomposed ->
            case ( cursorlessop, getOpAtCursor decomposed ) of
                ( Syntax.Cursorless.P_CLess query, Syntax.Cursorless.P_CLess op ) ->
                    same_p_CLess query op

                ( Syntax.Cursorless.B_CLess query, Syntax.Cursorless.B_CLess op ) ->
                    same_b_CLess query op

                ( Syntax.Cursorless.Bi_CLess query, Syntax.Cursorless.Bi_CLess op ) ->
                    same_bi_CLess query op

                ( Syntax.Cursorless.Vd_CLess query, Syntax.Cursorless.Vd_CLess op ) ->
                    same_vd_CLess query op

                ( Syntax.Cursorless.Fd_CLess query, Syntax.Cursorless.Fd_CLess op ) ->
                    same_fd_CLess query op

                ( Syntax.Cursorless.S_CLess query, Syntax.Cursorless.S_CLess op ) ->
                    same_s_CLess query op

                ( Syntax.Cursorless.Fa_CLess query, Syntax.Cursorless.Fa_CLess op ) ->
                    same_fa_CLess query op

                ( Syntax.Cursorless.Cond_CLess query, Syntax.Cursorless.Cond_CLess op ) ->
                    same_cond_CLess query op

                ( Syntax.Cursorless.T_CLess query, Syntax.Cursorless.T_CLess op ) ->
                    same_t_CLess query op

                ( Syntax.Cursorless.E_CLess query, Syntax.Cursorless.E_CLess op ) ->
                    same_e_CLess query op

                ( Syntax.Cursorless.Id_CLess query, Syntax.Cursorless.Id_CLess op ) ->
                    same_id_CLess query op

                _ ->
                    False


getOpAtCursor : Decomposed -> Syntax.Cursorless.CursorLess
getOpAtCursor decomposed =
    let
        ( cctx, wellformed ) =
            decomposed
    in
    case wellformed of
        Syntax.Wellformed.Root_p_CLess arg1 ->
            Syntax.Cursorless.P_CLess arg1

        Syntax.Wellformed.Root_s_CLess arg1 ->
            Syntax.Cursorless.S_CLess arg1

        Syntax.Wellformed.Root_vd_CLess arg1 ->
            Syntax.Cursorless.Vd_CLess arg1

        Syntax.Wellformed.Root_fd_CLess arg1 ->
            Syntax.Cursorless.Fd_CLess arg1

        Syntax.Wellformed.Root_t_CLess arg1 ->
            Syntax.Cursorless.T_CLess arg1

        Syntax.Wellformed.Root_id_CLess arg1 ->
            Syntax.Cursorless.Id_CLess arg1

        Syntax.Wellformed.Root_e_CLess arg1 ->
            Syntax.Cursorless.E_CLess arg1

        Syntax.Wellformed.Root_b_CLess arg1 ->
            Syntax.Cursorless.B_CLess arg1

        Syntax.Wellformed.Root_bi_CLess arg1 ->
            Syntax.Cursorless.Bi_CLess arg1

        Syntax.Wellformed.Root_fa_CLess arg1 ->
            Syntax.Cursorless.Fa_CLess arg1

        Syntax.Wellformed.Root_cond_CLess arg1 ->
            Syntax.Cursorless.Cond_CLess arg1


possibly : Syntax.Cursorless.CursorLess -> Maybe Decomposed -> Bool
possibly cursorlessop maybedecomposed =
    case maybedecomposed of
        Nothing ->
            False

        Just decomposed ->
            atOp cursorlessop (Just decomposed) || possibly
                                                           cursorlessop
                                                           (Movement.child
                                                                    1
                                                                    decomposed
                                                           ) || possibly
                                                                            cursorlessop
                                                                            (Movement.child
                                                                                         2
                                                                                         decomposed
                                                                            ) || possibly
                                                                                                 cursorlessop
                                                                                                 (Movement.child
                                                                                                                  3
                                                                                                                  decomposed
                                                                                                 ) || possibly
                                                                                                                          cursorlessop
                                                                                                                          (Movement.child
                                                                                                                                               4
                                                                                                                                               decomposed
                                                                                                                          ) || possibly
                                                                                                                                                       cursorlessop
                                                                                                                                                       (Movement.child
                                                                                                                                                                                5
                                                                                                                                                                                decomposed
                                                                                                                                                       )


necessity : Syntax.Cursorless.CursorLess -> Decomposed -> Bool
necessity cursorlessop decomposed =
    case getOpAtCursor decomposed of
        Syntax.Cursorless.P_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.Program_CLess _ ->
                    possibly cursorlessop (Movement.child 1 decomposed)

                Syntax.Cursorless.Hole_p_CLess ->
                    False

        Syntax.Cursorless.B_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.Block_CLess _ ->
                    possibly cursorlessop (Movement.child 1 decomposed)

                Syntax.Cursorless.Hole_b_CLess ->
                    False

        Syntax.Cursorless.Bi_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.Blockdecls_CLess _ ->
                    possibly cursorlessop (Movement.child 1 decomposed)

                Syntax.Cursorless.Blockstmts_CLess _ ->
                    possibly cursorlessop (Movement.child 1 decomposed)

                Syntax.Cursorless.Blockdone_CLess ->
                    False

                Syntax.Cursorless.Hole_bi_CLess ->
                    False

        Syntax.Cursorless.Vd_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.Vardecl_CLess _ _ _ ->
                    (possibly
                         cursorlessop
                         (Movement.child 1 decomposed) && possibly
                                                                  cursorlessop
                                                                  (Movement.child
                                                                           2
                                                                           decomposed
                                                                  )
                    ) && possibly cursorlessop (Movement.child 3 decomposed)

                Syntax.Cursorless.Hole_vd_CLess ->
                    False

        Syntax.Cursorless.Fd_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.Fundecl1_CLess _ _ _ _ ->
                    ((possibly
                          cursorlessop
                          (Movement.child 1 decomposed) && possibly
                                                                   cursorlessop
                                                                   (Movement.child
                                                                            2
                                                                            decomposed
                                                                   )
                     ) && possibly cursorlessop (Movement.child 3 decomposed)
                    ) && possibly cursorlessop (Movement.child 4 decomposed)

                Syntax.Cursorless.Fundecl2_CLess _ _ _ _ _ ->
                    (((possibly
                           cursorlessop
                           (Movement.child 1 decomposed) && possibly
                                                                    cursorlessop
                                                                    (Movement.child
                                                                             2
                                                                             decomposed
                                                                    )
                      ) && possibly cursorlessop (Movement.child 3 decomposed)
                     ) && possibly cursorlessop (Movement.child 4 decomposed)
                    ) && possibly cursorlessop (Movement.child 5 decomposed)

                Syntax.Cursorless.Fundecldone_CLess ->
                    False

                Syntax.Cursorless.Hole_fd_CLess ->
                    False

        Syntax.Cursorless.S_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.Assignment_CLess _ _ ->
                    possibly
                        cursorlessop
                        (Movement.child 1 decomposed) && possibly
                                                                 cursorlessop
                                                                 (Movement.child
                                                                          2
                                                                          decomposed
                                                                 )

                Syntax.Cursorless.Stmtfuncall_CLess _ _ ->
                    possibly
                        cursorlessop
                        (Movement.child 1 decomposed) && possibly
                                                                 cursorlessop
                                                                 (Movement.child
                                                                          2
                                                                          decomposed
                                                                 )

                Syntax.Cursorless.Return_CLess _ ->
                    possibly cursorlessop (Movement.child 1 decomposed)

                Syntax.Cursorless.Conditional_CLess _ ->
                    possibly cursorlessop (Movement.child 1 decomposed)

                Syntax.Cursorless.Compstmt_CLess _ _ ->
                    possibly
                        cursorlessop
                        (Movement.child 1 decomposed) && possibly
                                                                 cursorlessop
                                                                 (Movement.child
                                                                          2
                                                                          decomposed
                                                                 )

                Syntax.Cursorless.Hole_s_CLess ->
                    False

        Syntax.Cursorless.Fa_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.Funarg_CLess _ _ ->
                    possibly
                        cursorlessop
                        (Movement.child 1 decomposed) && possibly
                                                                 cursorlessop
                                                                 (Movement.child
                                                                          2
                                                                          decomposed
                                                                 )

                Syntax.Cursorless.Funargs_CLess _ _ _ ->
                    (possibly
                         cursorlessop
                         (Movement.child 1 decomposed) && possibly
                                                                  cursorlessop
                                                                  (Movement.child
                                                                           2
                                                                           decomposed
                                                                  )
                    ) && possibly cursorlessop (Movement.child 3 decomposed)

                Syntax.Cursorless.Hole_fa_CLess ->
                    False

        Syntax.Cursorless.Cond_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.Ifelse_CLess _ _ _ ->
                    (possibly
                         cursorlessop
                         (Movement.child 1 decomposed) && possibly
                                                                  cursorlessop
                                                                  (Movement.child
                                                                           2
                                                                           decomposed
                                                                  )
                    ) && possibly cursorlessop (Movement.child 3 decomposed)

                Syntax.Cursorless.Hole_cond_CLess ->
                    False

        Syntax.Cursorless.T_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.Tint_CLess ->
                    False

                Syntax.Cursorless.Tchar_CLess ->
                    False

                Syntax.Cursorless.Tbool_CLess ->
                    False

                Syntax.Cursorless.Hole_t_CLess ->
                    False

        Syntax.Cursorless.E_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.Int_CLess _ ->
                    False

                Syntax.Cursorless.Char_CLess _ ->
                    False

                Syntax.Cursorless.Bool_CLess _ ->
                    False

                Syntax.Cursorless.Plus_CLess _ _ ->
                    possibly
                        cursorlessop
                        (Movement.child 1 decomposed) && possibly
                                                                 cursorlessop
                                                                 (Movement.child
                                                                          2
                                                                          decomposed
                                                                 )

                Syntax.Cursorless.Equals_CLess _ _ ->
                    possibly
                        cursorlessop
                        (Movement.child 1 decomposed) && possibly
                                                                 cursorlessop
                                                                 (Movement.child
                                                                          2
                                                                          decomposed
                                                                 )

                Syntax.Cursorless.Expfuncall_CLess _ _ ->
                    possibly
                        cursorlessop
                        (Movement.child 1 decomposed) && possibly
                                                                 cursorlessop
                                                                 (Movement.child
                                                                          2
                                                                          decomposed
                                                                 )

                Syntax.Cursorless.Expident_CLess _ ->
                    possibly cursorlessop (Movement.child 1 decomposed)

                Syntax.Cursorless.Hole_e_CLess ->
                    False

        Syntax.Cursorless.Id_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.Ident_CLess _ ->
                    False

                Syntax.Cursorless.Hole_id_CLess ->
                    False


same_p_CLess : Syntax.Cursorless.P_CLess -> Syntax.Cursorless.P_CLess -> Bool
same_p_CLess query op =
    case ( query, op ) of
        ( Syntax.Cursorless.Program_CLess _, Syntax.Cursorless.Program_CLess _ ) ->
            True

        ( Syntax.Cursorless.Hole_p_CLess, Syntax.Cursorless.Hole_p_CLess ) ->
            True

        _ ->
            False


same_b_CLess : Syntax.Cursorless.B_CLess -> Syntax.Cursorless.B_CLess -> Bool
same_b_CLess query op =
    case ( query, op ) of
        ( Syntax.Cursorless.Block_CLess _, Syntax.Cursorless.Block_CLess _ ) ->
            True

        ( Syntax.Cursorless.Hole_b_CLess, Syntax.Cursorless.Hole_b_CLess ) ->
            True

        _ ->
            False


same_bi_CLess : Syntax.Cursorless.Bi_CLess -> Syntax.Cursorless.Bi_CLess -> Bool
same_bi_CLess query op =
    case ( query, op ) of
        ( Syntax.Cursorless.Blockdecls_CLess _, Syntax.Cursorless.Blockdecls_CLess _ ) ->
            True

        ( Syntax.Cursorless.Blockstmts_CLess _, Syntax.Cursorless.Blockstmts_CLess _ ) ->
            True

        ( Syntax.Cursorless.Blockdone_CLess, Syntax.Cursorless.Blockdone_CLess ) ->
            True

        ( Syntax.Cursorless.Hole_bi_CLess, Syntax.Cursorless.Hole_bi_CLess ) ->
            True

        _ ->
            False


same_vd_CLess : Syntax.Cursorless.Vd_CLess -> Syntax.Cursorless.Vd_CLess -> Bool
same_vd_CLess query op =
    case ( query, op ) of
        ( Syntax.Cursorless.Vardecl_CLess _ _ _, Syntax.Cursorless.Vardecl_CLess _ _ _ ) ->
            True

        ( Syntax.Cursorless.Hole_vd_CLess, Syntax.Cursorless.Hole_vd_CLess ) ->
            True

        _ ->
            False


same_fd_CLess : Syntax.Cursorless.Fd_CLess -> Syntax.Cursorless.Fd_CLess -> Bool
same_fd_CLess query op =
    case ( query, op ) of
        ( Syntax.Cursorless.Fundecl1_CLess _ _ _ _, Syntax.Cursorless.Fundecl1_CLess _ _ _ _ ) ->
            True

        ( Syntax.Cursorless.Fundecl2_CLess _ _ _ _ _, Syntax.Cursorless.Fundecl2_CLess _ _ _ _ _ ) ->
            True

        ( Syntax.Cursorless.Fundecldone_CLess, Syntax.Cursorless.Fundecldone_CLess ) ->
            True

        ( Syntax.Cursorless.Hole_fd_CLess, Syntax.Cursorless.Hole_fd_CLess ) ->
            True

        _ ->
            False


same_s_CLess : Syntax.Cursorless.S_CLess -> Syntax.Cursorless.S_CLess -> Bool
same_s_CLess query op =
    case ( query, op ) of
        ( Syntax.Cursorless.Assignment_CLess _ _, Syntax.Cursorless.Assignment_CLess _ _ ) ->
            True

        ( Syntax.Cursorless.Stmtfuncall_CLess _ _, Syntax.Cursorless.Stmtfuncall_CLess _ _ ) ->
            True

        ( Syntax.Cursorless.Return_CLess _, Syntax.Cursorless.Return_CLess _ ) ->
            True

        ( Syntax.Cursorless.Conditional_CLess _, Syntax.Cursorless.Conditional_CLess _ ) ->
            True

        ( Syntax.Cursorless.Compstmt_CLess _ _, Syntax.Cursorless.Compstmt_CLess _ _ ) ->
            True

        ( Syntax.Cursorless.Hole_s_CLess, Syntax.Cursorless.Hole_s_CLess ) ->
            True

        _ ->
            False


same_fa_CLess : Syntax.Cursorless.Fa_CLess -> Syntax.Cursorless.Fa_CLess -> Bool
same_fa_CLess query op =
    case ( query, op ) of
        ( Syntax.Cursorless.Funarg_CLess _ _, Syntax.Cursorless.Funarg_CLess _ _ ) ->
            True

        ( Syntax.Cursorless.Funargs_CLess _ _ _, Syntax.Cursorless.Funargs_CLess _ _ _ ) ->
            True

        ( Syntax.Cursorless.Hole_fa_CLess, Syntax.Cursorless.Hole_fa_CLess ) ->
            True

        _ ->
            False


same_cond_CLess :
    Syntax.Cursorless.Cond_CLess -> Syntax.Cursorless.Cond_CLess -> Bool
same_cond_CLess query op =
    case ( query, op ) of
        ( Syntax.Cursorless.Ifelse_CLess _ _ _, Syntax.Cursorless.Ifelse_CLess _ _ _ ) ->
            True

        ( Syntax.Cursorless.Hole_cond_CLess, Syntax.Cursorless.Hole_cond_CLess ) ->
            True

        _ ->
            False


same_t_CLess : Syntax.Cursorless.T_CLess -> Syntax.Cursorless.T_CLess -> Bool
same_t_CLess query op =
    case ( query, op ) of
        ( Syntax.Cursorless.Tint_CLess, Syntax.Cursorless.Tint_CLess ) ->
            True

        ( Syntax.Cursorless.Tchar_CLess, Syntax.Cursorless.Tchar_CLess ) ->
            True

        ( Syntax.Cursorless.Tbool_CLess, Syntax.Cursorless.Tbool_CLess ) ->
            True

        ( Syntax.Cursorless.Hole_t_CLess, Syntax.Cursorless.Hole_t_CLess ) ->
            True

        _ ->
            False


same_e_CLess : Syntax.Cursorless.E_CLess -> Syntax.Cursorless.E_CLess -> Bool
same_e_CLess query op =
    case ( query, op ) of
        ( Syntax.Cursorless.Int_CLess _, Syntax.Cursorless.Int_CLess _ ) ->
            True

        ( Syntax.Cursorless.Char_CLess _, Syntax.Cursorless.Char_CLess _ ) ->
            True

        ( Syntax.Cursorless.Bool_CLess _, Syntax.Cursorless.Bool_CLess _ ) ->
            True

        ( Syntax.Cursorless.Plus_CLess _ _, Syntax.Cursorless.Plus_CLess _ _ ) ->
            True

        ( Syntax.Cursorless.Equals_CLess _ _, Syntax.Cursorless.Equals_CLess _ _ ) ->
            True

        ( Syntax.Cursorless.Expfuncall_CLess _ _, Syntax.Cursorless.Expfuncall_CLess _ _ ) ->
            True

        ( Syntax.Cursorless.Expident_CLess _, Syntax.Cursorless.Expident_CLess _ ) ->
            True

        ( Syntax.Cursorless.Hole_e_CLess, Syntax.Cursorless.Hole_e_CLess ) ->
            True

        _ ->
            False


same_id_CLess : Syntax.Cursorless.Id_CLess -> Syntax.Cursorless.Id_CLess -> Bool
same_id_CLess query op =
    case ( query, op ) of
        ( Syntax.Cursorless.Ident_CLess _, Syntax.Cursorless.Ident_CLess _ ) ->
            True

        ( Syntax.Cursorless.Hole_id_CLess, Syntax.Cursorless.Hole_id_CLess ) ->
            True

        _ ->
            False