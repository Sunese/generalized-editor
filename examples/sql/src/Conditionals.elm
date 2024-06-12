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
                ( Syntax.Cursorless.Q_CLess query, Syntax.Cursorless.Q_CLess op ) ->
                    same_q_CLess query op

                ( Syntax.Cursorless.Cmd_CLess query, Syntax.Cursorless.Cmd_CLess op ) ->
                    same_cmd_CLess query op

                ( Syntax.Cursorless.Id_CLess query, Syntax.Cursorless.Id_CLess op ) ->
                    same_id_CLess query op

                ( Syntax.Cursorless.Const_CLess query, Syntax.Cursorless.Const_CLess op ) ->
                    same_const_CLess query op

                ( Syntax.Cursorless.Clause_CLess query, Syntax.Cursorless.Clause_CLess op ) ->
                    same_clause_CLess query op

                ( Syntax.Cursorless.Cond_CLess query, Syntax.Cursorless.Cond_CLess op ) ->
                    same_cond_CLess query op

                ( Syntax.Cursorless.Exp_CLess query, Syntax.Cursorless.Exp_CLess op ) ->
                    same_exp_CLess query op

                _ ->
                    False


getOpAtCursor : Decomposed -> Syntax.Cursorless.CursorLess
getOpAtCursor decomposed =
    let
        ( cctx, wellformed ) =
            decomposed
    in
    case wellformed of
        Syntax.Wellformed.Root_q_CLess arg1 ->
            Syntax.Cursorless.Q_CLess arg1

        Syntax.Wellformed.Root_cmd_CLess arg1 ->
            Syntax.Cursorless.Cmd_CLess arg1

        Syntax.Wellformed.Root_id_CLess arg1 ->
            Syntax.Cursorless.Id_CLess arg1

        Syntax.Wellformed.Root_const_CLess arg1 ->
            Syntax.Cursorless.Const_CLess arg1

        Syntax.Wellformed.Root_clause_CLess arg1 ->
            Syntax.Cursorless.Clause_CLess arg1

        Syntax.Wellformed.Root_cond_CLess arg1 ->
            Syntax.Cursorless.Cond_CLess arg1

        Syntax.Wellformed.Root_exp_CLess arg1 ->
            Syntax.Cursorless.Exp_CLess arg1


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
        Syntax.Cursorless.Q_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.Select_CLess _ _ _ ->
                    (possibly
                         cursorlessop
                         (Movement.child 1 decomposed) && possibly
                                                                  cursorlessop
                                                                  (Movement.child
                                                                           2
                                                                           decomposed
                                                                  )
                    ) && possibly cursorlessop (Movement.child 3 decomposed)

                Syntax.Cursorless.Hole_q_CLess ->
                    False

        Syntax.Cursorless.Cmd_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.Insert_CLess _ _ ->
                    possibly
                        cursorlessop
                        (Movement.child 1 decomposed) && possibly
                                                                 cursorlessop
                                                                 (Movement.child
                                                                          2
                                                                          decomposed
                                                                 )

                Syntax.Cursorless.Hole_cmd_CLess ->
                    False

        Syntax.Cursorless.Id_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.Ident_CLess _ ->
                    False

                Syntax.Cursorless.Hole_id_CLess ->
                    False

        Syntax.Cursorless.Const_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.Num_CLess _ ->
                    False

                Syntax.Cursorless.Str_CLess _ ->
                    False

                Syntax.Cursorless.Hole_const_CLess ->
                    False

        Syntax.Cursorless.Clause_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.Where_CLess _ ->
                    possibly cursorlessop (Movement.child 1 decomposed)

                Syntax.Cursorless.Having_CLess _ ->
                    possibly cursorlessop (Movement.child 1 decomposed)

                Syntax.Cursorless.Hole_clause_CLess ->
                    False

        Syntax.Cursorless.Cond_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.Greater_CLess _ _ ->
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

                Syntax.Cursorless.Hole_cond_CLess ->
                    False

        Syntax.Cursorless.Exp_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.Econst_CLess _ ->
                    possibly cursorlessop (Movement.child 1 decomposed)

                Syntax.Cursorless.Eident_CLess _ ->
                    possibly cursorlessop (Movement.child 1 decomposed)

                Syntax.Cursorless.Hole_exp_CLess ->
                    False


same_q_CLess : Syntax.Cursorless.Q_CLess -> Syntax.Cursorless.Q_CLess -> Bool
same_q_CLess query op =
    case ( query, op ) of
        ( Syntax.Cursorless.Select_CLess _ _ _, Syntax.Cursorless.Select_CLess _ _ _ ) ->
            True

        ( Syntax.Cursorless.Hole_q_CLess, Syntax.Cursorless.Hole_q_CLess ) ->
            True

        _ ->
            False


same_cmd_CLess :
    Syntax.Cursorless.Cmd_CLess -> Syntax.Cursorless.Cmd_CLess -> Bool
same_cmd_CLess query op =
    case ( query, op ) of
        ( Syntax.Cursorless.Insert_CLess _ _, Syntax.Cursorless.Insert_CLess _ _ ) ->
            True

        ( Syntax.Cursorless.Hole_cmd_CLess, Syntax.Cursorless.Hole_cmd_CLess ) ->
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


same_const_CLess :
    Syntax.Cursorless.Const_CLess -> Syntax.Cursorless.Const_CLess -> Bool
same_const_CLess query op =
    case ( query, op ) of
        ( Syntax.Cursorless.Num_CLess _, Syntax.Cursorless.Num_CLess _ ) ->
            True

        ( Syntax.Cursorless.Str_CLess _, Syntax.Cursorless.Str_CLess _ ) ->
            True

        ( Syntax.Cursorless.Hole_const_CLess, Syntax.Cursorless.Hole_const_CLess ) ->
            True

        _ ->
            False


same_clause_CLess :
    Syntax.Cursorless.Clause_CLess -> Syntax.Cursorless.Clause_CLess -> Bool
same_clause_CLess query op =
    case ( query, op ) of
        ( Syntax.Cursorless.Where_CLess _, Syntax.Cursorless.Where_CLess _ ) ->
            True

        ( Syntax.Cursorless.Having_CLess _, Syntax.Cursorless.Having_CLess _ ) ->
            True

        ( Syntax.Cursorless.Hole_clause_CLess, Syntax.Cursorless.Hole_clause_CLess ) ->
            True

        _ ->
            False


same_cond_CLess :
    Syntax.Cursorless.Cond_CLess -> Syntax.Cursorless.Cond_CLess -> Bool
same_cond_CLess query op =
    case ( query, op ) of
        ( Syntax.Cursorless.Greater_CLess _ _, Syntax.Cursorless.Greater_CLess _ _ ) ->
            True

        ( Syntax.Cursorless.Equals_CLess _ _, Syntax.Cursorless.Equals_CLess _ _ ) ->
            True

        ( Syntax.Cursorless.Hole_cond_CLess, Syntax.Cursorless.Hole_cond_CLess ) ->
            True

        _ ->
            False


same_exp_CLess :
    Syntax.Cursorless.Exp_CLess -> Syntax.Cursorless.Exp_CLess -> Bool
same_exp_CLess query op =
    case ( query, op ) of
        ( Syntax.Cursorless.Econst_CLess _, Syntax.Cursorless.Econst_CLess _ ) ->
            True

        ( Syntax.Cursorless.Eident_CLess _, Syntax.Cursorless.Eident_CLess _ ) ->
            True

        ( Syntax.Cursorless.Hole_exp_CLess, Syntax.Cursorless.Hole_exp_CLess ) ->
            True

        _ ->
            False