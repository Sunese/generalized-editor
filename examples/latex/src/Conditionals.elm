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
                ( Syntax.Cursorless.D_CLess query, Syntax.Cursorless.D_CLess op ) ->
                    same_d_CLess query op

                ( Syntax.Cursorless.Id_CLess query, Syntax.Cursorless.Id_CLess op ) ->
                    same_id_CLess query op

                ( Syntax.Cursorless.E_CLess query, Syntax.Cursorless.E_CLess op ) ->
                    same_e_CLess query op

                ( Syntax.Cursorless.Cmd_CLess query, Syntax.Cursorless.Cmd_CLess op ) ->
                    same_cmd_CLess query op

                ( Syntax.Cursorless.A_CLess query, Syntax.Cursorless.A_CLess op ) ->
                    same_a_CLess query op

                ( Syntax.Cursorless.C_CLess query, Syntax.Cursorless.C_CLess op ) ->
                    same_c_CLess query op

                _ ->
                    False


getOpAtCursor : Decomposed -> Syntax.Cursorless.CursorLess
getOpAtCursor decomposed =
    let
        ( cctx, wellformed ) =
            decomposed
    in
    case wellformed of
        Syntax.Wellformed.Root_d_CLess arg1 ->
            Syntax.Cursorless.D_CLess arg1

        Syntax.Wellformed.Root_e_CLess arg1 ->
            Syntax.Cursorless.E_CLess arg1

        Syntax.Wellformed.Root_cmd_CLess arg1 ->
            Syntax.Cursorless.Cmd_CLess arg1

        Syntax.Wellformed.Root_a_CLess arg1 ->
            Syntax.Cursorless.A_CLess arg1

        Syntax.Wellformed.Root_id_CLess arg1 ->
            Syntax.Cursorless.Id_CLess arg1

        Syntax.Wellformed.Root_c_CLess arg1 ->
            Syntax.Cursorless.C_CLess arg1


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
        Syntax.Cursorless.D_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.Latexdoc_CLess _ _ _ _ _ ->
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

                Syntax.Cursorless.Hole_d_CLess ->
                    False

        Syntax.Cursorless.Id_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.Ident_CLess _ ->
                    False

                Syntax.Cursorless.Hole_id_CLess ->
                    False

        Syntax.Cursorless.E_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.Environment_CLess _ _ _ ->
                    (possibly
                         cursorlessop
                         (Movement.child 1 decomposed) && possibly
                                                                  cursorlessop
                                                                  (Movement.child
                                                                           2
                                                                           decomposed
                                                                  )
                    ) && possibly cursorlessop (Movement.child 3 decomposed)

                Syntax.Cursorless.Hole_e_CLess ->
                    False

        Syntax.Cursorless.Cmd_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.Command_CLess _ _ ->
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

        Syntax.Cursorless.A_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.Argument_CLess _ ->
                    possibly cursorlessop (Movement.child 1 decomposed)

                Syntax.Cursorless.Hole_a_CLess ->
                    False

        Syntax.Cursorless.C_CLess arg1 ->
            case arg1 of
                Syntax.Cursorless.TextContent_CLess _ ->
                    False

                Syntax.Cursorless.CmdContent_CLess _ ->
                    possibly cursorlessop (Movement.child 1 decomposed)

                Syntax.Cursorless.EnvContent_CLess _ ->
                    possibly cursorlessop (Movement.child 1 decomposed)

                Syntax.Cursorless.SeqContent_CLess _ _ ->
                    possibly
                        cursorlessop
                        (Movement.child 1 decomposed) && possibly
                                                                 cursorlessop
                                                                 (Movement.child
                                                                          2
                                                                          decomposed
                                                                 )

                Syntax.Cursorless.Hole_c_CLess ->
                    False


same_d_CLess : Syntax.Cursorless.D_CLess -> Syntax.Cursorless.D_CLess -> Bool
same_d_CLess query op =
    case ( query, op ) of
        ( Syntax.Cursorless.Latexdoc_CLess _ _ _ _ _, Syntax.Cursorless.Latexdoc_CLess _ _ _ _ _ ) ->
            True

        ( Syntax.Cursorless.Hole_d_CLess, Syntax.Cursorless.Hole_d_CLess ) ->
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


same_e_CLess : Syntax.Cursorless.E_CLess -> Syntax.Cursorless.E_CLess -> Bool
same_e_CLess query op =
    case ( query, op ) of
        ( Syntax.Cursorless.Environment_CLess _ _ _, Syntax.Cursorless.Environment_CLess _ _ _ ) ->
            True

        ( Syntax.Cursorless.Hole_e_CLess, Syntax.Cursorless.Hole_e_CLess ) ->
            True

        _ ->
            False


same_cmd_CLess :
    Syntax.Cursorless.Cmd_CLess -> Syntax.Cursorless.Cmd_CLess -> Bool
same_cmd_CLess query op =
    case ( query, op ) of
        ( Syntax.Cursorless.Command_CLess _ _, Syntax.Cursorless.Command_CLess _ _ ) ->
            True

        ( Syntax.Cursorless.Hole_cmd_CLess, Syntax.Cursorless.Hole_cmd_CLess ) ->
            True

        _ ->
            False


same_a_CLess : Syntax.Cursorless.A_CLess -> Syntax.Cursorless.A_CLess -> Bool
same_a_CLess query op =
    case ( query, op ) of
        ( Syntax.Cursorless.Argument_CLess _, Syntax.Cursorless.Argument_CLess _ ) ->
            True

        ( Syntax.Cursorless.Hole_a_CLess, Syntax.Cursorless.Hole_a_CLess ) ->
            True

        _ ->
            False


same_c_CLess : Syntax.Cursorless.C_CLess -> Syntax.Cursorless.C_CLess -> Bool
same_c_CLess query op =
    case ( query, op ) of
        ( Syntax.Cursorless.TextContent_CLess _, Syntax.Cursorless.TextContent_CLess _ ) ->
            True

        ( Syntax.Cursorless.CmdContent_CLess _, Syntax.Cursorless.CmdContent_CLess _ ) ->
            True

        ( Syntax.Cursorless.EnvContent_CLess _, Syntax.Cursorless.EnvContent_CLess _ ) ->
            True

        ( Syntax.Cursorless.SeqContent_CLess _ _, Syntax.Cursorless.SeqContent_CLess _ _ ) ->
            True

        ( Syntax.Cursorless.Hole_c_CLess, Syntax.Cursorless.Hole_c_CLess ) ->
            True

        _ ->
            False