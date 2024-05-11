module Main exposing (..)


hello : String
hello =
    "Hello, World!"


type S
    = Let E (Bind E S)
    | Exp E
    | Hole_s
    | Cursor_s S


type E
    = Plus E E
    | Num
    | Var
    | Hole_e
    | Cursor_e E


type Base
    = S S
    | E E


type S_CLess
    = Let_CLess E_CLess (Bind E_CLess S_CLess)
    | Exp_CLess E_CLess
    | Hole_s_CLess


type E_CLess
    = Plus_CLess E_CLess E_CLess
    | Num_CLess
    | Var_CLess
    | Hole_e_CLess


type CursorLess
    = S_CLess S_CLess
    | E_CLess E_CLess


type Cctx
    = Cctx_hole
    | Let_CLess_cctx1 Cctx (Bind E_CLess S_CLess)
    | Let_CLess_cctx2 E_CLess (Bind E_CLess Cctx)
    | Exp_CLess_cctx1 Cctx
    | Plus_CLess_cctx1 Cctx E_CLess
    | Plus_CLess_cctx2 E_CLess Cctx


type Wellformed
    = Root_s_CLess S_CLess
    | Root_e_CLess E_CLess
    | Let_CLess_cursor1 E_CLess (Bind E_CLess S_CLess)
    | Let_CLess_cursor2 E_CLess (Bind E_CLess S_CLess)
    | Exp_CLess_cursor1 E_CLess
    | Plus_CLess_cursor1 E_CLess E_CLess
    | Plus_CLess_cursor2 E_CLess E_CLess


type alias Bind a b =
    ( List a, b )


toCLessS : S -> S_CLess
toCLessS s =
    case s of
        Let arg1 ( boundVars2, arg2 ) ->
            Let_CLess (toCLessE arg1) ( List.map toCLessE boundVars2, toCLessS arg2 )

        Exp arg1 ->
            Exp_CLess (toCLessE arg1)

        Hole_s ->
            Hole_s_CLess

        Cursor_s arg1 ->
            Debug.todo ""


toCLessE : E -> E_CLess
toCLessE e =
    case e of
        Plus arg1 arg2 ->
            Plus_CLess (toCLessE arg1) (toCLessE arg2)

        Num ->
            Num_CLess

        Var ->
            Var_CLess

        Hole_e ->
            Debug.todo ""

        Cursor_e arg1 ->
            Debug.todo ""


toCCtx : Base -> List Int -> Cctx
toCCtx base path =
    case path of
        [] ->
            Cctx_hole

        i :: rest ->
            case base of
                S s ->
                    case s of
                        Let arg1 ( boundVars2, arg2 ) ->
                            case i of
                                1 ->
                                    Let_CLess_cctx1
                                        (toCCtx (E arg1) rest)
                                        -- NOTE: these could be cursors
                                        ( List.map toCLessE boundVars2, toCLessS arg2 )

                                _ ->
                                    Debug.todo ""

                        _ ->
                            Debug.todo ""

                _ ->
                    Debug.todo ""


decompose : Base -> List Int -> Maybe ( Cctx, Wellformed )
decompose base path =
    case base of
        S s ->
            case path of
                [] ->
                    Just ( toCCtx (S s) path, Root_s_CLess (toCLessS s) )

                1 :: rest ->
                    Debug.todo ""

                _ ->
                    Nothing

        _ ->
            Debug.todo ""