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


toCLess : Base -> CursorLess
toCLess base =
    case base of
        S s ->
            S_CLess (toCLess_s s)

        E e ->
            E_CLess (toCLess_e e)


toCLess_s : S -> S_CLess
toCLess_s s =
    case s of
        Let arg1 ( boundVars2, arg2 ) ->
            Let_CLess (toCLess_e arg1) ( List.map toCLess_e boundVars2, toCLess_s arg2 )

        Exp arg1 ->
            Exp_CLess (toCLess_e arg1)

        Hole_s ->
            Hole_s_CLess

        Cursor_s arg1 ->
            Debug.todo "Not wellformed"


toCLess_e : E -> E_CLess
toCLess_e e =
    case e of
        Plus arg1 arg2 ->
            Plus_CLess (toCLess_e arg1) (toCLess_e arg2)

        Num ->
            Num_CLess

        Var ->
            Var_CLess

        Hole_e ->
            Hole_e_CLess

        Cursor_e arg1 ->
            Debug.todo "Not wellformed"


toCCtx_s : S -> List Int -> Cctx
toCCtx_s s path =
    case path of
        [] ->
            Cctx_hole

        i :: rest ->
            case s of
                Let arg1 ( boundVars2, arg2 ) ->
                    case i of
                        1 ->
                            Let_CLess_cctx1
                                (toCCtx_e arg1 rest)
                                -- NOTE: these could be cursors
                                ( List.map toCLess_e boundVars2, toCLess_s arg2 )

                        2 ->
                            Let_CLess_cctx2
                                (toCLess_e arg1)
                                ( List.map toCLess_e boundVars2, toCCtx_s arg2 rest )

                        _ ->
                            Debug.todo "Invalid path"

                Exp arg1 ->
                    case i of
                        1 ->
                            Exp_CLess_cctx1 (toCCtx_e arg1 rest)

                        _ ->
                            Debug.todo "Invalid path"

                Hole_s ->
                    -- If we hit a hole and the path is not empty,
                    -- then we are not wellformed
                    Debug.todo "Invalid path"

                Cursor_s _ ->
                    Cctx_hole


toCCtx_e : E -> List Int -> Cctx
toCCtx_e e path =
    case path of
        [] ->
            Cctx_hole

        i :: rest ->
            case e of
                Plus arg1 arg2 ->
                    case i of
                        1 ->
                            Plus_CLess_cctx1 (toCCtx_e arg1 rest) (toCLess_e arg2)

                        2 ->
                            Plus_CLess_cctx2 (toCLess_e arg1) (toCCtx_e arg2 rest)

                        _ ->
                            Debug.todo "Invalid path"

                Num ->
                    -- If we hit a number and the path is not empty,
                    -- then we are not wellformed
                    Debug.todo "Invalid path"

                Var ->
                    -- If we hit a variable and the path is not empty,
                    -- then we are not wellformed
                    Debug.todo "Invalid path"

                Hole_e ->
                    -- If we hit a hole and the path is not empty,
                    -- then we are not wellformed
                    Debug.todo "Invalid path"

                Cursor_e _ ->
                    Cctx_hole


toCCtx : Base -> List Int -> Cctx
toCCtx base path =
    case base of
        S s ->
            toCCtx_s s path

        E e ->
            toCCtx_e e path


decompose : Base -> List Int -> Maybe ( Cctx, Wellformed )
decompose base path =
    case base of
        S s ->
            case path of
                [] ->
                    Just ( toCCtx (S s) path, Root_s_CLess (toCLess_s s) )

                i :: rest ->
                    Debug.todo "Not implemented"

        _ ->
            Debug.todo "Not implemented"


getCursorPath : List Int -> Base -> List Int
getCursorPath path base =
    case base of
        S s ->
            case s of
                Let arg1 ( boundVars2, arg2 ) ->
                    getCursorPath (path ++ [ 1 ]) (E arg1)
                        ++ getCursorPath
                            (path
                                ++ [ 2
                                   ]
                            )
                            (S arg2)

                Exp arg1 ->
                    getCursorPath (path ++ [ 1 ]) (E arg1)

                Hole_s ->
                    []

                Cursor_s _ ->
                    path

        E e ->
            case e of
                Plus arg1 arg2 ->
                    getCursorPath (path ++ [ 1 ]) (E arg1)
                        ++ getCursorPath
                            (path
                                ++ [ 2
                                   ]
                            )
                            (E arg2)

                Num ->
                    []

                Var ->
                    []

                Hole_e ->
                    []

                Cursor_e _ ->
                    path
