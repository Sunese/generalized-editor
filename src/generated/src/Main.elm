module Main exposing (..)


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


type alias Bind a b =
    ( List a, b )


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


toCLess_s : S -> S_CLess
toCLess_s s =
    case s of
        Let arg1 ( boundVars2, arg2 ) ->
            Let_CLess
                (toCLess_e arg1)
                ( List.map toCLess_e boundVars2, toCLess_s arg2 )

        Exp arg1 ->
            Exp_CLess (toCLess_e arg1)

        Hole_s ->
            Hole_s_CLess

        Cursor_s cursor ->
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

        Cursor_e cursor ->
            Debug.todo "Not wellformed"


toCLess : Base -> CursorLess
toCLess base =
    case base of
        S arg1 ->
            S_CLess (toCLess_s arg1)

        E arg1 ->
            E_CLess (toCLess_e arg1)


toCCtx_s : S -> List Int -> ( Cctx, Base )
toCCtx_s s path =
    case path of
        [] ->
            ( Cctx_hole, S s )

        i :: rest ->
            case s of
                Let arg1 ( boundVars2, arg2 ) ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_s arg2 rest
                            in
                            ( Let_CLess_cctx1
                                cctxChild
                                ( List.map toCLess_e boundVars2
                                , toCLess_s arg2
                                )
                            , restTree
                            )

                        2 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_s arg2 rest
                            in
                            ( Let_CLess_cctx2
                                (toCLess_e arg1)
                                ( List.map toCLess_e boundVars2, cctxChild )
                            , restTree
                            )

                        _ ->
                            Debug.todo "Invalid path"

                Exp arg1 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_e arg1 rest
                            in
                            ( Exp_CLess_cctx1 cctxChild, restTree )

                        _ ->
                            Debug.todo "Invalid path"

                Hole_s ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cursor_s _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx_e : E -> List Int -> ( Cctx, Base )
toCCtx_e e path =
    case path of
        [] ->
            ( Cctx_hole, E e )

        i :: rest ->
            case e of
                Plus arg1 arg2 ->
                    case i of
                        1 ->
                            let
                                ( cctxChild, restTree ) =
                                    toCCtx_e arg2 rest
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

                Num ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Var ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Hole_e ->
                    Debug.todo
                        "Invalid path: we hit a 0-arity operator but path list is non-empty"

                Cursor_e _ ->
                    Debug.todo
                        "Invalid path: we hit a cursor but path list is non-empty"


toCCtx : Base -> List Int -> ( Cctx, Base )
toCCtx base path =
    case base of
        S arg1 ->
            toCCtx_s arg1 path

        E arg1 ->
            toCCtx_e arg1 path


toWellformed : Base -> Wellformed
toWellformed base =
    case consumeCursor base of
        S arg1 ->
            Root_s_CLess (toCLess_s arg1)

        E arg1 ->
            Root_e_CLess (toCLess_e arg1)


consumeCursor : Base -> Base
consumeCursor base =
    case base of
        S arg1 ->
            case arg1 of
                Cursor_s underCursor ->
                    S underCursor

                _ ->
                    S arg1

        E arg1 ->
            case arg1 of
                Cursor_e underCursor ->
                    E underCursor

                _ ->
                    E arg1


decompose : Base -> ( Cctx, Wellformed )
decompose base =
    let
        ( cctx, rest ) =
            toCCtx base (getCursorPath [] base)
    in
    ( cctx, toWellformed rest )
