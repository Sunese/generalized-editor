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
                                    toCCtx_e arg1 rest
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


replaceCctxHole : Int -> Cctx -> CursorLess -> Cctx
replaceCctxHole i orig_cctx underCursor =
    case orig_cctx of
        Cctx_hole ->
            case underCursor of
                S_CLess underCursor0 ->
                    case underCursor0 of
                        Let_CLess arg1 ( boundVars2, arg2 ) ->
                            case i of
                                1 ->
                                    Let_CLess_cctx1 Cctx_hole ( boundVars2, arg2 )

                                2 ->
                                    Let_CLess_cctx2
                                        arg1
                                        ( boundVars2, Cctx_hole )

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Exp_CLess arg1 ->
                            case i of
                                1 ->
                                    Exp_CLess_cctx1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Hole_s_CLess ->
                            Debug.todo "Invalid replacement"

                E_CLess underCursor0 ->
                    case underCursor0 of
                        Plus_CLess arg1 arg2 ->
                            case i of
                                1 ->
                                    Plus_CLess_cctx1 Cctx_hole arg2

                                2 ->
                                    Plus_CLess_cctx2 arg1 Cctx_hole

                                _ ->
                                    Debug.todo "Invalid arg position"

                        Num_CLess ->
                            Debug.todo "Invalid replacement"

                        Var_CLess ->
                            Debug.todo "Invalid replacement"

                        Hole_e_CLess ->
                            Debug.todo "Invalid replacement"

        Let_CLess_cctx1 arg1 ( boundVars2, arg2 ) ->
            Let_CLess_cctx1
                (replaceCctxHole i arg1 underCursor)
                ( boundVars2, arg2 )

        Let_CLess_cctx2 arg1 ( boundVars2, arg2 ) ->
            Let_CLess_cctx2
                arg1
                ( boundVars2, replaceCctxHole i arg2 underCursor )

        Exp_CLess_cctx1 cctx ->
            Exp_CLess_cctx1 (replaceCctxHole i cctx underCursor)

        Plus_CLess_cctx1 arg1 arg2 ->
            Plus_CLess_cctx1 (replaceCctxHole i arg1 underCursor) arg2

        Plus_CLess_cctx2 arg1 arg2 ->
            Plus_CLess_cctx2 arg1 (replaceCctxHole i arg2 underCursor)


child : Int -> ( Cctx, Wellformed ) -> ( Cctx, Wellformed )
child i decomposed =
    let
        ( cctx, wellformed ) =
            decomposed
    in
    case wellformed of
        Root_s_CLess underCursor ->
            case underCursor of
                Let_CLess arg1 ( boundVars2, arg2 ) ->
                    case i of
                        1 ->
                            ( replaceCctxHole i cctx (S_CLess underCursor)
                            , Root_e_CLess arg1
                            )

                        2 ->
                            ( replaceCctxHole i cctx (S_CLess underCursor)
                            , Root_s_CLess arg2
                            )

                        _ ->
                            Debug.todo "Invalid child index"

                Exp_CLess arg1 ->
                    case i of
                        1 ->
                            ( replaceCctxHole i cctx (S_CLess underCursor)
                            , Root_e_CLess arg1
                            )

                        _ ->
                            Debug.todo "Invalid child index"

                Hole_s_CLess ->
                    Debug.todo
                        "No children at op under cursor, cannot go deeper"

        Root_e_CLess underCursor ->
            case underCursor of
                Plus_CLess arg1 arg2 ->
                    case i of
                        1 ->
                            ( replaceCctxHole i cctx (E_CLess underCursor)
                            , Root_e_CLess arg1
                            )

                        2 ->
                            ( replaceCctxHole i cctx (E_CLess underCursor)
                            , Root_e_CLess arg2
                            )

                        _ ->
                            Debug.todo "Invalid child index"

                Num_CLess ->
                    Debug.todo
                        "No children at op under cursor, cannot go deeper"

                Var_CLess ->
                    Debug.todo
                        "No children at op under cursor, cannot go deeper"

                Hole_e_CLess ->
                    Debug.todo
                        "No children at op under cursor, cannot go deeper"


example : Base
example =
    S (Let (Plus (Cursor_e (Plus Num Var)) Num) ( [ Var ], Exp Num ))



-- not generated part


getCctxPath : Cctx -> List Int -> List Int
getCctxPath cctx path =
    case cctx of
        Cctx_hole ->
            path

        Let_CLess_cctx1 cctx_ ( boundVars2, arg2 ) ->
            getCctxPath cctx_ (path ++ [ 1 ])

        Let_CLess_cctx2 arg1 ( boundVars2, cctx_ ) ->
            getCctxPath cctx_ (path ++ [ 2 ])

        Exp_CLess_cctx1 cctx_ ->
            getCctxPath cctx_ (path ++ [ 1 ])

        Plus_CLess_cctx1 cctx_ arg2 ->
            getCctxPath cctx_ (path ++ [ 1 ])

        Plus_CLess_cctx2 arg1 cctx_ ->
            getCctxPath cctx_ (path ++ [ 2 ])


moveCCtxHoleUp : Cctx -> List Int -> Maybe ( Cctx, Cctx )
moveCCtxHoleUp cctx path =
    -- where tuple is (new, removed)
    -- and path is path to cctx_hole
    case path of
        [ _, _ ] ->
            case cctx of
                Let_CLess_cctx1 cctx_ ( boundVars2, arg2 ) ->
                    Just ( Let_CLess_cctx1 Cctx_hole ( boundVars2, arg2 ), cctx_ )

                Let_CLess_cctx2 arg1 ( boundVars2, cctx_ ) ->
                    Just ( Let_CLess_cctx2 arg1 ( boundVars2, Cctx_hole ), cctx_ )

                Exp_CLess_cctx1 cctx_ ->
                    Just ( Exp_CLess_cctx1 Cctx_hole, cctx_ )

                Plus_CLess_cctx1 cctx_ arg2 ->
                    Just ( Plus_CLess_cctx1 Cctx_hole arg2, cctx_ )

                Plus_CLess_cctx2 arg1 cctx_ ->
                    Just ( Plus_CLess_cctx2 arg1 Cctx_hole, cctx_ )

                Cctx_hole ->
                    Debug.todo "Invalid cctx path"

        [ _ ] ->
            -- in this case, the hole is right under the root
            Just ( Cctx_hole, cctx )

        _ :: rest ->
            case cctx of
                Let_CLess_cctx1 cctx_ ( boundVars2, arg2 ) ->
                    moveCCtxHoleUp cctx_ rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Let_CLess_cctx1 newCctx ( boundVars2, arg2 )
                                , removedCctx
                                )
                            )

                -- Just ( newCctx, removedCctx ) ->
                --     Just ( Let_CLess_cctx1 newCctx ( boundVars2, arg2 ), removedCctx )
                -- Nothing ->
                --     Nothing
                Let_CLess_cctx2 arg1 ( boundVars2, cctx_ ) ->
                    moveCCtxHoleUp cctx_ rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Let_CLess_cctx2 arg1 ( boundVars2, newCctx )
                                , removedCctx
                                )
                            )

                Exp_CLess_cctx1 cctx_ ->
                    moveCCtxHoleUp cctx_ rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Exp_CLess_cctx1 newCctx, removedCctx )
                            )

                Plus_CLess_cctx1 cctx_ arg2 ->
                    moveCCtxHoleUp cctx_ rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Plus_CLess_cctx1 newCctx arg2, removedCctx )
                            )

                Plus_CLess_cctx2 arg1 cctx_ ->
                    moveCCtxHoleUp cctx_ rest
                        |> Maybe.map
                            (\( newCctx, removedCctx ) ->
                                ( Plus_CLess_cctx2 arg1 newCctx, removedCctx )
                            )

                Cctx_hole ->
                    Debug.todo "Invalid cctx path"

        [] ->
            Nothing


addParent : Cctx -> Wellformed -> Wellformed
addParent cctx wellformed =
    -- add the cctx as the parent of the wellformed tree,
    -- wrapped in a Root_s_CLess or Root_e_CLess
    -- replacing the cctx hole with the current root of the wellformed tree
    -- under the cursor
    case cctx of
        Cctx_hole ->
            Debug.todo "No parent"

        Let_CLess_cctx1 _ ( boundVars2, arg2 ) ->
            case wellformed of
                Root_e_CLess underCursor ->
                    Root_s_CLess (Let_CLess underCursor ( boundVars2, arg2 ))

                _ ->
                    Debug.todo "Not wellformed"

        Let_CLess_cctx2 arg1 ( boundVars2, _ ) ->
            case wellformed of
                Root_s_CLess underCursor ->
                    Root_s_CLess (Let_CLess arg1 ( boundVars2, underCursor ))

                _ ->
                    Debug.todo "Not wellformed"

        Exp_CLess_cctx1 _ ->
            case wellformed of
                Root_e_CLess underCursor ->
                    Root_s_CLess (Exp_CLess underCursor)

                _ ->
                    Debug.todo "Not wellformed"

        Plus_CLess_cctx1 _ arg2 ->
            case wellformed of
                Root_e_CLess underCursor ->
                    Root_e_CLess (Plus_CLess underCursor arg2)

                _ ->
                    Debug.todo "Not wellformed"

        Plus_CLess_cctx2 arg1 _ ->
            case wellformed of
                Root_e_CLess underCursor ->
                    Root_e_CLess (Plus_CLess arg1 underCursor)

                _ ->
                    Debug.todo "Not wellformed"


parent : ( Cctx, Wellformed ) -> Maybe ( Cctx, Wellformed )
parent decomposed =
    let
        ( cctx, wellformed ) =
            decomposed
    in
    moveCCtxHoleUp cctx (getCctxPath cctx [])
        |> Maybe.map
            (\( newCctx, removedCctx ) ->
                ( newCctx
                , addParent removedCctx wellformed
                )
            )


example2 : Base
example2 =
    S
        (Let
            (Plus
                Num
                (Cursor_e Num)
            )
            ( [ Var ]
            , Exp
                Num
            )
        )



-- decomposed


ex2_decomposed : ( Cctx, Wellformed )
ex2_decomposed =
    ( Let_CLess_cctx1
        (Plus_CLess_cctx2
            Num_CLess
            Cctx_hole
        )
        ( [ Var_CLess ]
        , Exp_CLess
            Num_CLess
        )
    , Root_e_CLess Num_CLess
    )


example3 : Base
example3 =
    S
        (Let
            (Plus Num Num)
            ( [ Var ]
            , Exp
                (Cursor_e Num)
            )
        )


exampleCCtx : Cctx
exampleCCtx =
    Let_CLess_cctx1 Cctx_hole ( [ Var_CLess ], Hole_s_CLess )


exampleCCtx2 : Cctx
exampleCCtx2 =
    Let_CLess_cctx2 (Plus_CLess Num_CLess Num_CLess) ( [ Var_CLess ], Cctx_hole )


exampleCCtx3 : Cctx
exampleCCtx3 =
    Cctx_hole
