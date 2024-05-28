module Main exposing (..)

-- Example of how to simulate a type class in Elm
-- First, let's define some types we are working with


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



-- We want to define a substitutatble and decomposable type class
-- with which we can substitute S for other S's
-- and E for other E's and perform decomposition of S and E


type alias Substitutable a =
    { substitute : a -> a -> a
    }


type alias Decomposable baseSyntax cursorLess cctx wellformed =
    { decompose : cursorLess -> List Int -> Maybe ( cctx, wellformed )
    , getCursorPath : baseSyntax -> List Int
    , toCLess : baseSyntax -> cursorLess
    }



-- Define a type class instance for Base


getCursorPath : List Int -> Base -> List Int
getCursorPath path base =
    case base of
        S s ->
            case s of
                Let arg1 arg2 ->
                    let
                        ( boundVars2, arg2_ ) =
                            arg2
                    in
                    getCursorPath (path ++ [ 1 ]) (E arg1)
                        ++ getCursorPath (path ++ [ 2 ]) (S arg2_)

                Exp e ->
                    getCursorPath (path ++ [ 1 ]) (E e)

                Hole_s ->
                    []

                Cursor_s s_ ->
                    path

        E e ->
            case e of
                Plus arg1 arg2 ->
                    getCursorPath (path ++ [ 1 ]) (E arg1) ++ getCursorPath (path ++ [ 2 ]) (E arg2)

                Num ->
                    []

                Var ->
                    []

                Hole_e ->
                    []

                Cursor_e e_ ->
                    path


exampleS : S
exampleS =
    Let (Plus Var Var) ( [ Var ], Exp Num )


example2S : S
example2S =
    Let (Plus Var Var) ( [ Var ], Cursor_s (Exp Num) )



-- deeper cursor


example3S : S
example3S =
    Let (Plus Var Var) ( [ Var ], Exp (Cursor_e Num) )


exampleBase : Base
exampleBase =
    S exampleS


example2Base : Base
example2Base =
    S example2S


example3Base : Base
example3Base =
    S example3S



-- getCursorPathBind : List Int -> Bind a b -> List Int
-- I want to be able to create specific branch cases
-- i.e. if there are binders in an arg to an operator,
-- I want to deconstruct the argument into (boundvars, argX)


decompose_base : Decomposable Base CursorLess Cctx Wellformed
decompose_base =
    { decompose = \cless path -> Nothing
    , getCursorPath =
        \base ->
            getCursorPath [] base
    , toCLess = \base -> E_CLess Hole_e_CLess
    }


sub_cursorless : Substitutable CursorLess
sub_cursorless =
    { substitute =
        \old new ->
            let
                sameSortSubstitution : CursorLess -> CursorLess
                sameSortSubstitution new_ =
                    case ( old, new_ ) of
                        ( S_CLess _, S_CLess newS ) ->
                            S_CLess newS

                        ( E_CLess _, E_CLess newE ) ->
                            E_CLess newE

                        _ ->
                            Debug.todo "Substituting different sorts"
            in
            sameSortSubstitution new
    }



-- Now we have a polymorphic function that can substitute
-- any type that is an instance of the Substitutable type class
-- and another polymorphic function that can decompose any type
-- that is an instance of the Decomposable type class


decompose : Decomposable a b c d -> a -> Maybe ( c, d )
decompose instance ast =
    let
        cless =
            instance.toCLess ast

        path =
            instance.getCursorPath ast
    in
    instance.decompose cless path


substitute : Substitutable a -> a -> a -> a
substitute instance old new =
    instance.substitute old new



-- Let's imagine we are somewhere in the main application now
-- and we want to perform a substitution
-- Due to Elm not supporting implicit type classes, we have to
-- pass an instance explicitly, depending on what type we are working with
-- i.e., what type the cursor is encapsulating
-- Let's say we have a cursor that is encapsulating an E:


newTree =
    substitute sub_cursorless (E_CLess Hole_e_CLess) (S_CLess Hole_s_CLess)
