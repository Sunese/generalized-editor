module Example exposing (..)

import Elm
import Elm.Annotation exposing (..)
import Parser exposing (..)
import Syntax exposing (..)



-- exampleFiles : List Elm.File
-- exampleFiles =
--     [ Elm.file [ "Ops" ] <| getTypeDecls exampleAddOperators
--     , Elm.file [ "Ops_CLess" ] <| getTypeDecls <| addHoleOps <| exampleSyntax
--     , Elm.file [ "Ops_CCtx" ] <| getTypeDecls <| addCCtxOp <| addCCtxSort <| addCCtxOps <| addHoleOps <| exampleSyntax
--     , Elm.file [ "Ops_Wellformed" ] <|
--         getTypeDecls <|
--             addCursorSortAndOps <|
--                 addHoleOps <|
--                     exampleSyntax
--     ]


exampleFiles : List Elm.File
exampleFiles =
    [ Elm.file [ "Main" ] <|
        createBaseSyntaxSorts exampleSyntax
            ++ createCursorlessSyntaxSorts exampleSyntax
            ++ (fromCLessToCCtxSyntaxSorts <| createCursorlessSyntax exampleSyntax)
            ++ (fromCLessToWellFormed <| createCursorlessSyntax exampleSyntax)
            ++ [ createBindType ]
    ]


exampleSynCats : List SynCat
exampleSynCats =
    [ { exp = "s", set = "Stmt" }
    , { exp = "e", set = "Exp" }
    ]


exampleSynCatRules : List SynCatOps
exampleSynCatRules =
    [ { ops =
            [ { term = "\"let \" x \" = \" e \" in \" s"
              , arity = [ ( [], "e" ), ( [ "e" ], "s" ) ]
              , name = "let"
              , synCat = "s"
              }
            , { term = "e"
              , arity = [ ( [], "e" ) ]
              , name = "exp"
              , synCat = "s"
              }
            ]
      , synCat = "s"
      }
    , { ops =
            [ { term = "e_1 \" + \" e_2"
              , arity = [ ( [], "e" ), ( [], "e" ) ]
              , name = "plus"
              , synCat = "e"
              }
            , { term = "n"
              , arity = []
              , name = "num"
              , synCat = "e"
              }
            , { term = "x"
              , arity = []
              , name = "var"
              , synCat = "e"
              }
            ]
      , synCat = "e"
      }
    ]


exampleSyntax : Syntax
exampleSyntax =
    { synCats = exampleSynCats
    , synCatOps = exampleSynCatRules
    }


exampleAddOperators : Syntax
exampleAddOperators =
    addCursorHoleOps exampleSyntax



-- Cursor stuff


{-| Well-formed sorts and operators
-}
type S_WellFormed
    = Let_WellFormed E_WellFormed (Bind E_WellFormed S_WellFormed)
    | Exp_WellFormed E_WellFormed
    | Hole_s_WellFormed


type E_WellFormed
    = Plus_WellFormed E_WellFormed E_WellFormed
    | Num_WellFormed
    | Var_WellFormed
    | Hole_e_WellFormed


type WellF
    = RootCursor_s_WellFormed S_WellFormed
    | RootCursor_e_WellFormed E_WellFormed
    | Let_cursor_arg_1_WellFormed E_WellFormed (Bind E_WellFormed S_WellFormed)
    | Let_cursor_arg_2_WellFormed E_WellFormed (Bind E_WellFormed S_WellFormed)
    | Exp_cursor_arg_1_WellFormed E_WellFormed
    | Plus_cursor_arg_1_WellFormed E_WellFormed E_WellFormed
    | Plus_cursor_arg_2_WellFormed E_WellFormed E_WellFormed


type alias WellFormed a =
    { getPathToCursor : a -> List Int
    }


{-| Cursor context / CCtx / C sorts and operators
-}
type S_CCtx
    = Let_CCtx E_CCtx (Bind E_CCtx S_CCtx)
    | Exp_CCtx E_CCtx
    | Hole_s_CCtx


type E_CCtx
    = Plus_CCtx E_CCtx E_CCtx
    | Num_CCtx
    | Var_CCtx
    | Hole_e_CCtx


type Cctx
    = Let_cctx1 Cctx (Bind E S)
    | Let_cctx2 E (Bind E Cctx)
    | Exp_cctx1 Cctx
    | Plus_cctx1 Cctx E
    | Plus_cctx2 E Cctx
    | CctxHole


type alias CursorContext a =
    { toWellFormed : a -> WellF
    }


{-| Cursorless sorts and operators
-}
type S_Cless
    = Let_Cless E_Cless (Bind E_Cless S_Cless)
    | Exp_Cless E_Cless
    | Hole_s_Cless


type E_Cless
    = Plus_Cless E_Cless E_Cless
    | Num_Cless
    | Var_Cless
    | Hole_e_Clessi


type alias CursorLess a =
    { toWellFormed : a -> List Int -> Maybe ( CursorContext a, WellFormed a )
    }



-- cursorless_S : CursorLess S
-- cursorless_S =
--     { toWellFormed = \_ -> RootCursor_s_WellFormed Hole_s_WellFormed }


{-| "Normal"/initial/clean sorts and operators
-}
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


type alias BaseSyntax a =
    { toCursorLess : a -> CursorLess a
    }



-- toCursorLess_S : S -> CursorLess S
-- toCursorLess_S s =
--     case s of
--         Let e1_ ( _, s_ ) ->
--             { toWellFormed = \_ -> Let_cursor_arg_1_WellFormed e1_ ( e1_, s_ ) }
--         Exp e_ ->
--             { toWellFormed = \_ -> Exp_cursor_arg_1_WellFormed e_ }
--         Hole_s ->
--             { toWellFormed = \_ -> RootCursor_s_WellFormed Hole_s_WellFormed }
--         Cursor_s s_ ->
--             { toWellFormed = \_ -> RootCursor_s_WellFormed Hole_s_WellFormed }
-- baseSyntax_E : BaseSyntax E
-- baseSyntax_E =
--     { toCursorLess = cursorless_E }


type alias Bind a b =
    ( List a, b )



-- getCursorPath : List Int -> Ast -> List Int
-- getCursorPath path ast =
--     case ast of
--         S s ->
--             case s of
--                 Let e1_ ( _, s_ ) ->
--                     getCursorPath (path ++ [ 1 ]) (E e1_)
--                         ++ getCursorPath (path ++ [ 2 ]) (S s_)
--                 Exp e_ ->
--                     getCursorPath (path ++ [ 1 ]) (E e_)
--                 Hole_s ->
--                     []
--                 Cursor_s _ ->
--                     path
--         E e ->
--             case e of
--                 Plus e1 e2 ->
--                     getCursorPath (path ++ [ 1 ]) (E e1)
--                         ++ getCursorPath (path ++ [ 2 ]) (E e2)
--                 Num ->
--                     []
--                 Var ->
--                     []
--                 Hole_e ->
--                     []
--                 Cursor_e e_ ->
--                     path


emptyWellFormed : ( Cctx, WellF )
emptyWellFormed =
    ( CctxHole, RootCursor_s_WellFormed Hole_s_WellFormed )



-- toWellFormed : List Int -> S_Cless -> ( Cctx, Maybe WellF )
-- toWellFormed path stmt =
--     case ( path, stmt ) of
--         ( [], _ ) ->
--             ( CctxHole, Maybe.map RootCursor_s_WellFormed (Tuple.second (toWellFormed path stmt)) )
--         ( _, _ ) ->
--             Nothing
