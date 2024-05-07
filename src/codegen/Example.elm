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
        createBaseSyntaxSorts
            (addCursorHoleOps <| exampleSyntax)
            ++ createCursorlessSyntaxSorts exampleSyntax
            ++ (fromCLessToCCtxSyntaxSorts <| createCursorlessSyntax exampleSyntax)
            ++ (fromCLessToWellFormed <| createCursorlessSyntax exampleSyntax)
            ++ [ createBindType, createToCLessFun <| addCursorHoleOps <| exampleSyntax ]
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
type Wellformed
    = Root_s_CLess S_CLess
    | Root_e_CLess E_CLess
    | Let_CLess_cursor1 E_CLess (Bind E_CLess S_CLess)
    | Let_CLess_cursor2 E_CLess (Bind E_CLess S_CLess)
    | Exp_CLess_cursor1 E_CLess
    | Plus_CLess_cursor1 E_CLess E_CLess
    | Plus_CLess_cursor2 E_CLess E_CLess


type WellFormedSyntax
    = S_CLess_WellFormed S_CLess
    | E_CLess_WellFormed E_CLess
    | Wellformed_WellFormed Wellformed


{-| Cursor context / CCtx / C sorts and operators
-}
type Cctx
    = Hole
    | Let_CLess_cctx1 Cctx (Bind E_CLess S_CLess)
    | Let_CLess_cctx2 E_CLess (Bind E_CLess Cctx)
    | Exp_CLess_cctx1 Cctx
    | Plus_CLess_cctx1 Cctx E_CLess
    | Plus_CLess_cctx2 E_CLess Cctx


type CctxSyntax
    = S_CLess_CCtx S_CLess
    | E_CLess_CCtx E_CLess
    | Cctx_CCtx Cctx


{-| Cursorless sorts and operators
-}
type S_CLess
    = Let_CLess E_CLess (Bind E_CLess S_CLess)
    | Exp_CLess E_CLess
    | Hole_s_CLess


type E_CLess
    = Plus_CLess E_CLess E_CLess
    | Num_CLess
    | Var_CLess
    | Hole_e_CLess


type CursorlessSyntax
    = S_CLess S_CLess
    | E_CLess E_CLess


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


type BaseSyntax
    = S S
    | E E


type alias Bind a b =
    ( List a, b )



-- toWellFormed : List Int -> BaseSyntax -> Maybe ( CctxSyntax, WellFormedSyntax )
-- toWellFormed pos syntax =
--     case ( pos, syntax ) of
--         ( [], _ ) ->
--             case syntax of
--                 S s ->
--                     Just ( Cctx_CCtx Hole, Root_s_CLess s )
-- toCursorLessOp : BaseSyntax -> Maybe CursorlessSyntax
-- toCursorLessOp syntax =
--     case syntax of
--         S s ->
--             Just (S_CLess s)
--         E e ->
--             Just (E_CLess e)
--         _ ->
--             Nothing
