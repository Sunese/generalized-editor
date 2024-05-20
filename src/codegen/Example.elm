module Example exposing (..)

import DecomposeFun exposing (..)
import Elm
import Elm.Annotation as Type exposing (..)
import Gen.Decomposable
import Gen.Substitutable
import GetCursorPath exposing (..)
import Movement exposing (..)
import Parser exposing (..)
import Syntax exposing (..)
import ToCCtx exposing (..)
import ToCLess exposing (..)



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
-- typeClassExample : List Elm.File
-- typeClassExample =
--     [ Elm.file [ "TypeClass" ] <|
--         [ Elm.declaration "myFun" <|
--             Elm.fn
--                 ( "myArg"
--                 , Just
--                     (Type.namedWith
--                         Gen.Substitutable.moduleName_
--                         "Substitutable"
--                         [ Type.var "a" ]
--                     )
--                 )
--                 (\arg -> arg)
--         , Elm.declaration "myFun2" <|
--             Elm.fn
--                 ( "myArg"
--                 , Just
--                     (Type.namedWith
--                         Gen.Decomposable.moduleName_
--                         "Decomposable"
--                         [ Type.named [] "E", Type.named [] "S", Type.named [] "S" ]
--                     )
--                 )
--                 (\arg -> Elm.val "\"Hello")
--         ]
--             ++ createBaseSyntaxSorts exampleSyntax
--     ]


exampleFiles : List Elm.File
exampleFiles =
    [ Elm.file [ "Main" ] <|
        createBaseSyntaxSorts
            (addCursorHoleOps <| exampleSyntax)
            ++ createCursorlessSyntaxSorts exampleSyntax
            ++ (fromCLessToCCtxSyntaxSorts <| createCursorlessSyntax exampleSyntax)
            ++ (fromCLessToWellFormedSorts <| createCursorlessSyntax exampleSyntax)
            ++ [ createBindType ]
            ++ [ createGetCursorPath <| addCursorHoleOps <| exampleSyntax ]
            ++ createToCLessFuns (addCursorHoleOps exampleSyntax)
            ++ createToCCtxFuns (addCursorHoleOps exampleSyntax)
            ++ createDecomposeFuns (addCursorHoleOps exampleSyntax)
            ++ [ createReplaceCCtxHoleFun
                    (addCursorHoleOps <| exampleSyntax)
                    (createCursorlessSyntax exampleSyntax)
                    (fromCLessToCCtxSyntax <| createCursorlessSyntax exampleSyntax)
               ]

    -- ++ [ createChildFun
    --         (addCursorHoleOps <| exampleSyntax)
    --         (createCursorlessSyntax exampleSyntax)
    --         (fromCLessToCCtxSyntax <| createCursorlessSyntax exampleSyntax)
    --         (fromCLessToWellFormedSyntax <| createCursorlessSyntax exampleSyntax)
    --    ]
    -- ++ [createDecomposeFun <| addCursorHoleOps <| exampleSyntax]
    -- ++ [ createToCursorLessFun <| addCursorHoleOps <| exampleSyntax ]
    -- ++ [ decomposableInstance <| addCursorHoleOps <| exampleSyntax ]
    -- ++ convertableInstances (addCursorHoleOps exampleSyntax)
    ]


exampleSynCats : List SynCat
exampleSynCats =
    [ { exp = "p", set = "Prog" }
    , { exp = "s", set = "Stmt" }
    , { exp = "vd", set = "VariableDecl" }
    , { exp = "fd", set = "FunDecl" }
    , { exp = "t", set = "Type" }
    , { exp = "id", set = "Id" }
    , { exp = "e", set = "Exp" }
    , { exp = "b", set = "Block" }
    , { exp = "bi", set = "BlockItem" }
    , { exp = "fa", set = "FunArg" }
    , { exp = "cond", set = "Conditional" }
    ]


exampleSynCatRules : List SynCatOps
exampleSynCatRules =
    [ { ops =
            [ { term = "fd"
              , arity = [ ( [], "fd" ) ]
              , name = "program"
              , synCat = "p"
              }
            ]
      , synCat = "p"
      }
    , { ops =
            [ { term = "bi"
              , arity = [ ( [], "bi" ) ]
              , name = "block"
              , synCat = "b"
              }
            ]
      , synCat = "b"
      }
    , { ops =
            [ { term = "vd"
              , arity = []
              , name = "blockdecls"
              , synCat = "bi"
              }
            , { term = "s"
              , arity = [ ( [], "s" ) ]
              , name = "blockstmts"
              , synCat = "bi"
              }
            , { term = ""
              , arity = []
              , name = "blockdone"
              , synCat = "bi"
              }
            ]
      , synCat = "bi"
      }
    , { ops =
            [ { term = "todo"
              , arity = [ ( [], "t" ), ( [], "e" ), ( [ "id" ], "bi" ) ]
              , name = "vardecl"
              , synCat = "vd"
              }
            ]
      , synCat = "vd"
      }
    , { ops =
            [ { term = "todo"
              , arity = [ ( [], "t" ), ( [ "id" ], "fd" ), ( [], "t" ), ( [ "id" ], "b" ) ]
              , name = "fundecl1"
              , synCat = "fd"
              }
            , { term = "todo"
              , arity = [ ( [], "t" ), ( [ "id" ], "fd" ), ( [], "t" ), ( [], "t" ), ( [ "id", "id" ], "b" ) ]
              , name = "fundecl2"
              , synCat = "fd"
              }
            ]
      , synCat = "fd"
      }
    , { ops =
            [ { term = "todo"
              , arity = [ ( [], "id" ), ( [], "e" ) ]
              , name = "assignment"
              , synCat = "s"
              }
            , { term = "todo"
              , arity = [ ( [], "id" ), ( [], "fa" ) ]
              , name = "stmtfuncall"
              , synCat = "s"
              }
            , { term = "todo"
              , arity = [ ( [], "e" ) ]
              , name = "return"
              , synCat = "s"
              }
            , { term = "todo"
              , arity = [ ( [], "cond" ) ]
              , name = "conditional"
              , synCat = "s"
              }
            , { term = "todo"
              , arity = [ ( [], "s" ), ( [], "s" ) ]
              , name = "compstmt"
              , synCat = "s"
              }
            ]
      , synCat = "s"
      }
    , { ops =
            [ { term = "todo"
              , arity = [ ( [], "t" ), ( [], "id" ) ]
              , name = "funarg"
              , synCat = "fa"
              }
            , { term = "todo"
              , arity = [ ( [], "t" ), ( [], "id" ), ( [], "fa" ) ]
              , name = "funargs"
              , synCat = "fa"
              }
            ]
      , synCat = "fa"
      }
    , { ops =
            [ { term = "todo"
              , arity = [ ( [], "e" ), ( [], "b" ), ( [], "b" ) ]
              , name = "ifelse"
              , synCat = "cond"
              }
            ]
      , synCat = "cond"
      }
    , { ops =
            [ { term = "todo"
              , arity = []
              , name = "tint"
              , synCat = "t"
              }
            , { term = "todo"
              , arity = []
              , name = "tchar"
              , synCat = "t"
              }
            , { term = "todo"
              , arity = []
              , name = "tbool"
              , synCat = "t"
              }
            ]
      , synCat = "t"
      }
    , { ops =
            [ { term = "todo"
              , arity = []
              , name = "cint"
              , synCat = "e"
              }
            , { term = "todo"
              , arity = []
              , name = "cchar"
              , synCat = "e"
              }
            , { term = "todo"
              , arity = []
              , name = "cbool"
              , synCat = "e"
              }
            , { term = "todo"
              , arity = [ ( [], "e" ), ( [], "e" ) ]
              , name = "plus"
              , synCat = "e"
              }
            , { term = "todo"
              , arity = [ ( [], "e" ), ( [], "e" ) ]
              , name = "equals"
              , synCat = "e"
              }
            , { term = "todo"
              , arity = [ ( [], "id" ), ( [], "fa" ) ]
              , name = "expfuncall"
              , synCat = "e"
              }
            , { term = "todo"
              , arity = [ ( [], "id" ) ]
              , name = "expident"
              , synCat = "e"
              }
            ]
      , synCat = "e"
      }
    , { ops =
            [ { term = "bi"
              , arity = []
              , name = "ident"
              , synCat = "id"
              }
            ]
      , synCat = "id"
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



-- -- Cursor stuff
-- {-| Well-formed sorts and operators
-- -}
-- type Wellformed
--     = Root_s_CLess S_CLess
--     | Root_e_CLess E_CLess
--     | Let_CLess_cursor1 E_CLess (Bind E_CLess S_CLess)
--     | Let_CLess_cursor2 E_CLess (Bind E_CLess S_CLess)
--     | Exp_CLess_cursor1 E_CLess
--     | Plus_CLess_cursor1 E_CLess E_CLess
--     | Plus_CLess_cursor2 E_CLess E_CLess
-- type WellFormedSyntax
--     = S_CLess_WellFormed S_CLess
--     | E_CLess_WellFormed E_CLess
--     | Wellformed_WellFormed Wellformed
-- {-| Cursor context / CCtx / C sorts and operators
-- -}
-- type Cctx
--     = Hole
--     | Let_CLess_cctx1 Cctx (Bind E_CLess S_CLess)
--     | Let_CLess_cctx2 E_CLess (Bind E_CLess Cctx)
--     | Exp_CLess_cctx1 Cctx
--     | Plus_CLess_cctx1 Cctx E_CLess
--     | Plus_CLess_cctx2 E_CLess Cctx
-- type CctxSyntax
--     = S_CLess_CCtx S_CLess
--     | E_CLess_CCtx E_CLess
--     | Cctx_CCtx Cctx
-- {-| Cursorless sorts and operators
-- -}
-- type S_CLess
--     = Let_CLess E_CLess (Bind E_CLess S_CLess)
--     | Exp_CLess E_CLess
--     | Hole_s_CLess
-- type E_CLess
--     = Plus_CLess E_CLess E_CLess
--     | Num_CLess
--     | Var_CLess
--     | Hole_e_CLess
-- type CursorlessSyntax
--     = S_CLess S_CLess
--     | E_CLess E_CLess
-- {-| "Normal"/initial/clean sorts and operators
-- -}
-- type S
--     = Let E (Bind E S)
--     | Exp E
--     | Hole_s
--     | Cursor_s S
-- type E
--     = Plus E E
--     | Num
--     | Var
--     | Hole_e
--     | Cursor_e E
-- type BaseSyntax
--     = S S
--     | E E
-- type alias Bind a b =
--     ( List a, b )
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
