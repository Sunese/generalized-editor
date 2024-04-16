module Example exposing (..)

import Elm
import Elm.Annotation exposing (..)
import Parser exposing (..)
import Syntax exposing (..)



-- We want something like this:


type S
    = Let E ( List E, S )
    | Hole_s


type E
    = Plus E E
    | Num
    | Var String
    | Hole_e


type alias Substitutable a =
    { substitute : a -> a -> a }


substituteS : Substitutable S
substituteS =
    { substitute = \_ replacement -> replacement }


substituteE : Substitutable E
substituteE =
    { substitute = \_ replacement -> replacement }


substitute : Substitutable a -> a -> a -> a
substitute substitutable expression replacement =
    substitutable.substitute expression replacement


example : S
example =
    Let (Plus Num Num) ( [ Num ], Hole_s )


example_sub_ : S -> S -> S
example_sub_ =
    substitute substituteS


exampleFiles : List Elm.File
exampleFiles =
    [ Elm.file [ "Ops" ] <| getTypeDecls exampleAddOperators
    , Elm.file [ "Ops_CLess" ] <| getTypeDecls <| addHoleOps <| exampleSyntax
    , Elm.file [ "Ops_CCtx" ] <| getTypeDecls <| addCCtxOp <| addCCtxSort <| addCCtxOps <| addHoleOps <| exampleSyntax
    , Elm.file [ "Ops_Wellformed" ] <|
        getTypeDecls <|
            addCursorSortAndOps <|
                addHoleOps <|
                    exampleSyntax
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
