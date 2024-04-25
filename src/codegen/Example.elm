module Example exposing (..)

import Elm
import Elm.Annotation exposing (..)
import Parser exposing (..)
import Syntax exposing (..)

type alias C =
  WellFormedSyntax

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
