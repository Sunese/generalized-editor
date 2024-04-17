module Example exposing (..)

import Elm
import Elm.Annotation exposing (..)
import Parser exposing (..)
import Syntax exposing (..)



-- We want something like this


type S
    = Let E ( List E, S )
    | Hole_s


type E
    = Plus E E
    | Num
    | Var String
    | Hole_e


{-| Simulate a type class
-}
type alias Substitutable a =
    { substitute : a -> a -> a }


{-| (Generic) Instance of a typeclass, we don't need any specific implementation
for each type/sort, we just want to assure that the expression and replacement
are of the same type. This is constrained by the `substitute` function signature
in the typeclass.
-}
substituteAny : Substitutable a
substituteAny =
    { substitute = \_ replacement -> replacement }


{-| Polymorphic function that can be used with any type that has an instance of
the `Substitutable` typeclass.
-}
substitute : Substitutable a -> a -> a -> a
substitute substitutable expression replacement =
    substitutable.substitute expression replacement


exampleS : S
exampleS =
    Let (Var "x") ( [ Num ], Hole_s )


exampleE : E
exampleE =
    Plus Num Num


{-| Passing anything else of type E will result in a type error (at compile time)
-}
subEExample : E
subEExample =
    substitute substituteAny exampleE Hole_e


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
