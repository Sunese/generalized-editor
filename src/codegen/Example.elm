module Example exposing (..)

import Conditionals.Conditionals exposing (..)
import Elm
import Elm.Annotation as Type exposing (..)
import Movement.DecomposeFun exposing (..)
import Movement.GetCursorPath exposing (..)
import Movement.Movement exposing (..)
import Movement.Parent exposing (..)
import Movement.Substitution exposing (..)
import Movement.ToCCtx exposing (..)
import Movement.ToCLess exposing (..)
import Parser exposing (..)
import Syntax.RawSyntaxP exposing (parseRawSyntax)
import Syntax.Syntax exposing (..)


exampleFiles : List Elm.File
exampleFiles =
    [ Elm.file [ "Syntax", "Base" ] <|
        createBaseSyntaxSorts
            (addCursorHoleOps <| exampleSyntax)
    , Elm.file [ "Syntax", "Bind" ] <| [ createBindType ]
    , Elm.file [ "Syntax", "Cursorless" ] <|
        createCursorlessSyntaxSorts exampleSyntax
    , Elm.file [ "Syntax", "CCtx" ] <|
        (fromCLessToCCtxSyntaxSorts <| createCursorlessSyntax exampleSyntax)
    , Elm.file [ "Syntax", "Wellformed" ] <|
        (fromCLessToWellFormedSorts <| createCursorlessSyntax exampleSyntax)
    , Elm.file [ "Movement" ] <|
        createToCLessFuns (addCursorHoleOps exampleSyntax)
            ++ createToCCtxFuns (addCursorHoleOps exampleSyntax)
            ++ createDecomposeFuns (addCursorHoleOps exampleSyntax)
            ++ [ createReplaceCCtxHoleFun
                    (addCursorHoleOps <| exampleSyntax)
                    (createCursorlessSyntax exampleSyntax)
                    (fromCLessToCCtxSyntax <| createCursorlessSyntax exampleSyntax)
               , createChildFun
                    (addCursorHoleOps <| exampleSyntax)
                    (createCursorlessSyntax exampleSyntax)
                    (fromCLessToCCtxSyntax <| createCursorlessSyntax exampleSyntax)
                    (fromCLessToWellFormedSyntax <| createCursorlessSyntax exampleSyntax)
               , createSubFun (fromCLessToWellFormedSyntax <| createCursorlessSyntax exampleSyntax)
               , createGetCursorPath <|
                    addCursorHoleOps <|
                        exampleSyntax
               ]
            ++ createParentFuns
                (filterCLess <| createCursorlessSyntax exampleSyntax)
                (filterCctx <| fromCLessToCCtxSyntax <| createCursorlessSyntax exampleSyntax)
    , Elm.file [ "Conditionals" ] <|
        createEditorCondDecls (filterCLess <| createCursorlessSyntax exampleSyntax)
            (filterWellformed <| fromCLessToWellFormedSyntax <| createCursorlessSyntax exampleSyntax)
    ]


exampleSyntax : Syntax
exampleSyntax =
    case parseRawSyntax rawSyntax of
        Ok syntax ->
            fromRawSyntax syntax

        Err err ->
            Debug.todo "Error parsing syntax"


rawSyntax : String
rawSyntax =
    "q in Query\ncmd in Command\nid in Id\nconst in Const\nclause in Clause\ncond in Condition\nexp in Expression\n\nq ::= \" SELECT \" id_1 \" FROM \" id_2 clause # (id,id,clause)q # select\ncmd ::= \" INSERT INTO \" id_1 \" AS \" id_2 q # (id,id.q)cmd # insert\nid ::= %string # ()id # ident[String]\nconst ::= %number # ()const # num[Int] | \"'\" %string \"'\" # ()const # str[String]\nclause ::= \" WHERE \" cond # (cond)clause # where | \" HAVING \" cond # (cond)clause # having\ncond ::= exp_1 \">\" exp_2 # (exp,exp)cond # greater | exp_1 \"=\" exp_2 # (exp,exp)cond # equals\nexp ::= const # (const)exp # econst | id # (id)exp # eident"


filterCctx : Syntax -> Syntax
filterCctx syntax =
    { synCats = List.filter (\x -> x.exp == "cctx") syntax.synCats
    , synCatOps = List.filter (\x -> x.synCat == "cctx") syntax.synCatOps
    }


filterWellformed : Syntax -> Syntax
filterWellformed syntax =
    { synCats = List.filter (\x -> x.exp == "wellformed") syntax.synCats
    , synCatOps = List.filter (\x -> x.synCat == "wellformed") syntax.synCatOps
    }


filterCLess : Syntax -> Syntax
filterCLess syntax =
    { synCats = List.filter (\x -> String.endsWith "_CLess" x.exp) syntax.synCats
    , synCatOps = List.filter (\x -> String.endsWith "_CLess" x.synCat) syntax.synCatOps
    }
