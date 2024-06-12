module Syntax.Syntax exposing (..)

import Elm
import Elm.Annotation as Type exposing (..)
import Elm.Case exposing (..)
import Elm.Case.Branch as Branch
import Gen.Convertable
import Gen.Decomposable exposing (..)
import Gen.Dict exposing (remove)
import Gen.Substitutable exposing (..)
import Parser exposing (..)
import Syntax.RawSyntaxP exposing (..)


type alias Arity =
    -- an arity is a list of pairs of (List String, String)
    -- the first element of the pair is the list of variables to be bound (if any)
    -- the second element of the pair is the operator argument's syntactic category
    List ( List String, String )


type alias Operator =
    { term : Term
    , arity : Arity
    , name : String
    , synCat : String
    , literal : Maybe String
    }


type alias SynCatOps =
    { ops : List Operator
    , synCat : String
    }


type alias Term =
    String


type alias SynCat =
    { exp : String
    , set : String
    }


type alias Syntax =
    { synCats : List SynCat
    , synCatOps : List SynCatOps
    }


type alias CCtxOp =
    Operator


type alias CCtxSyntax =
    { synCats : List SynCat
    , synCatOps : List CCtxSynCatOps
    }


type alias CCtxSynCatOps =
    SynCatOps


type alias CLessSyntax =
    Syntax


type alias WellFormedSyntax =
    Syntax


type alias WellFormedSynCatOps =
    SynCatOps


{-| def. 14, step 1 + 2 + 3, for every operator of cursorless sort and 1 <= i <= n,
add operator encapsulating the i'th subtree of the abt
-}



-- addCursorSortAndOps : CLessSyntax -> WellFormedSyntax
-- addCursorSortAndOps syntax =
--     -- NOTE: this is making me consider creating a new model specific for
--     -- wellformed syntax (or just their operators), to have a way to
--     -- specify what subtree is encapsulated.
--     -- With the current setup, one has to figure it out from the operator's name,
--     -- e.g. "assignment_cursor_arg_0" is the cursor operator for the first argument
--     -- Nothing is stopping us from having a better/different model
--     let
--         ops =
--             syntax.synCatOps
--                 |> List.map .ops
--                 |> List.concat
--         rootCursorOps =
--             List.map
--                 (\syncat ->
--                     { term = "TODO"
--                     , arity = [ ( [], syncat.exp ) ]
--                     , name = "root_" ++ syncat.exp
--                     , synCat = "wellformed"
--                     }
--                 )
--                 syntax.synCats
--         newOps =
--             List.map
--                 (\op ->
--                     List.indexedMap
--                         (\i _ ->
--                             { term = "TODO"
--                             , arity = op.arity
--                             , name = op.name ++ "_cursor" ++ String.fromInt (i + 1)
--                             , synCat = "wellformed"
--                             }
--                         )
--                         op.arity
--                 )
--                 ops
--                 |> List.concat
--                 |> List.append rootCursorOps
--     in
--     { syntax
--         | synCatOps =
--             syntax.synCatOps
--                 ++ [ { ops = newOps
--                      , synCat = "wellformed"
--                      }
--                    ]
--         , synCats =
--             syntax.synCats
--                 ++ [ { exp = "wellformed"
--                      , set = "WellFormed"
--                      }
--                    ]
--     }
--  in this version we do the same, but we only add root cursor operators,
-- as to my suspicion where we only want a decomposition into cursor context +
-- wellformed subtree, but where the input tree is always fully walked
-- until a cursor is found, and the rest of the tree will be checked for wellformedness,
-- but where the cursor of course only can be at the root of the tree,
-- since we stopped the cursor context decomposition at the cursor,
-- and we continue from there


addCursorSortAndOps : CLessSyntax -> WellFormedSyntax
addCursorSortAndOps syntax =
    let
        ops =
            syntax.synCatOps
                |> List.map .ops
                |> List.concat

        rootCursorOps =
            List.map
                (\syncat ->
                    { term = "TODO"
                    , arity = [ ( [], syncat.exp ) ]
                    , name = "root_" ++ syncat.exp
                    , synCat = "wellformed"
                    , literal = Nothing
                    }
                )
                syntax.synCats
    in
    { syntax
        | synCatOps =
            syntax.synCatOps
                ++ [ { ops = rootCursorOps
                     , synCat = "wellformed"
                     }
                   ]
        , synCats =
            syntax.synCats
                ++ [ { exp = "wellformed"
                     , set = "WellFormed"
                     }
                   ]
    }


addCCtxSort : Syntax -> CCtxSyntax
addCCtxSort syntax =
    { syntax
        | synCats =
            syntax.synCats
                ++ [ { exp = "cctx"
                     , set = "CursorCtx"
                     }
                   ]
    }


createCCtxSort : Syntax -> CCtxSyntax
createCCtxSort syntax =
    { synCats =
        syntax.synCats
            ++ [ { exp = "cctx"
                 , set = "CursorCtx"
                 }
               ]
    , synCatOps = []
    }


addCCtxOp : CCtxSyntax -> CCtxSyntax
addCCtxOp syntax =
    { syntax
        | synCatOps =
            syntax.synCatOps
                ++ [ { ops =
                        [ { term = "\"[TODO-cursorCtxOp]\""
                          , arity = []
                          , name = "Cctx_hole"
                          , synCat = "cctx"
                          , literal = Nothing
                          }
                        ]
                     , synCat = "cctx"
                     }
                   ]
    }


addCCtxOps : CCtxSyntax -> CCtxSyntax
addCCtxOps syntax =
    { syntax
        | synCatOps =
            syntax.synCatOps
                ++ List.map toCCtxSynCatRules syntax.synCatOps
    }


toCCtxSynCatRules : SynCatOps -> CCtxSynCatOps
toCCtxSynCatRules synCatRules =
    let
        newOps =
            toCCtxOps synCatRules.ops
    in
    { ops = newOps
    , synCat = "cctx"
    }


toCCtxOps : List Operator -> List CCtxOp
toCCtxOps ops =
    List.map
        (\op ->
            List.indexedMap
                (\i _ ->
                    { term = "TODO:" ++ op.name ++ "_cctx"
                    , arity =
                        List.indexedMap
                            -- replace the i'th element in op.arity
                            -- list with "cctx"
                            (\j ( mbybound, arg ) ->
                                if i == j then
                                    ( mbybound, "cctx" )

                                else
                                    ( mbybound, arg )
                            )
                            op.arity
                    , name = op.name ++ "_cctx" ++ String.fromInt (i + 1)
                    , synCat = "cctx"
                    , literal = op.literal
                    }
                )
                op.arity
        )
        ops
        |> List.concat


addPostfixToSyntax : String -> Syntax -> Syntax
addPostfixToSyntax postfix syntax =
    -- add postfix to:
    -- 1. All synCat.exp and synCat.set
    -- 2. All synCatOp.synCat and all synCatOp.name, synCatOp.synCat, synCatOp.arity
    { syntax
        | synCats =
            List.map
                (\syncat ->
                    { exp = syncat.exp ++ postfix
                    , set = syncat.set ++ postfix
                    }
                )
                syntax.synCats
        , synCatOps =
            List.map
                (\synCatOp ->
                    { synCatOp
                        | synCat = synCatOp.synCat ++ postfix
                        , ops =
                            List.map
                                (\op ->
                                    { op
                                        | name = op.name ++ postfix
                                        , synCat = op.synCat ++ postfix
                                        , arity =
                                            List.map
                                                (\( boundvars, param ) ->
                                                    ( List.map (\x -> x ++ postfix) boundvars, param ++ postfix )
                                                )
                                                op.arity
                                    }
                                )
                                synCatOp.ops
                    }
                )
                syntax.synCatOps
    }


toCLessSyntax : Syntax -> CLessSyntax
toCLessSyntax syntax =
    -- Add hole operators and
    -- postfix all operators and sorts with "_CLess"
    let
        syntaxWithHole =
            addHoleOps syntax
    in
    { syntaxWithHole
        | synCatOps =
            List.map
                (\synCatRule ->
                    { synCatRule
                        | ops =
                            List.map
                                (\op ->
                                    { op
                                        | name = op.name ++ "_CLess"
                                    }
                                )
                                synCatRule.ops
                    }
                )
                syntax.synCatOps
        , synCats =
            List.map
                (\syncat ->
                    { exp = syncat.exp ++ "_CLess"
                    , set = syncat.set ++ "_CLess"
                    }
                )
                syntax.synCats
    }


addCursorOps : Syntax -> Syntax
addCursorOps syntax =
    { syntax
        | synCatOps =
            List.map
                (\synCatRule ->
                    { synCatRule
                        | ops =
                            List.concat
                                [ synCatRule.ops
                                , [ createCursorOperator synCatRule ]
                                ]
                    }
                )
                syntax.synCatOps
    }


addHoleOps : Syntax -> Syntax
addHoleOps syntax =
    { syntax
        | synCatOps =
            List.map
                (\synCatRule ->
                    { synCatRule
                        | ops =
                            List.concat
                                [ synCatRule.ops
                                , [ createHoleOperator synCatRule ]
                                ]
                    }
                )
                syntax.synCatOps
    }


addCursorHoleOps : Syntax -> Syntax
addCursorHoleOps syntax =
    { syntax
        | synCatOps =
            List.map
                (\synCatRule ->
                    { synCatRule
                        | ops =
                            List.concat
                                [ synCatRule.ops
                                , [ createHoleOperator synCatRule ]
                                , [ createCursorOperator synCatRule ]
                                ]
                    }
                )
                syntax.synCatOps
    }


createHoleOperator : SynCatOps -> Operator
createHoleOperator synCatRules =
    { term = "\"{}\""
    , arity = []
    , name = "hole_" ++ synCatRules.synCat
    , synCat = synCatRules.synCat
    , literal = Nothing
    }


createCursorOperator : SynCatOps -> Operator
createCursorOperator synCatRules =
    { term = "\"[\"" ++ synCatRules.synCat ++ "\"]\""
    , arity = [ ( [], synCatRules.synCat ) ]
    , name = "cursor_" ++ synCatRules.synCat
    , synCat = synCatRules.synCat
    , literal = Nothing
    }


getSyntacticCategories : Syntax -> List String
getSyntacticCategories syntax =
    List.map .exp syntax.synCats


fromCLessToCCtxSyntax : Syntax -> CCtxSyntax
fromCLessToCCtxSyntax syntax =
    addCCtxOps <| addCCtxOp <| addCCtxSort syntax


fromCLessToCCtxSyntaxSorts : Syntax -> List Elm.Declaration
fromCLessToCCtxSyntaxSorts syntax =
    let
        cctxSyntax =
            addCCtxOps <| addCCtxOp <| addCCtxSort syntax
    in
    [ getCCtxCustomType cctxSyntax
    ]


getCCtxCustomType : CCtxSyntax -> Elm.Declaration
getCCtxCustomType syntax =
    let
        -- get all the operators that belong to the current syntactic category
        ops =
            List.filter
                (\synCatRule -> synCatRule.synCat == "cctx")
                syntax.synCatOps
                |> List.map .ops
                |> List.concat
    in
    Elm.customType "cctx" <|
        List.map
            (\op ->
                Elm.variantWith op.name (getNamedAnnotationsWithImports op "cctx")
            )
            ops


fromCLessToWellFormedSyntax : Syntax -> WellFormedSyntax
fromCLessToWellFormedSyntax syntax =
    addCursorSortAndOps syntax


fromCLessToWellFormedSorts : Syntax -> List Elm.Declaration
fromCLessToWellFormedSorts syntax =
    let
        wellFormedSyntax =
            addCursorSortAndOps syntax
    in
    [ getCustomTypeWellformed wellFormedSyntax
    ]


createCursorlessSyntax : Syntax -> Syntax
createCursorlessSyntax syntax =
    -- i.e. do the same as createCursorlessSyntaxSorts but just return a new syntax,
    -- not a list of Elm.Declarations
    addPostfixToSyntax "_CLess" <| addHoleOps syntax


createDecomposeFun : Syntax -> Elm.Declaration
createDecomposeFun syntax =
    let
        startSymbol =
            syntax.synCats
                |> List.head
                |> Maybe.withDefault { exp = "error", set = "error" }
                |> .exp
    in
    Elm.declaration "decompose" <|
        Elm.fn ( "ast", Just <| Type.named [] startSymbol )
            (\arg ->
                Elm.Case.custom arg (Type.named [] startSymbol) <|
                    createBranches syntax
            )


createBranches : Syntax -> List Elm.Case.Branch
createBranches syntax =
    let
        startSymbolOps =
            syntax.synCatOps
                |> List.head
                |> Maybe.withDefault { ops = [], synCat = "error" }
    in
    List.map
        (\op ->
            Elm.Case.branchWith op.name (List.length op.arity) <|
                \_ ->
                    Elm.maybe <|
                        Just <|
                            Elm.tuple
                                (getCctxExp op |> Elm.withType (Type.namedWith [] "Cctx" []))
                                (getWellFormedExp op |> Elm.withType (Type.namedWith [] "Wellformed" []))
        )
        startSymbolOps.ops


getCctxExp : Operator -> Elm.Expression
getCctxExp op =
    -- if the operator's name contains "cursor", return Cctx.Hole
    if String.contains "cursor" op.name then
        Elm.val "Hole"

    else
        Elm.val "Hole"


getWellFormedExp : Operator -> Elm.Expression
getWellFormedExp op =
    -- if the operator's name contains "cursor", return root_*op.syncat*_CLess *toCLess op*
    -- Elm.val <| "Root_s_CLess " ++ toCLessOp op
    Elm.val <| "Root_s_CLess Hole_s_CLess"


decomposableInstance : Syntax -> Elm.Declaration
decomposableInstance syntax =
    let
        startSymbol =
            syntax.synCats
                |> List.head
                |> Maybe.withDefault { exp = "error", set = "error" }
                |> .exp
    in
    Elm.declaration ("decomposable_" ++ startSymbol) <|
        Elm.withType
            (Type.namedWith [ "Decomposable" ]
                "Decomposable"
                [ Type.named [] startSymbol
                , Type.named [] "Cctx"
                , Type.named [] "Wellformed"
                ]
            )
        <|
            Elm.function []
                (\_ ->
                    Elm.record
                        [ ( "decompose", Elm.val <| "decompose" )
                        ]
                )


convertableInstances : Syntax -> List Elm.Declaration
convertableInstances syntax =
    List.map
        (\synCatOp ->
            Elm.declaration ("convertable_" ++ synCatOp.synCat) <|
                Elm.withType
                    (Type.namedWith [ "Convertable" ]
                        "Convertable"
                        [ Type.named [] synCatOp.synCat
                        , Type.named [] (synCatOp.synCat ++ "_CLess")
                        ]
                    )
                <|
                    Elm.function []
                        (\_ ->
                            Elm.record
                                [ ( "toCLess"
                                  , Elm.fn ( synCatOp.synCat, Nothing )
                                        (\arg ->
                                            Elm.Case.custom arg (Type.named [] synCatOp.synCat) <|
                                                createToCLessBranches2 synCatOp
                                        )
                                  )
                                ]
                        )
        )
        syntax.synCatOps


createToCLessBranches2 : SynCatOps -> List Elm.Case.Branch
createToCLessBranches2 synCatOp =
    List.map
        (\op ->
            Elm.Case.branchWith op.name (List.length op.arity) <|
                \_ ->
                    createToCLessExp op
        )
        synCatOp.ops


createToCLessExp : Operator -> Elm.Expression
createToCLessExp op =
    let
        clessOpName =
            firstCharToUpper op.name ++ "_CLess"
    in
    if String.contains "cursor" op.name then
        Elm.nothing

    else
        case op.arity of
            [] ->
                Elm.maybe <|
                    Just <|
                        Elm.val <|
                            clessOpName

            _ ->
                Elm.maybe <|
                    Just <|
                        Elm.apply (Elm.val <| clessOpName) <|
                            List.indexedMap
                                (\i ( boundVars, arg ) ->
                                    case boundVars of
                                        [] ->
                                            Elm.apply
                                                (Elm.get "toCLess"
                                                    (Elm.value
                                                        { importFrom = []
                                                        , name = "convertable_" ++ arg
                                                        , annotation =
                                                            Just <|
                                                                Type.namedWith []
                                                                    "Convertable"
                                                                    [ Type.named [] op.synCat
                                                                    , Type.named [] <| op.synCat ++ "_CLess"
                                                                    ]
                                                        }
                                                    )
                                                )
                                                [ Elm.val <| "arg" ++ String.fromInt (i + 1) ]

                                        _ ->
                                            Elm.val <| "IMPLEMENTME"
                                )
                                op.arity


toCLessOp : Operator -> String
toCLessOp op =
    -- TODO: create call to toCLess fun
    firstCharToUpper op.name ++ "_CLess"


createToCursorLessFun : Syntax -> Elm.Declaration
createToCursorLessFun syntax =
    let
        startSymbol =
            syntax.synCats
                |> List.head
                |> Maybe.withDefault { exp = "error", set = "error" }
                |> .exp
    in
    Elm.declaration "toCLess" <|
        Elm.fn ( "ast", Just <| Type.named [] startSymbol )
            (\arg ->
                Elm.Case.custom arg (Type.named [] startSymbol) <|
                    createToCLessBranches syntax
            )


createToCLessBranches : Syntax -> List Elm.Case.Branch
createToCLessBranches syntax =
    let
        startSymbolOps =
            syntax.synCatOps
                |> List.head
                |> Maybe.withDefault { ops = [], synCat = "error" }
    in
    List.map
        (\op ->
            Elm.Case.branchWith op.name (List.length op.arity) <|
                \_ ->
                    toCLessReturnExpression op
        )
        startSymbolOps.ops


toCLessReturnExpression : Operator -> Elm.Expression
toCLessReturnExpression op =
    if String.contains "cursor" op.name then
        Elm.value
            { importFrom = [ "Debug" ]
            , name = "todo \"Cursor operator cannot be mapped to cursorless operator\""
            , annotation = Nothing
            }

    else
        Elm.withType (Type.maybe <| Type.named [] "S_CLess") <| Elm.maybe <| Just <| Elm.val <| "Hole_s_CLess"



-- let
--     prefix =
--         firstCharToUpper op.name ++ "_CLess" ++ " "
--     returnString =
--         List.indexedMap
--             (\i ( boundVars, arg ) ->
--                 case boundVars of
--                     [] ->
--                         "( toCLess (" ++ " " ++ "arg" ++ String.fromInt (i + 1) ++ " )" ++ " )"
--                     _ ->
--                         "HIRE ME"
--             )
--             op.arity
--             |> String.join " "
-- in
-- Elm.value
--     { importFrom = []
--     , name = prefix ++ returnString
--     , annotation = Just <| Type.named [] op.name
--     }


removeCursorOps : List Operator -> List Operator
removeCursorOps ops =
    List.filter
        (\op ->
            not <| String.contains "cursor" op.name
        )
        ops


firstCharToUpper : String -> String
firstCharToUpper s =
    (String.toUpper <| String.left 1 s) ++ String.dropLeft 1 s


createCursorlessSyntaxSorts : Syntax -> List Elm.Declaration
createCursorlessSyntaxSorts syntax =
    -- i.e. do the same as createBaseSyntaxSorts but add
    -- a hole operator for each syntactic category
    -- and create a custom type called CursorlessSyntax that is a union of all of them
    let
        cursorlessSyntax =
            addPostfixToSyntax "_CLess" <| addHoleOps syntax

        uniqueSynCats =
            List.map (\syncat -> syncat.exp) cursorlessSyntax.synCats
                |> List.foldl
                    (\syncat acc ->
                        if List.member syncat acc then
                            acc

                        else
                            acc ++ [ syncat ]
                    )
                    []
    in
    List.map (\synCat -> getCustomType synCat cursorlessSyntax) uniqueSynCats
        ++ [ Elm.customType "CursorLess" <|
                List.map
                    (\syncat -> Elm.variantWith syncat [ Type.named [] syncat ])
                    uniqueSynCats
           ]


createBindType : Elm.Declaration
createBindType =
    Elm.alias "Bind" <|
        Type.tuple
            (Type.list <|
                Type.var "a"
            )
            (Type.var "b")


createBaseSyntaxSorts : Syntax -> List Elm.Declaration



-- i.e. take all syncat rules and create a custom type for each


createBaseSyntaxSorts syntax =
    -- and then create a custom type called BaseSyntax that is a union of all of them
    let
        uniqueSynCats =
            List.map (\syncat -> syncat.exp) syntax.synCats
                |> List.foldl
                    (\syncat acc ->
                        if List.member syncat acc then
                            acc

                        else
                            acc ++ [ syncat ]
                    )
                    []
    in
    List.map (\synCat -> getCustomType synCat syntax) uniqueSynCats
        ++ [ Elm.customType "Base" <|
                List.map
                    (\syncat -> Elm.variantWith syncat [ Type.named [] syncat ])
                    uniqueSynCats
           ]


getCustomType : String -> Syntax -> Elm.Declaration
getCustomType synCat syntax =
    -- e.g. for synCat = Statement, getCustomType returns
    -- Elm.customType "Statement" [ Elm.variantWith "Assignment" <--- operator
    --                                  [Type.named [] "Id"] <--- arity (args)
    --                             , Elm.variantWith "While" [] <--- operator without args
    --                             ]
    let
        -- get all the operators that belong to the current syntactic category
        ops =
            List.filter
                (\synCatRule -> synCatRule.synCat == synCat)
                syntax.synCatOps
                |> List.map .ops
                |> List.concat
    in
    Elm.customType synCat <|
        List.map
            (\op ->
                Elm.variantWith op.name (getNamedAnnotations op)
            )
            ops


getCustomTypeWellformed : WellFormedSyntax -> Elm.Declaration
getCustomTypeWellformed syntax =
    -- e.g. for synCat = Statement, getCustomType returns
    -- Elm.customType "Statement" [ Elm.variantWith "Assignment" <--- operator
    --                                  [Type.named [] "Id"] <--- arity (args)
    --                             , Elm.variantWith "While" [] <--- operator without args
    --                             ]
    let
        -- get all the operators that belong to the current syntactic category
        ops =
            List.filter
                (\synCatRule -> synCatRule.synCat == "wellformed")
                syntax.synCatOps
                |> List.map .ops
                |> List.concat
    in
    Elm.customType "wellformed" <|
        List.map
            (\op ->
                Elm.variantWith op.name (getNamedAnnotationsWithImports op "wellformed")
            )
            ops


getNamedAnnotationsWithImports : Operator -> String -> List Annotation
getNamedAnnotationsWithImports op dontimport =
    if List.isEmpty op.arity then
        case op.literal of
            Nothing ->
                []

            Just literal ->
                [ Type.named [] literal ]

    else
        List.map
            (\( boundvars, param ) ->
                let
                    parammodule =
                        if param == dontimport then
                            []

                        else
                            [ "Syntax", "Cursorless" ]

                    binderModule =
                        if (Maybe.withDefault "" <| List.head boundvars) == dontimport then
                            []

                        else
                            [ "Syntax", "Cursorless" ]
                in
                case boundvars of
                    [] ->
                        Type.named parammodule param

                    _ ->
                        -- Due to the limitation of the list of bound variables being of the same type,
                        -- we extract only the first element of the list
                        Type.namedWith [ "Syntax", "Bind" ] "Bind" <|
                            [ Type.named binderModule <| Maybe.withDefault "" <| List.head boundvars
                            , Type.named parammodule param
                            ]
            )
            op.arity


getNamedAnnotations : Operator -> List Annotation
getNamedAnnotations op =
    if List.isEmpty op.arity then
        case op.literal of
            Nothing ->
                []

            Just literal ->
                [ Type.named [] literal ]

    else
        List.map
            (\( boundvars, param ) ->
                case boundvars of
                    [] ->
                        Type.named [] param

                    _ ->
                        -- Due to the limitation of the list of bound variables being of the same type,
                        -- we extract only the first element of the list
                        Type.namedWith [ "Syntax", "Bind" ] "Bind" <|
                            [ Type.named [] <| Maybe.withDefault "" <| List.head boundvars
                            , Type.named [] param
                            ]
            )
            op.arity


fromRawSyntax : RawSyntax -> Syntax
fromRawSyntax rs =
    { synCats = rs.synCats
    , synCatOps = List.map fromRawSynCatRules rs.synCatRules
    }


fromRawSynCatRules : RawSynCatRules -> SynCatOps
fromRawSynCatRules raw =
    { ops = List.map fromRawOp raw.operators
    , synCat = raw.synCat
    }


fromRawOp : RawOp -> Operator
fromRawOp raw =
    { term = raw.term
    , arity = rawArityToArity raw.arity
    , name = raw.name
    , synCat = getSynCat raw.arity
    , literal = raw.literal
    }


getSynCat : String -> String
getSynCat s =
    String.split ")" s
        |> List.reverse
        |> List.head
        |> Maybe.withDefault ""


rawArityToArity : String -> Arity
rawArityToArity rawArity =
    let
        inbetweenparens =
            String.dropLeft 1 rawArity |> String.split ")" |> List.head
    in
    case inbetweenparens of
        Nothing ->
            []

        Just s ->
            if String.isEmpty s then
                []

            else
                String.split "," s
                    |> List.map
                        (\arity ->
                            case String.split "." arity of
                                _ :: binders ->
                                    case binders of
                                        -- there are no binders
                                        [] ->
                                            ( [], arity )

                                        _ ->
                                            -- there are binders
                                            getBoundVariablesAndBinder arity

                                _ ->
                                    -- the string is empty
                                    ( [], arity )
                        )


getBoundVariablesAndBinder : String -> ( List String, String )
getBoundVariablesAndBinder s =
    let
        binder =
            String.split "." s
                |> List.reverse
                |> List.head

        variables =
            String.split "." s
                |> List.reverse
                |> List.drop 1
                |> List.reverse
    in
    case binder of
        Nothing ->
            ( [], "error, no parameters found" )

        Just last_ ->
            ( variables, last_ )
