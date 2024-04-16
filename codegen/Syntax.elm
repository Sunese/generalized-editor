module Syntax exposing (..)

import Elm
import Elm.Annotation exposing (..)
import Parser exposing (..)
import RawSyntaxP exposing (..)



-- ✅    1. raw arity to arity with binders
--        (skipped for now, this is only relevant for pretty-printing)
-- ✅    2. check that terms are well-formed, i.e. that everything outside
--       quotations are syntactic categories
-- ✅    3. generate a codegen declaration for each operator and its arguments
--          Given a list of syntactic categories and a list of derivations,
--          generate custom types this way:
--          for each syntactic category, create an Elm.customType
--          then, for each operator within that syntactic category,
--          add an Elm.variantWith to the list of variants to the custom type
-- ✅    4. for every sort/syntactic category, add a hole_s operator with arity ()s
--          and a cursor_s operator with arity (s)s
-- ✅    5. create the sorts and family of cursorless operators
-- ✅    6. create the sorts and family of operators for cursor contexts
-- ✅    7. ditto, but for well-formed trees


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



-- toWellFormedSyntax : CLessSyntax -> WellFormedSyntax
-- toWellFormedSyntax cursorlessSyn =
--     { cursorlessSyn
--         | synCatOps =
--             toWellFormedSynCatOps cursorlessSyn.synCatOps
--     }


{-| def. 14, step 1 + 2 + 3, for every operator of cursorless sort and 1 <= i <= n,
add operator encapsulating the i'th subtree of the abt
-}
addCursorSortAndOps : CLessSyntax -> WellFormedSyntax
addCursorSortAndOps syntax =
    -- NOTE: this is making me consider creating a new model specific for
    -- wellformed syntax (or just their operators), to have a way to
    -- specify what subtree is encapsulated.
    -- With the current setup, one has to figure it out from the operator's name,
    -- e.g. "assignment_cursor_arg_0" is the cursor operator for the first argument
    -- Nothing is stopping us from having a better/different model
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
                    , name = "rootCursor_" ++ syncat.exp
                    , synCat = "wellformed"
                    }
                )
                syntax.synCats

        newOps =
            List.map
                (\op ->
                    List.indexedMap
                        (\i _ ->
                            { term = "TODO"
                            , arity = op.arity
                            , name = op.name ++ "_cursor_arg_" ++ String.fromInt i
                            , synCat = "wellformed"
                            }
                        )
                        op.arity
                )
                ops
                |> List.concat
                |> List.append rootCursorOps
    in
    { syntax
        | synCatOps =
            syntax.synCatOps
                ++ [ { ops = newOps
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



-- toWellFormedSynCatOps : List SynCatOps -> List WellFormedSynCatOps
-- toWellFormedSynCatOps synCatOps =
-- toWellFormedOp : List Operator -> List Operator
-- toWellFormedOp ops =
--     List.indexedMap
--         (\i op ->
--             { op | name = op.name ++ "_wf" ++ String.fromInt i }
--         )
--         ops


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


addCCtxOp : CCtxSyntax -> CCtxSyntax
addCCtxOp syntax =
    { syntax
        | synCatOps =
            syntax.synCatOps
                ++ [ { ops =
                        [ { term = "\"[TODO-cursorCtxOp]\""
                          , arity = []
                          , name = "cctxHole"
                          , synCat = "cctx"
                          }
                        ]
                     , synCat = "cctx"
                     }
                   ]
        , synCats =
            syntax.synCats
                ++ [ { exp = "cctx"
                     , set = "CursorCtx"
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
                    , name = op.name ++ "_cctx" ++ String.fromInt i
                    , synCat = "cctx"
                    }
                )
                op.arity
        )
        ops
        |> List.concat


toCLessSyntax : Syntax -> CLessSyntax
toCLessSyntax syntax =
    addHoleOps syntax


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
    }


createCursorOperator : SynCatOps -> Operator
createCursorOperator synCatRules =
    { term = "\"[\"" ++ synCatRules.synCat ++ "\"]\""
    , arity = [ ( [], synCatRules.synCat ) ]
    , name = "cursor_" ++ synCatRules.synCat
    , synCat = synCatRules.synCat
    }


getSyntacticCategories : Syntax -> List String
getSyntacticCategories syntax =
    List.map .exp syntax.synCats


getTypeDecls : Syntax -> List Elm.Declaration
getTypeDecls syntax =
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
        -- TODO: this limits the list of bound vars to all be of same type
        ++ [ Elm.alias "Bind" <|
                Elm.Annotation.tuple
                    (Elm.Annotation.list <|
                        Elm.Annotation.var "a"
                    )
                    (Elm.Annotation.var "b")
           ]


getCustomType : String -> Syntax -> Elm.Declaration
getCustomType synCat syntax =
    -- e.g. for synCat = Statement, getCustomType returns
    -- Elm.customType "Statement" [ Elm.variantWith "Assignment" <--- operator
    --                                  [Elm.Annotation.named [] "Id"] <--- arity (args)
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
            (\rule ->
                Elm.variantWith rule.name (getNamedAnnotations rule.arity)
            )
            ops


getNamedAnnotations : Arity -> List Annotation
getNamedAnnotations arity =
    List.map
        (\( boundvars, param ) ->
            case boundvars of
                [] ->
                    Elm.Annotation.named [] param

                _ ->
                    -- Due to the limitation of the list of bound variables being of the same type,
                    -- we extract only the first element of the list
                    Elm.Annotation.namedWith [] "Bind" <|
                        [ Elm.Annotation.named [] <| Maybe.withDefault "" <| List.head boundvars
                        , Elm.Annotation.named [] param
                        ]
        )
        arity


rawSyntaxToSyntax : RawSyntax -> Syntax
rawSyntaxToSyntax rs =
    { synCats = rs.synCats
    , synCatOps = List.map rawSynCatRulesToSynCatRules rs.synCatRules
    }


rawSynCatRulesToSynCatRules : RawSynCatRules -> SynCatOps
rawSynCatRulesToSynCatRules raw =
    { ops = List.map rawRuleToRule raw.operators
    , synCat = raw.synCat
    }


rawRuleToRule : RawOp -> Operator
rawRuleToRule re =
    { term = re.term
    , arity = rawArityToArity re.arity
    , name = re.name
    , synCat = getSynCat re.arity
    }


getSynCat : String -> String
getSynCat s =
    String.split ")" s
        |> List.reverse
        |> List.head
        |> Maybe.withDefault ""


rawArityToArity : String -> Arity
rawArityToArity rawArity =
    -- ignore parentheses
    rawArity
        -- drop '('
        |> String.dropLeft 1
        -- drop ')s'
        |> String.split ")"
        |> List.head
        |> Maybe.withDefault ""
        |> String.split ","
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
