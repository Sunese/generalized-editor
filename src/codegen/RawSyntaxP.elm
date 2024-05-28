module RawSyntaxP exposing (..)

import Html exposing (li)
import Parser exposing (..)



-- Raw syntax is the syntax of a language as it directly appears in the specification
-- i.e. without any transformations, binders, or other relevant checks


type alias RawSyntax =
    { synCats : List RawSynCat
    , synCatRules : List RawSynCatRules
    }


type alias RawSynCatRules =
    { synCat : String
    , operators : List RawOp
    }


type alias RawOp =
    { term : String
    , arity : String
    , name : String
    , literal : Maybe String
    }


type alias RawSynCat =
    { exp : String
    , set : String
    }


exampleRawRun : Result (List DeadEnd) RawSyntax
exampleRawRun =
    Parser.run syntaxP "p in Prog\ns in Stmt\nvd in VariableDecl\nfd in FunDecl\nt in Type\nid in Id\ne in Exp\nb in Block\nfa in Funarg\ncond in Conditional\nint in Int\nchar in Char\nbool in Bool\nstring in String\n\np ::= fd # (fd)p # program\nb ::= bi # (bi)b # block\nbi ::= vd # (vd)bi # blockdecls | s # (s)bi # blockstmts | eps # ()bi # blockdone\nvd ::= t id \"=\" e \";\" bi # (t,e,id.bi)s # vardecl"


parseRawSyntax : String -> Result (List DeadEnd) RawSyntax
parseRawSyntax input =
    Parser.run syntaxP input


syntaxP : Parser RawSyntax
syntaxP =
    Parser.succeed RawSyntax
        |= scsP
        |= derivationsP


derivationsP : Parser (List RawSynCatRules)
derivationsP =
    Parser.sequence
        { start = ""
        , separator = "\n"
        , end = ""
        , spaces = Parser.chompWhile (\c -> c == ' ')
        , item = derivationP
        , trailing = Optional
        }


derivationP : Parser RawSynCatRules
derivationP =
    Parser.succeed RawSynCatRules
        |= formSymP
        |. Parser.spaces
        |. Parser.keyword "::="
        |. Parser.spaces
        |= Parser.sequence
            { start = ""
            , separator = "|"
            , end = ""
            , spaces = Parser.chompWhile (\c -> c == ' ')
            , item = expP
            , trailing = Forbidden
            }


expP : Parser RawOp
expP =
    Parser.succeed RawOp
        |= termP
        |. Parser.keyword "#"
        |. Parser.chompWhile (\c -> c == ' ')
        |= arityP
        |. Parser.keyword "#"
        |. Parser.chompWhile (\c -> c == ' ')
        |= nameP
        |= literalP


literalP : Parser (Maybe String)
literalP =
    Parser.chompWhile (\c -> c == '[')
        |> Parser.getChompedString
        |> Parser.andThen
            (\s ->
                if String.isEmpty s then
                    Parser.succeed Nothing

                else
                    Parser.chompWhile (\c -> c /= ']')
                        |> Parser.getChompedString
                        |> Parser.andThen
                            (\s_ ->
                                if String.isEmpty s_ then
                                    Parser.problem "Expected a literal"

                                else
                                    Parser.symbol "]"
                                        |> Parser.andThen
                                            (\_ -> Parser.succeed (Just s_))
                            )
            )


nameP : Parser String
nameP =
    Parser.chompWhile (\c -> c /= '\n' && c /= '|' && c /= '[')
        |> Parser.getChompedString
        |> Parser.andThen
            (\s ->
                if String.isEmpty s then
                    Parser.problem "Expected an operator name"

                else
                    String.trim s |> Parser.succeed
            )


arityP : Parser String
arityP =
    Parser.chompWhile (\c -> c /= '#')
        |> Parser.getChompedString
        |> Parser.andThen
            (\s ->
                if String.isEmpty s then
                    Parser.problem "Expected an arity"

                else
                    String.trim s |> Parser.succeed
            )


termP : Parser String
termP =
    Parser.chompWhile (\c -> c /= '#')
        |> Parser.getChompedString
        |> Parser.andThen
            (\s ->
                if String.isEmpty s then
                    Parser.problem "Expected a term (concrete syntax)"

                else
                    String.trim s |> Parser.succeed
            )


formSymP : Parser String
formSymP =
    Parser.chompWhile Char.isAlphaNum
        |> Parser.getChompedString
        |> Parser.andThen
            (\s ->
                if String.isEmpty s then
                    Parser.problem "Expected a name for the formation symbol"

                else
                    String.trim s |> Parser.succeed
            )


scsP : Parser (List RawSynCat)
scsP =
    Parser.sequence
        { start = ""
        , separator = "\n"
        , end = "\n"
        , spaces = Parser.chompWhile (\c -> c == ' ')
        , item = scP
        , trailing = Optional
        }


scP : Parser RawSynCat
scP =
    Parser.succeed RawSynCat
        |= scExpP
        |. Parser.spaces
        |. Parser.keyword "in"
        |. Parser.spaces
        |= scSetP


scExpP : Parser String
scExpP =
    Parser.chompWhile Char.isAlphaNum
        |> Parser.getChompedString
        |> Parser.andThen
            (\s ->
                if String.isEmpty s then
                    Parser.problem "No syntactic category expression"

                else
                    Parser.succeed s
            )


scSetP : Parser String
scSetP =
    Parser.chompWhile Char.isAlphaNum
        |> Parser.getChompedString
        |> Parser.andThen
            (\s ->
                if String.isEmpty s then
                    Parser.problem "No syntactic category set"

                else
                    Parser.succeed s
            )
