module Movement.Substitution exposing (..)

import Array
import Elm
import Elm.Annotation as Type exposing (..)
import Elm.Case exposing (..)
import Elm.Case.Branch as Branch exposing (ignore)
import Elm.Let
import Gen.Decomposable exposing (..)
import Gen.Substitutable exposing (..)
import Parser exposing (..)
import Syntax.RawSyntaxP exposing (..)
import Syntax.Syntax exposing (..)


createSubFun : WellFormedSyntax -> Elm.Declaration
createSubFun wellformedSyntax =
    Elm.declaration "substitute" <|
        Elm.withType
            (Type.function
                [ Type.tuple (Type.named [ "Syntax", "CCtx" ] "Cctx") (Type.named [ "Syntax", "Wellformed" ] "Wellformed")
                , Type.named [ "Syntax", "Cursorless" ] "CursorLess"
                ]
                (Type.maybe <| Type.tuple (Type.named [ "Syntax", "CCtx" ] "Cctx") (Type.named [ "Syntax", "Wellformed" ] "Wellformed"))
            )
        <|
            Elm.fn2
                ( "decomposed", Just <| Type.tuple (Type.named [ "Syntax", "CCtx" ] "Cctx") (Type.named [ "Syntax", "Wellformed" ] "Wellformed") )
                ( "sub", Just <| Type.named [ "Syntax", "Cursorless" ] "CursorLess" )
                (\decomposed sub ->
                    Elm.Let.letIn
                        (\( cctx, wellformed ) ->
                            Elm.Case.custom
                                wellformed
                                (Type.named [ "Syntax", "Wellformed" ] "Wellformed")
                                (wellFormedCases wellformedSyntax sub)
                        )
                        |> Elm.Let.tuple "cctx" "wellformed" (Elm.val "decomposed")
                        |> Elm.Let.toExpression
                )


wellFormedCases : Syntax -> Elm.Expression -> List Branch.Branch
wellFormedCases syntax sub =
    List.map
        (\op ->
            Elm.Case.branch1 op.name ( "_", Type.named [ "Syntax", "Wellformed" ] "Wellformed" ) <|
                \_ ->
                    Elm.Case.custom
                        sub
                        (Type.named [ "Syntax", "Cursorless" ] "CursorLess")
                        [ branch1
                            (firstCharToUpper <| String.dropLeft 5 op.name)
                            ( "sub", Type.named [ "Syntax", "Cursorless" ] "CursorLess" )
                          <|
                            \sub_ ->
                                Elm.just <|
                                    Elm.tuple
                                        (Elm.val "cctx")
                                        (Elm.apply
                                            (Elm.value
                                                { importFrom = [ "Syntax", "Wellformed" ]
                                                , name = firstCharToUpper op.name
                                                , annotation = Nothing
                                                }
                                            )
                                            [ sub_ ]
                                        )
                        , ignore <| Elm.nothing
                        ]
        )
        (List.concatMap .ops <| List.filter (\x -> x.synCat == "wellformed") syntax.synCatOps)
