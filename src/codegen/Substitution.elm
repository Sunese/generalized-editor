module Substitution exposing (..)

import Array
import Elm
import Elm.Annotation as Type exposing (..)
import Elm.Case exposing (..)
import Elm.Case.Branch as Branch exposing (ignore)
import Elm.Let
import Gen.Decomposable exposing (..)
import Gen.Substitutable exposing (..)
import Parser exposing (..)
import RawSyntaxP exposing (..)
import Syntax exposing (..)


createSubFun : WellFormedSyntax -> Elm.Declaration
createSubFun wellformedSyntax =
    Elm.declaration "substitute" <|
        Elm.withType
            (Type.function
                [ Type.tuple (Type.named [] "Cctx") (Type.named [] "Wellformed")
                , Type.named [] "CursorLess"
                ]
                (Type.maybe <| Type.tuple (Type.named [] "Cctx") (Type.named [] "Wellformed"))
            )
        <|
            Elm.fn2
                ( "decomposed", Just <| Type.tuple (Type.named [] "Cctx") (Type.named [] "Wellformed") )
                ( "sub", Just <| Type.named [] "CursorLess" )
                (\decomposed sub ->
                    Elm.Let.letIn
                        (\( cctx, wellformed ) ->
                            Elm.Case.custom
                                wellformed
                                (Type.named [] "Wellformed")
                                (wellFormedCases wellformedSyntax sub)
                        )
                        |> Elm.Let.tuple "cctx" "wellformed" (Elm.val "decomposed")
                        |> Elm.Let.toExpression
                )


wellFormedCases : Syntax -> Elm.Expression -> List Branch.Branch
wellFormedCases syntax sub =
    List.map
        (\op ->
            Elm.Case.branch1 op.name ( "_", Type.named [] "Wellformed" ) <|
                \_ ->
                    Elm.Case.custom
                        sub
                        (Type.named [] "CursorLess")
                        [ branch1
                            (firstCharToUpper <| String.dropLeft 5 op.name)
                            ( "sub", Type.named [] "CursorLess" )
                          <|
                            \sub_ ->
                                Elm.just <|
                                    Elm.tuple
                                        (Elm.val "cctx")
                                        (Elm.apply
                                            (Elm.val <| firstCharToUpper op.name)
                                            [ sub_ ]
                                        )
                        , ignore <| Elm.nothing
                        ]
        )
        (List.concatMap .ops <| List.filter (\x -> x.synCat == "wellformed") syntax.synCatOps)
