module Generate exposing (main)

{-| -}

import Elm
import Elm.Annotation as Type
import Elm.Case as Case
import Elm.Case.Branch as Branch
import Gen.CodeGen.Generate as Generate


main : Program {} () ()
main =
    Generate.run
        [ file
        ]


file : Elm.File
file =
    Elm.file [ "HelloWorld" ]
        decs


decs : List Elm.Declaration
decs =
    Case.custom myValue
        (Elm.Annotation.named [] "Msg")
        -- Define all the branches in your case
        [ Branch.variant1 "ButtonClicked" (Branch.var "id") <|
            \id -> Elm.Op.append id (Elm.string " was clicked!")

        -- A branch which also destructures a record
        , Branch.variant1 "FormSubmitted" (Branch.record2 Tuple.pair "id" "isValid") <|
            \( id, isValid ) ->
                Elm.ifThen isValue
                    (Elm.string "Form is valid")
                    (Elm.string "Form is NOT valid")
        ]
