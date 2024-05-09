module Generate exposing (main)

{-| -}

import Elm
import Elm.Annotation exposing (..)
import Example exposing (..)
import Gen.CodeGen.Generate as Generate
import Syntax exposing (..)


main : Program {} () ()
main =
    Generate.run <|
        exampleFiles



-- file : Elm.File
-- file =
--     Elm.file [ "HelloWorld" ] <|
--         getTypeDecls
--             exampleAddOperators
