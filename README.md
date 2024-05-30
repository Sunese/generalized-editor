# README

## Dependencies

- [Elm](https://guide.elm-lang.org/install/elm.html)
- [elm-codegen](https://package.elm-lang.org/packages/mdgriffith/elm-codegen/latest/)
- [Node.js](https://nodejs.org/en/download)

## How to run the editor

### Use one of the generated examples

- Navigate to an example src folder, e.g. examples\c\src, and run the Elm REPL:

    ```bash
    > import Main exposing (..)
    > ---> do editing here <---
    ```

See examples in the report for more information.

### Generate one yourself

- Write up a language specification
- Paste it in the `rawSyntax` declaration in src\codegen\Example.elm
- Run `elm-codegen run --debug --output \repo-dir\src\generated\src`
- Navigate to \repo-dir\src\generated\src and run the Elm REPL:

    ```bash
    > import Main exposing (..)
    > ---> do editing here <---
    ```

See examples in the report for more information.
