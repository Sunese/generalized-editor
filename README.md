# 👷⚒️ ... Under construction ... 👷⚒️

## Type-safe generalized syntax-directed editor

This project is part of my master's thesis, where one of my main goals is to
implement a type-safe generalized syntax-directed editor.

Already heard enough? See [how to run it](#how-to-run-the-editor)!

The editor is generalized in the sense that it can perform edit actions on any
language, as long as it's syntax is provided. See [examples](#example-languages-and-their-syntax) of how the syntax should be provided.

The editor guarantees that no syntactical errors can be produced and allows for
incomplete programs (or parts thereof).

The editor has some limitations, including:

- **TODO**

## Dependencies

- [Elm](https://guide.elm-lang.org/install/elm.html)
- [elm-codegen](https://package.elm-lang.org/packages/mdgriffith/elm-codegen/latest/)
- [Node.js](https://nodejs.org/en/download)

## How to run the editor

<!-- TODO: replace these with a bash (or similiar) script
            that do the manual work of invoking the syntax parser
            and running codegen for us  -->
Run the following commands from the project's root:

**Powershell**:

    rm .\generated\src\*; elm-codegen run --output generated/src

**Unix**:

    rm -rf .\generated\src\ && elm-codegen run --output generated/src

## Example languages and their syntax
