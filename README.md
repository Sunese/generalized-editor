# README

## Dependencies

- [Elm](https://guide.elm-lang.org/install/elm.html)
- [elm-codegen](https://package.elm-lang.org/packages/mdgriffith/elm-codegen/latest/)
- [Node.js](https://nodejs.org/en/download)

## How to run the editor

### Use one of the generated examples

- Navigate to an example src folder, e.g. examples\sql\src, and run the Elm REPL:

```bash
> import Syntax.Base exposing (..)
> import Movement exposing (..)
> ---> do editing here <---
```

Following is an example editing session of the generated SQL example:

```bash
> import Movement exposing (..)
> import Syntax.Base exposing (..)
> import Syntax.Cursorless exposing (..)
> import Syntax.CCtx exposing (..)
> import Syntax.Wellformed exposing (..)
> import Syntax.Bind exposing (..)
> import Conditionals exposing (..)
> example = Q (Select (Ident "col-a") (Ident "table-b") (Where (Greater (Eident (Ident "col-a")) (Econst (Num 2)))))
Q (Select (Ident "col-a") (Ident "table-b") (Where (Greater (Eident (Ident "col-a")) (Econst (Num 2)))))
    : Base
> decomposed = decompose example
(Cctx_hole,Root_q_CLess (Select_CLess (Ident_CLess "col-a") (Ident_CLess "table-b") (Where_CLess (Greater_CLess (Eident_CLess (Ident_CLess "col-a")) (Econst_CLess (Num_CLess 2))))))
    : ( Cctx, Wellformed )
> new = parent decomposed
Nothing : Maybe ( Cctx, Wellformed )
> child 1 decomposed
Just (Select_CLess_cctx1 Cctx_hole (Ident_CLess "table-b") (Where_CLess (Greater_CLess (Eident_CLess (Ident_CLess "col-a")) (Econst_CLess (Num_CLess 2)))),Root_id_CLess (Ident_CLess "col-a"))
    : Maybe ( Cctx, Wellformed )
> substitute decomposed (Q_CLess Hole_q_CLess)
Just (Cctx_hole,Root_q_CLess Hole_q_CLess)
    : Maybe ( Cctx, Wellformed )
> evalCond decomposed <| At <| Q_CLess <| Select_CLess Hole_id_CLess Hole_id_CLess Hole_clause_CLess
True : Bool

```

See examples in the report and presentation for more information.

### Generate one yourself

- Write up a language specification
- Paste it in the `rawSyntax` declaration in src\codegen\Example.elm
- Run `elm-codegen run --debug --output \repo-dir\src\generated\src`
- Navigate to \repo-dir\src\generated\src and run the Elm REPL:

    ```bash
    > import Movement exposing (..)
    > import Syntax.Base exposing (..)
    > ---> do editing here <---
    ```

See examples in the report and presentation for more information.
