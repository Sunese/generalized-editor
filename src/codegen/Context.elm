module Context exposing (..)

import Substitution exposing (..)


{-| Cursor context, i.e. zipper data structure
for any type 'a'
-}
type alias Traversable a =
    { goUp : Location a -> Maybe (Location a)
    , goNChild : Int -> Location a -> Maybe (Location a)
    }


type AnyTree a
    = Nil
    | TreeNode a (AnyTree a) (AnyTree a)


type Path a
    = Top
    | Node (Maybe a) (Path a) (Maybe a) -- Node: left sibling:a , parent:Path a , right sibling:a


type Location a
    = Loc a (Path a) -- First a: current tree in focus, Path a: LPR structure, i.e. left tree, parent path, right tree



-- example usage
-- Following are auto-generated types for the cursor context
-- example type class instance
-- traversableCctx : Traversable Cctx
-- traversableCctx =
--     { goUp = goUpCctx
--     , goNChild = goNChildCctx
--     }


getCursorHoleLocation : Location Cctx -> Location Cctx
getCursorHoleLocation loc =
    -- expects an empty location that it will recursively fill
    case loc of
        Loc cctx (Node maybeLeft parentPath maybeRight) ->
            case cctx of
                Let_cctx0 cctx_ bind ->
                    case getCursorHoleLocation (Loc cctx_ (Node maybeLeft parentPath Nothing)) of
                        Loc cctx__ path_ ->
                            Loc (Let_cctx0 cctx__ bind) path_

                Let_cctx1 e bind ->
                    case getCursorHoleLocation (Loc e (Node Nothing path Nothing)) of
                        Loc e_ path_ ->
                            Loc (Let_cctx1 e_ bind) path_

                CctxHole ->
                    Loc CctxHole path

        -- We are looking at a hole
        CctxHole ->
            Loc CctxHole Top


type S
    = Let E (Bind E S)
    | Exp E
    | Hole_s


type E
    = Plus E E
    | Num
    | Var
    | Hole_e


type Cctx
    = Let_cctx0 Cctx (Bind E S)
    | Let_cctx1 E (Bind E Cctx)
    | Exp_cctx0 Cctx
    | Plus_cctx0 Cctx E
    | Plus_cctx1 E Cctx
    | CctxHole


type alias Bind a b =
    ( List a, b )
