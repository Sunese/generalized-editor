data Tree = Item Item | Section [Tree]
    deriving (Show)

type Item = String

data Path = Top | Node [Tree] Path [Tree]
    deriving (Show)

data Location = Loc Tree Path
    deriving (Show)

t = Section [Section [Item "a", Item "*", Item "b"],
            Item "+",
            Section [Item "c", Item "*", Item "d"]]

loc = Loc t Top

goLeft :: Location -> Location
goLeft loc = case loc of
    Loc tree path -> case path of
      Node (l:left) up right -> Loc l (Node left up (tree:right))
      Node [] up right -> loc -- FAIL: left of first
      Top -> loc -- FAIL: left of top

goRight :: Location -> Location
goRight loc = case loc of
    Loc tree path -> case path of
      Node left up (r:right) -> Loc r (Node (tree:left) up right)
      Node left up [] -> loc -- FAIL: right of last
      Top -> loc -- FAIL: right of top

goUp :: Location -> Location
goUp loc = case loc of
    Loc tree path -> case path of
      Node left up right -> Loc (Section (left ++ [tree] ++ right)) up
      Top -> loc -- FAIL: up of top

goDown :: Location -> Location
goDown loc = case loc of
    Loc tree path -> case tree of
      Section (t1:trees) -> Loc t1 (Node [] path trees)
      Section [] -> loc -- FAIL: down of last
      Item _ -> loc -- FAIL: down of item

nthSon :: Int -> Location -> Location
nthSon n loc = case n of
    1 -> goDown loc
    n -> if n > 0 then goRight (nthSon (n-1) loc)
         else loc -- FAIL: negative n