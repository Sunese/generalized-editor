data Tree a = Fork (Tree a) (Tree a) | Leaf a
  deriving (Show) 

t = Fork (Fork  (Leaf 1)
                (Leaf 2))
          (Fork (Leaf 3)
                (Leaf 4))

data Ctx a = Top | L (Ctx a) (Tree a) | R (Tree a) (Ctx a)
    deriving (Show)

type Loc a = (Tree a, Ctx a)

left :: Loc a -> Loc a
left (Fork l r, c) = (l, L c r)

right :: Loc a -> Loc a
right (Fork l r, c) = (r, R l c)

top :: Tree a -> Loc a
top t = (t, Top)

up :: Loc a -> Loc a
up (t, L c r) = (Fork t r, c)
up (t, R l c) = (Fork l t, c)

upmost :: Loc a -> Loc a
upmost l@(t, Top) = l
upmost l = upmost (up l)

modify :: Loc a -> (Tree a -> Tree a) -> Loc a
modify (t, c) f = (f t, c)

